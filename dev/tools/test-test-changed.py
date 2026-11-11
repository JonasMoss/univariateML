#!/usr/bin/env python3
"""Small selector checks; no build, git writes or source mutations."""
import importlib.util
from pathlib import Path
import unittest
import contextlib
import io
import os
import sys
from unittest.mock import patch

spec = importlib.util.spec_from_file_location('changed', Path(__file__).with_name('test-changed.py'))
changed = importlib.util.module_from_spec(spec)
spec.loader.exec_module(changed)


class Selection(unittest.TestCase):
    def test_git_layers_are_unioned_even_when_later_edits_cancel(self):
        # A net base-to-worktree diff would miss cancelled committed/staged paths.
        with patch.object(changed, 'git', side_effect=[
                'base-sha\n', 'committed\0cancelled\0', 'staged\0cancelled\0',
                'unstaged\0', 'untracked\0']):
            base, paths = changed.changed_paths('master')
        self.assertEqual(base, 'base-sha')
        self.assertEqual(paths, ['cancelled', 'committed', 'staged', 'unstaged', 'untracked'])

    def test_shared_plan_cannot_inherit_narrowing_filters(self):
        with patch.object(changed, 'changed_paths', return_value=('base', ['Justfile'])), \
             patch.object(changed, 'select', return_value=(['bhatt'], {'bhatt':['shared']}, ['Justfile'])), \
             patch.object(changed.subprocess, 'call', return_value=0) as run, \
             patch.object(sys, 'argv', ['test-changed.py']), \
             patch.dict(os.environ, {'UMVL_TEST_FAMILIES':'bhatt', 'FIT_FAMILY':'bhatt', 'JS_FAMILIES':'bhatt'}), \
             contextlib.redirect_stdout(io.StringIO()):
            self.assertEqual(changed.main(), 0)
        for call in run.call_args_list:
            for key in ('UMVL_TEST_FAMILIES', 'FIT_FAMILY', 'JS_FAMILIES'):
                self.assertNotIn(key, call.kwargs['env'])

    def test_leaf_and_binding(self):
        self.assertEqual(changed.select([str(changed.DOSSIERS / "bhatt/test.cpp")])[0], ["bhatt"])
        for path in [str(changed.DOSSIERS / 'exp/test.cpp'),
                     'bindings/r/distributions/exp/test.R',
                     'bindings/common/distributions/exp/fit.hpp']:
            selected, reasons, shared = changed.select([path])
            self.assertIn('exp', selected)
            self.assertFalse(shared)
            self.assertIn(path, reasons['exp'])

    def test_delegate(self):
        selected, reasons, shared = changed.select([str(changed.DOSSIERS / 'invweibull/mle.hpp')])
        self.assertIn('frechet', selected)
        self.assertTrue(any('includes' in reason for reason in reasons['frechet']))
        self.assertFalse(shared)

    def test_shared_and_docs(self):
        for path in ['univariateML-cpp/include/cppunivariate/core/span.hpp',
                     'bindings/r/scripts/test_local.R', 'bindings/js/test.mjs',
                     'univariateML-cpp/tests/test_support.hpp', 'Justfile']:
            selected, _, shared = changed.select([path])
            self.assertEqual(len(selected), 99)
            self.assertEqual(shared, [path])
        for path in ['dev/research/example.md', 'bindings/r/man/example.Rd',
                     'univariateML-cpp/README.md', 'bindings/r/man/figures/logo.svg']:
            self.assertEqual(changed.select([path])[0], [])


if __name__ == '__main__':
    unittest.main()
