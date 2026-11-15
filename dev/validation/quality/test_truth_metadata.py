"""Fingerprint dependency coverage and metadata-only migration invariants."""
import json
import os
import shutil
import tempfile
import unittest
from pathlib import Path
from quality.truth import DISTS, GENERATOR_VERSION, input_sha256, make, mp, DIGITS
from quality.truth_metadata import numerical_bytes, payload_close


class TruthMetadataTests(unittest.TestCase):
    def test_semantic_inputs(self):
        with tempfile.TemporaryDirectory() as tmp:
            folder = Path(tmp)/'exp'
            shutil.copytree(DISTS/'exp', folder)
            readme = folder/'README.md'
            original = readme.read_text()
            baseline = input_sha256(folder)
            readme.write_text(original+'\nUnrelated prose.\n')
            self.assertEqual(input_sha256(folder), baseline)
            readme.write_text(original.replace('summary:', 'editorial_summary:'))
            self.assertEqual(input_sha256(folder), baseline)
            for old, new in [('log(rate) - rate*x', 'log(rate) - 2*rate*x'),
                             ('domain: (0, inf)', 'domain: (0, 10)'),
                             ('simulate: 1.0', 'simulate: 2.0'),
                             ('lower: 0.0', 'lower: 1.0')]:
                readme.write_text(original.replace(old, new))
                self.assertNotEqual(input_sha256(folder), baseline)
            readme.write_text(original)
            route = folder/'truth.py'
            route.write_text(route.read_text()+'\n# route revision\n')
            self.assertNotEqual(input_sha256(folder), baseline)
            shutil.copyfile(DISTS/'exp/truth.py', route)
            golden = folder/'golden.json'
            g = json.loads(golden.read_text())
            g['estimates']['rate'] *= 2
            golden.write_text(json.dumps(g))
            self.assertNotEqual(input_sha256(folder), baseline)

    def test_stored_fingerprints_and_no_timings(self):
        # The expected set is the set of family folders: every dossier has a table.
        folders = {p.parent.name for p in DISTS.glob('*/README.md')}
        tables = list(DISTS.glob('*/truth.json'))
        self.assertTrue(folders)
        self.assertEqual({p.parent.name for p in tables}, folders)
        for path in tables:
            table = json.loads(path.read_text())
            self.assertEqual(table['input_sha256'], input_sha256(path.parent), path)
            self.assertNotIn('density_sha256', table)
            self.assertTrue(all('seconds' not in r for r in table['normalization']))

    def test_payload_filter_preserves_numbers(self):
        a = '  "density_sha256": "abc",\n      "mass": "1.0",\n      "seconds": 0.12\n'
        b = '  "input_sha256": "def",\n      "mass": "1.0"\n'
        self.assertEqual(numerical_bytes(a), numerical_bytes(b))
        self.assertNotEqual(numerical_bytes(a), numerical_bytes(b.replace('1.0', '1.1')))

    def test_shared_generator_sources_are_not_hashed_but_version_is(self):
        # The policy: a table's fingerprint covers its own inputs plus the
        # hand-bumped GENERATOR_VERSION, not the shared generator sources.
        import quality.truth as truth
        baseline = input_sha256(DISTS/'exp')
        truth.GENERATOR_VERSION += 1
        try:
            self.assertNotEqual(input_sha256(DISTS/'exp'), baseline)
        finally:
            truth.GENERATOR_VERSION -= 1
        self.assertEqual(input_sha256(DISTS/'exp'), baseline)


# Cheap spot regeneration: catches a shared-generator edit that changes numbers
# without bumping GENERATOR_VERSION. Runs in the full tier (`just truth-check`,
# part of `just test-full`); set UMVL_TRUTH_REGEN=1 to run it directly.
#
# The stored tables are NOT byte-reproducible today (see the 2026-11-14
# truth-meta report: golden estimates and generator revisions drifted after
# they were written), so the default comparison is numeric (payload_close).
# After a deliberate full regeneration, UMVL_TRUTH_REGEN=bytes demands
# byte-for-byte equality and is the intended steady state.
SPOT_TABLES = ('exp', 'unif')


@unittest.skipUnless(os.environ.get('UMVL_TRUTH_REGEN'), 'full tier: UMVL_TRUTH_REGEN=1')
class RegenerationTests(unittest.TestCase):
    def test_regeneration_reproduces_stored_payload(self):
        for id in SPOT_TABLES:
            with self.subTest(table=id), mp.workdps(DIGITS):
                fresh = make(DISTS/id)
                path = DISTS/id/'truth.json'
                self.assertEqual(fresh['input_sha256'], json.loads(path.read_text())['input_sha256'])
                if os.environ['UMVL_TRUTH_REGEN'] == 'bytes':
                    self.assertEqual(json.dumps(fresh, indent=2, allow_nan=False)+'\n', path.read_text())
                else:
                    diffs = payload_close(json.loads(json.dumps(fresh)), json.loads(path.read_text()))
                    self.assertEqual(diffs[:5], [], f'{len(diffs)} numeric differences')
