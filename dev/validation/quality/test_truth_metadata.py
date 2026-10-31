"""Fingerprint dependency coverage and metadata-only migration invariants."""
import json
import shutil
import tempfile
import unittest
from pathlib import Path
from quality.truth import DISTS, input_sha256
from quality.truth_metadata import numerical_bytes


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
        tables = list(DISTS.glob('*/truth.json'))
        self.assertEqual(len(tables), 98)
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
