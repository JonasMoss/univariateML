"""Synthetic examples distinguish fit failures from reference mismatches."""
import json
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch
import yaml
from quality.harness import check


class GoldenChecks(unittest.TestCase):
    def run_fixture(self, params, density, estimates, ll, data):
        with tempfile.TemporaryDirectory() as tmp:
            folder = Path(tmp) / 'fixture'
            folder.mkdir()
            meta = dict(id='fixture', params=params, log_density=density,
                        support=dict(type='real', lower=0, upper='inf'))
            (folder/'README.md').write_text('---\n'+yaml.safe_dump(meta)+'---\n')
            (folder/'golden.json').write_text(json.dumps(dict(
                estimates=estimates, logLik=ll, data=data)))
            with patch.object(check, 'DISTS', Path(tmp)):
                return check.check('fixture')

    def test_wrong_estimate_is_detected_even_with_matching_loglik(self):
        r = self.run_fixture(
            [dict(name='rate', role='rate', domain='(0, inf)', default=1)],
            'log(rate)-rate*x', dict(rate=1), -4, [1, 3])
        self.assertEqual(r['logLik']['status'], 'pass')
        self.assertEqual(r['score_hessian']['status'], 'fail')
        self.assertEqual(r['oracle']['status'], 'fail')
        self.assertGreater(r['oracle']['gap'], .5)

    def test_closed_lower_endpoint_uses_outward_score(self):
        r = self.run_fixture(
            [dict(name='scale', role='scale', domain='[1, inf)', default=2)],
            '-log(scale)-x/scale', dict(scale=1), -.2, [.2])
        self.assertEqual(r['score_hessian']['status'], 'pass')
        self.assertEqual(r['score_hessian']['one_sided'], {'scale':'lower'})

    def test_incorrect_reported_loglik_is_detected(self):
        r = self.run_fixture(
            [dict(name='rate', role='rate', domain='(0, inf)', default=1)],
            'log(rate)-rate*x', dict(rate=1), -1, [1, 1])
        self.assertEqual(r['logLik']['status'], 'fail')
        self.assertEqual(r['score_hessian']['status'], 'pass')

    def test_inward_score_at_closed_lower_endpoint_fails(self):
        r = self.run_fixture(
            [dict(name='scale', role='scale', domain='[1, inf)', default=2)],
            '-log(scale)-x/scale', dict(scale=1), -2, [2])
        self.assertEqual(r['score_hessian']['status'], 'fail')
        self.assertEqual(r['oracle']['status'], 'fail')

    def test_support_endpoints_are_held_and_reported(self):
        r = self.run_fixture(
            [dict(name='min', role='lower', domain='(-inf, max)', default=0, bound='lower'),
             dict(name='max', role='upper', domain='(min, inf)', default=1, bound='upper')],
            '-log(max-min)', dict(min=0, max=1), 0, [0, 1])
        self.assertEqual(r['logLik']['status'], 'pass')
        self.assertEqual(r['score_hessian']['status'], 'skipped')
        self.assertEqual(r['oracle']['status'], 'skipped')
        self.assertEqual(set(r['skipped_parameters']), {'min', 'max'})


if __name__ == '__main__':
    unittest.main()
