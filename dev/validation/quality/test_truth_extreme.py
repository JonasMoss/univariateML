"""The extreme grid extends rather than replaces the historical coverage."""
import unittest
from types import SimpleNamespace
from quality.truth import DISTS, read, module_at, points, historical_points, extreme_points, domains

class ExtremeGrid(unittest.TestCase):
    def test_historical_prefix_and_domain_projection(self):
        for path in sorted(DISTS.glob('*/truth.py')):
            meta, golden = read(path.parent)
            route = module_at(path)
            before = historical_points(meta, golden, route)
            after = points(meta, golden, route)
            self.assertEqual(after[:len(before)], before, path.parent.name)
            for point in extreme_points(meta, route):
                for p, (lo, hi) in zip(meta['params'], domains(meta, point)):
                    value = point[p['name']]
                    self.assertTrue(lo <= value <= hi, (path, p, point))
                    self.assertFalse(value == lo and p['domain'][0] == '(')
                    self.assertFalse(value == hi and p['domain'][-1] == ')')

    def test_late_coordinates_are_not_capped(self):
        meta = {'params': [{'name': n, 'role': 'shape', 'domain': '(0, inf)', 'simulate': 1} for n in 'abcdef']}
        stress = extreme_points(meta, SimpleNamespace())
        self.assertEqual(len(stress), 12)
        self.assertIn(dict(a=1.,b=1.,c=1.,d=1.,e=1.,f=1e8), stress)

    def test_quantile_cut_beyond_binary64_does_not_stop_at_chart_cap(self):
        import mpmath as mp
        from quality.truth_reference import Reference, pair
        with mp.workdps(80):
            k = mp.mpf('1e-8')
            ref = Reference(lambda x: pair(-k*mp.log1p(x),False),0,mp.inf)
            x = ref.quantile(mp.mpf(-1),False)
            self.assertLess(abs(k*mp.log1p(x)-1),mp.mpf('1e-35'))

    def test_complements_keep_representable_log_tails(self):
        import mpmath as mp
        from quality.truth_reference import complement, beta
        with mp.workdps(80):
            self.assertLess(abs(complement(mp.mpf(-230))/(-mp.exp(-230))-1),mp.mpf('1e-70'))
            a=mp.mpf('1e-8');x=mp.mpf(float.fromhex('0x0.0000000000001p-1022'))
            self.assertLess(abs(beta(a,mp.mpf(1),x)[0]-a*mp.log(x)),mp.mpf('1e-70'))
            w=mp.mpf('1e-100')
            self.assertLess(abs(beta(mp.mpf(1),a,mp.mpf(1),w)[1]-a*mp.log(w)),mp.mpf('1e-70'))

    def test_endpoint_limit_keeps_exact_binary64_product(self):
        import mpmath as mp
        from quality.truth import declarative
        cases = [
            ('genbeta', dict(shape1=1e8,shape2=2.,shape3=1e-8,scale=1.)),
            ('genbeta', dict(shape1=1e-8,shape2=2.,shape3=1e8,scale=1.)),
            ('gb2', dict(shape1=2.,shape2=1e8,shape3=1e-8,scale=1.)),
            ('fpareto', dict(shape1=2.,shape2=1e8,shape3=1e-8,scale=1.,min=0.)),
        ]
        with mp.workdps(100):
            for ident,theta in cases:
                folder=DISTS/ident;meta,_=read(folder)
                ld=declarative(folder,meta,theta,module_at(folder/'truth.py'))
                self.assertEqual(ld(mp.mpf(0)),-mp.inf,(ident,theta))
