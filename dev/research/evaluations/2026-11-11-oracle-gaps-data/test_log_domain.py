"""Regression checks for references with astronomically small physical masses."""
import unittest
import mpmath as mp
from quality.truth import DISTS,module_at,verify,quantile_acceptance

class LogDomain(unittest.TestCase):
    def test_inverse_burr_endpoint_power(self):
        from quality.truth_reference import burr_reference
        with mp.workdps(160):
            for a,b,want in [(mp.mpf('1e-8'),1,mp.inf),(2,1,-mp.inf),(2,mp.mpf('.5'),mp.log(mp.mpf('.25')))]:
                ref=burr_reference(a,b,mp.mpf(4),inverse=True)
                self.assertEqual(ref.log_density(mp.mpf(0)),want)

    def test_positive_conditioning_keeps_tiny_lower_tail(self):
        with mp.workdps(160):
            for name,theta,expected in [
                ('ztpois',dict(**{'lambda':mp.mpf('1e8')}),-mp.mpf('1e8')+mp.log(mp.mpf('1e8'))),
                ('ztnbinom',dict(size=mp.mpf('1e8'),prob=mp.mpf('.5')),mp.mpf('1e8')*mp.log(mp.mpf('.5'))+mp.log(mp.mpf('5e7')))]:
                ref=module_at(DISTS/name/'truth.py').build(theta,None)
                self.assertLess(abs(ref.tails(mp.mpf(1))[0]-expected),mp.mpf('1e-100'))

    def test_far_truncation_retains_a_subnormal_interval(self):
        with mp.workdps(160):
            ref=module_at(DISTS/'tnorm/truth.py').build(dict(mean=-mp.mpf('1e16'),sd=mp.mpf('1e8'),a=mp.mpf(0),b=mp.inf))
            x=mp.mpf(float.fromhex('0x0.0000000000001p-1022'))
            self.assertLess(abs(ref.tails(x)[0]-mp.log(x)-ref.log_density(mp.mpf(0))),mp.mpf('1e-100'))

    def test_skellam_far_negative_count_bound(self):
        with mp.workdps(160):
            route=module_at(DISTS/'skellam/truth.py')
            ref=route.asymmetric_reference(mp.mpf(40),mp.mpf('.3'))
            for k in (mp.mpf('-1e100'),mp.mpf('-1e300')):
                ll,lu=ref.tails(k)
                self.assertTrue(mp.isfinite(ll))
                self.assertEqual(lu,0)
                self.assertLessEqual(abs(ll-ref.log_density(k)),mp.mpf('1e-50')+mp.eps*abs(ll))

    def test_far_count_bracket_keeps_the_last_lower_endpoint(self):
        from quality.truth_reference import Reference,pair
        with mp.workdps(160):
            rate=mp.mpf('1e-100');calls=[]
            def tails(k):
                calls.append(k)
                return pair(-(k+1)*rate,False)
            ref=Reference(tails,0,mp.inf,True)
            q=ref.quantile(mp.mpf(-10000),False)
            self.assertEqual(float(q),float(mp.mpf(10000)/rate-1))
            self.assertLess(len(calls),120)

    def test_logarithmic_stieltjes_bound_against_hypergeometric(self):
        from quality.truth_reference import logarithmic_stieltjes_tail
        with mp.workdps(160):
            for p,k in [(mp.mpf('.9'),200),(mp.mpf('.51'),300),
                        (mp.mpf('0.99999999'),10000)]:
                actual=logarithmic_stieltjes_tail(p,mp.mpf(k))
                expected=k*mp.log(p)-mp.log(k)+mp.log(mp.hyp2f1(1,k,k+1,p))-mp.log(-mp.log1p(-p))
                self.assertLess(abs(actual-expected),mp.mpf('1e-45'))

    def test_gamma_small_z_integral_bound(self):
        from quality.truth_reference import gamma
        with mp.workdps(160):
            z=mp.mpf('1e-60')
            for a in (mp.mpf('.1'),mp.mpf('1e-8'),mp.mpf(2)):
                observed=gamma(a,z)[0]
                expected=mp.log(mp.gammainc(a,0,z)/mp.gamma(a))
                self.assertLessEqual(observed-expected,2*z)
                self.assertGreaterEqual(observed-expected,-mp.mpf('1e-150'))

    def test_natural_probability_jump_check_avoids_huge_exponent(self):
        with mp.workdps(160):
            route=module_at(DISTS/'dweibull/truth.py')
            ref=route.build(dict(shape1=mp.mpf('.5'),shape2=mp.mpf('1e8')),None)
            actual=quantile_acceptance(ref,ref.log_density,mp.mpf(1),1e-300,False,False)
            self.assertEqual(actual,(1.,1.,0,0.))

    def test_concentrated_discrete_weibull_mass_check(self):
        with mp.workdps(160):
            route=module_at(DISTS/'dweibull/truth.py')
            ref=route.build(dict(shape1=mp.mpf('.5'),shape2=mp.mpf('1e8')),None)
            self.assertEqual(verify(ref,ref.log_density),1)
            self.assertEqual(ref.quantile(mp.log(mp.mpf('.9')),True),1)
            self.assertEqual(ref.quantile(mp.log(mp.mpf('.1')),True),0)

    def test_ged_large_shape_bracket_fallback(self):
        with mp.workdps(160):
            route=module_at(DISTS/'ged/truth.py')
            ref=route.build(dict(mean=mp.mpf(0),sd=mp.mpf(1),nu=mp.mpf('2e8')))
            ref.high_precision_quantile=True
            for u in (mp.mpf('.1'),mp.mpf('.9'),mp.mpf('1e-300')):
                lp=mp.log(u);q=ref.quantile(lp,True)
                self.assertLess(abs(mp.exp(ref.tails(q)[0])/u-1),mp.mpf('1e-25'))

if __name__=='__main__':unittest.main()
