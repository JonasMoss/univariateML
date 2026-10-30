"""Validate oracle primitives against independent density integrals."""
import unittest
import mpmath as mp
from quality.truth_reference import normal, pair, gamma, Reference, beta_normalization, complement, negative_binomial
from quality.truth import DISTS, module_at, points, read, declarative, verify, number, quantile_acceptance
import json
from types import SimpleNamespace


class ClosedForms(unittest.TestCase):
    def assert_relative(self, actual, expected):
        self.assertLess(abs(actual-expected), mp.mpf('1e-15')*abs(expected))

    def test_five_closed_forms(self):
        with mp.workdps(60):
            for x in (-6, -.3, 4):
                for lower in (True, False):
                    integral = mp.quad(lambda y:mp.exp(-y*y/2)/mp.sqrt(2*mp.pi),
                                       [-mp.inf,x] if lower else [x,mp.inf])
                    self.assert_relative(mp.exp(normal(x)[0 if lower else 1]),integral)
            for x in (mp.mpf('.01'),mp.mpf('1.2'),mp.mpf(8)):
                for logd,tails in (
                    (lambda y:mp.log(2)-2*y,pair(-2*x,False)),
                    (lambda y:mp.log(6)-4*mp.log1p(2*y),pair(-3*mp.log1p(2*x),False))):
                    for lower in (True,False):
                        integral=mp.quad(lambda y:mp.exp(logd(y)),[0,x] if lower else [x,mp.inf])
                        self.assert_relative(mp.exp(tails[0 if lower else 1]),integral)
            for x in (-2,0,4):
                # y=exp(-x) transforms Gumbel into a unit exponential.
                y=mp.exp(-x)
                integral=mp.quad(lambda v:mp.exp(-v),[y,mp.inf])
                self.assert_relative(mp.exp(-y),integral)
            for k in (0,3,15,60):
                tail=mp.fsum(mp.exp(-3)*mp.mpf(3)**j/mp.factorial(j) for j in range(k+1,300))
                self.assert_relative(mp.exp(gamma(k+1,3)[0]),tail)

    def test_normal_extreme_does_not_overflow(self):
        with mp.workdps(60):
            logtail=normal(mp.mpf('1e300'))[1]
            self.assertTrue(mp.isfinite(logtail))
            self.assert_relative(logtail,-mp.mpf('5e599'))

class OracleRegressions(unittest.TestCase):
    def test_exact_zero_median(self):
        with mp.workdps(80):
            reference=Reference(normal)
            self.assertEqual(reference.quantile(-mp.log(2),True),0)
            self.assertEqual(reference.quantile(-mp.log(2),False),0)

    def test_normal_quantile_rounding(self):
        with mp.workdps(80):
            reference=Reference(normal)
            for u in (.01,.25,.5,1e-6):
                p=mp.mpf(u)
                expected=mp.sqrt(2)*mp.erfinv(2*p-1)
                self.assertEqual(float(reference.quantile(mp.log(p),True)),float(expected))
            lp=mp.mpf(float(-mp.log(2)))
            expected=mp.sqrt(2)*mp.erfinv(2*mp.exp(lp)-1)
            self.assertEqual(float(reference.quantile(lp,True)),float(expected))

    def test_small_beta_shapes_normalize(self):
        with mp.workdps(80):
            reference=beta_normalization(Reference(lambda x:None,0,1),mp.mpf('.04'),mp.mpf('.03'))
            mass=mp.quad(reference.normalization_integrand,reference.normalization_domain)
            self.assertLess(abs(mass-1),mp.mpf('1e-25'))

    def test_discrete_quantile_double_limit(self):
        with mp.workdps(80):
            reference=Reference(lambda x:pair(-mp.log(x+1),False),1,mp.inf,True)
            self.assertEqual(reference.quantile(mp.mpf(-10000),False),mp.inf)
            actual=reference.quantile(mp.mpf(-100),False)
            expected=mp.ceil(mp.exp(100)-1)
            self.assertEqual(float(actual),float(expected))
            self.assertEqual(complement(-mp.mpf('1e10000')),0)


class FarCountDensities(unittest.TestCase):
    def test_yule_rho_one(self):
        with mp.workdps(80):
            route=module_at(DISTS/'yulesimon/truth.py')
            ref=route.build({'rho':mp.mpf(1)},lambda x:None)
            for x in (mp.mpf(3),mp.mpf('1e100'),mp.mpf('1e300')):
                expected=-mp.log(x)-mp.log(x+1)
                self.assertLess(abs(ref.log_density(x)-expected),mp.mpf('1e-15')*abs(expected))

    def test_discretized_exponential_and_weibull(self):
        with mp.workdps(80):
            gamma_route=module_at(DISTS/'dgamma/truth.py')
            weibull_route=module_at(DISTS/'dweibull/truth.py')
            g=gamma_route.build({'shape':mp.mpf(1),'rate':mp.mpf(2)},lambda x:None)
            w=weibull_route.build({'shape1':mp.mpf('.7'),'shape2':mp.mpf(1)},lambda x:None)
            for x in (mp.mpf(3),mp.mpf('1e100'),mp.mpf('1e300')):
                self.assertLess(abs(g.log_density(x)-(-2*x+mp.log(-mp.expm1(-2)))),mp.mpf('1e-15')*x)
                self.assertLess(abs(w.log_density(x)-(x*mp.log(mp.mpf('.7'))+mp.log(mp.mpf('.3')))),mp.mpf('1e-15')*x)

    def test_negative_binomial_closed_tails(self):
        with mp.workdps(80):
            p=mp.mpf('.04')
            for x in (mp.mpf(3000),mp.mpf('1e100'),mp.mpf('1e300')):
                for r in (1,2):
                    expected=(x+1)*mp.log1p(-p)+(mp.log1p((x+1)*p) if r==2 else 0)
                    actual=negative_binomial(mp.mpf(r),p,x)[1]
                    self.assertLess(abs(actual-expected),mp.mpf('1e-15')*abs(expected))


class TailCompositionRegressions(unittest.TestCase):
    def test_half_t_one_matches_half_cauchy(self):
        with mp.workdps(80):
            hc=module_at(DISTS/'hcauchy/truth.py').build({'mu':mp.mpf(0),'sigma':mp.mpf(2)})
            ht=module_at(DISTS/'ht/truth.py').build({'mu':mp.mpf(0),'sigma':mp.mpf(2),'nu':mp.mpf(1)})
            for text in ('1e-300','1e-100','.01','2','1e100'):
                x=mp.mpf(text)
                for tail in (0,1):
                    expected=mp.exp(hc.tails(x)[tail]);actual=mp.exp(ht.tails(x)[tail])
                    self.assertLess(abs(actual-expected),mp.mpf('1e-15')*expected)
            u=mp.mpf('1e-300');expected=2*mp.tan(mp.pi*u/2)
            self.assertLess(abs(hc.quantile(mp.log(u),True)/expected-1),mp.mpf('1e-15'))

    def test_half_normal_tiny_quantile(self):
        with mp.workdps(80):
            ref=module_at(DISTS/'hnorm/truth.py').build({'mu':mp.mpf(0),'sigma':mp.mpf(2)})
            u=mp.mpf('1e-300');expected=2*mp.sqrt(2)*mp.erfinv(u)
            self.assertLess(abs(ref.quantile(mp.log(u),True)/expected-1),mp.mpf('1e-15'))

    def test_folded_normal_far_from_zero(self):
        with mp.workdps(80):
            m,x=mp.mpf(20),mp.mpf('1e-13')
            ref=module_at(DISTS/'foldnorm/truth.py').build({'mu':m,'sigma':mp.mpf(1)})
            expected=x*mp.exp(-m*m/2)/mp.sqrt(2*mp.pi)*mp.quad(
                lambda v:mp.exp(m*x*v-(x*v)**2/2)+mp.exp(-m*x*v-(x*v)**2/2),[0,1])
            self.assertLess(abs(mp.exp(ref.tails(x)[0])/expected-1),mp.mpf('1e-15'))

    def test_shifted_gompertz_exponential_limit(self):
        with mp.workdps(80):
            ref=module_at(DISTS/'sgomp/truth.py').build({'b':mp.mpf(2),'eta':mp.mpf(0)})
            for text in ('1e-300','.1','10000'):
                x=mp.mpf(text);actual=ref.tails(x)
                expected=(complement(-2*x),-2*x)
                for a,e in zip(actual,expected):
                    self.assertLessEqual(abs(a-e),mp.mpf('1e-15')*max(1,abs(e)))


class SamplerPoints(unittest.TestCase):
    def test_default_is_independent_of_golden_sampler_and_held_coordinates(self):
        meta = {'params': [
            {'name': 'mu', 'role': 'location', 'domain': '(-inf, inf)',
             'kind': 'threshold', 'simulate': 0},
            {'name': 'scale', 'role': 'scale', 'domain': '(0, inf)', 'simulate': 1}]}
        golden = {'estimates': {'mu': 3, 'scale': 2},
                  'sampler_args': {'mu': 4, 'scale': 5}}
        pts = points(meta, golden, SimpleNamespace())
        self.assertEqual(pts[0], {'mu': 3., 'scale': 5.})
        self.assertIn({'mu': 3., 'scale': 2.}, pts)
        self.assertIn({'mu': 0., 'scale': 1.}, pts)

    def test_default_is_retained_when_stress_grid_reaches_cap(self):
        meta = {'params': [
            {'name': name, 'role': 'shape', 'domain': '(0, inf)', 'simulate': 1}
            for name in ('a', 'b', 'c', 'd')]}
        golden = {'estimates': dict.fromkeys(('a', 'b', 'c', 'd'), 2),
                  'sampler_args': dict.fromkeys(('a', 'b', 'c', 'd'), 3)}
        pts = points(meta, golden, SimpleNamespace())
        self.assertEqual(len(pts), 9)
        self.assertEqual(pts[-1], dict.fromkeys(('a', 'b', 'c', 'd'), 1.))

    def test_all_tables_select_public_default_and_frozen_golden_fit(self):
        folders = sorted(DISTS.glob('*/README.md'))
        self.assertEqual(len(folders), 98)
        for filename in folders:
            with self.subTest(family=filename.parent.name):
                meta, golden = read(filename.parent)
                table = json.loads(filename.with_name('truth.json').read_text())
                self.assertEqual(table['schema'], 3)
                selected = table['sampler_points']
                self.assertEqual(len(selected), 2)
                expected = [
                    [float(p['simulate']) for p in meta['params']],
                    [float(golden['estimates'][p['name']]) for p in meta['params']]]
                for row, theta in zip(selected, expected):
                    self.assertEqual(len(row), 1)
                    actual = [float(v) for v in table['parameters'][row[0]]]
                    self.assertEqual(actual, theta)


class OracleValidation(unittest.TestCase):
    def test_small_shape_paralogistic_derivatives_at_extreme_body_quantiles(self):
        with mp.workdps(80):
            for id in ('paralogis', 'invparalogis'):
                with self.subTest(family=id):
                    folder = DISTS/id
                    meta, _ = read(folder)
                    theta = {'shape': .1, 'scale': 1.}
                    route = module_at(folder/'truth.py')
                    logd = declarative(folder, meta, theta, route)
                    ref = route.build({k: number(v) for k,v in theta.items()})
                    self.assertLess(abs(verify(ref, logd)-1), mp.mpf('1e-25'))

    def test_kumar_singular_endpoint_normalization(self):
        with mp.workdps(80):
            folder = DISTS/'kumar'
            meta, _ = read(folder)
            theta = {'a': 2., 'b': .3}
            route = module_at(folder/'truth.py')
            logd = declarative(folder, meta, theta, route)
            ref = route.build({k: number(v) for k,v in theta.items()})
            self.assertLess(abs(verify(ref, logd)-1), mp.mpf('1e-25'))


class ExactJumps(unittest.TestCase):
    def test_discrete_oracle_preserves_exact_and_binary64_log_half(self):
        from quality.truth_reference import finite_pmf
        with mp.workdps(80):
            ref = finite_pmf(lambda k: -mp.log(2), 0, 1)
            self.assertEqual(ref.quantile(mp.log(mp.mpf('.5')), True), 0)
            self.assertEqual(ref.quantile(mp.log(mp.mpf('.5')), False), 0)
            self.assertEqual(ref.quantile(mp.mpf(-0.6931471805599453), True), 1)
            self.assertEqual(ref.quantile(mp.mpf(-0.6931471805599453), False), 0)

    def test_normal_skew_normal_oracle_has_exact_median(self):
        with mp.workdps(80):
            folder = DISTS/'sn'
            meta, _ = read(folder)
            theta = {'xi': 0., 'omega': 1., 'alpha': 0.}
            route = module_at(folder/'truth.py')
            ref = route.build({k:number(v) for k,v in theta.items()}, declarative(folder,meta,theta,route))
            for lower in (True, False):
                self.assertEqual(ref.quantile(mp.log(mp.mpf('.5')),lower),0)


class QuantileAcceptance(unittest.TestCase):
    def test_jump_ambiguity_uses_the_supplied_probability_scale(self):
        from quality.truth_reference import finite_pmf
        with mp.workdps(80):
            ref = finite_pmf(lambda k: -mp.log(2), 0, 1)
            for lower in (True, False):
                for logp, inp in ((False,.5),(True,-0.6931471805599453)):
                    lp = number(inp) if logp else mp.log(number(inp))
                    q = ref.quantile(lp,lower)
                    a,b,flag,floor = quantile_acceptance(ref,None,q,inp,lower,logp)
                    self.assertEqual((a,b,flag,floor),(0.,1.,1,0.))
            # Away from the jump no neighbouring integer is admitted.
            self.assertEqual(quantile_acceptance(ref,None,mp.mpf(0),.4,True,False)[2],0)
            # Natural distance is tiny, but relative log distance is not: never
            # use the natural scale when the supplied input is a log probability.
            tiny = Reference(lambda x: (-mp.mpf('2e-15'),mp.log(mp.mpf('2e-15'))),0,1,True)
            self.assertEqual(quantile_acceptance(tiny,None,mp.mpf(0),-1e-15,True,True)[2],0)
            self.assertEqual(quantile_acceptance(tiny,None,mp.mpf(0),float(mp.exp(-mp.mpf('1e-15'))),True,False)[2],1)

    def test_zero_floor_scales_with_the_model(self):
        with mp.workdps(80):
            for scale in (mp.mpf('.01'),mp.mpf(10)):
                ref=Reference(lambda x:normal(x/scale))
                logd=lambda x:-mp.log(scale)-mp.log(2*mp.pi)/2-x*x/(2*scale*scale)
                a,b,flag,floor=quantile_acceptance(ref,logd,mp.mpf(0),.5,True,False)
                self.assertEqual((a,b,flag),(0.,0.,0))
                self.assertAlmostEqual(floor/float(scale),float(mp.mpf('1e-12')*mp.sqrt(2*mp.pi)),places=24)
                self.assertEqual(quantile_acceptance(ref,logd,mp.mpf(1),.8,True,False)[3],0.)


if __name__=='__main__':
    unittest.main()
