"""Independent mathematical checks of the extreme reference reformulations."""
import unittest
import mpmath as mp
from quality.truth_reference import beta_fraction, burr_reference, gamma_power_reference, concentrated_gamma


class OracleGaps(unittest.TestCase):
    def test_beta_fraction_against_hypergeometric(self):
        with mp.workdps(160):
            for a,b,z in [(2,3,'.2'),(1000,2000,'.3'),('.5',500000000,'1e-9')]:
                a,b,z=mp.mpf(a),mp.mpf(b),mp.mpf(z)
                value=z**a*(1-z)**b*beta_fraction(a,b,z)/a/mp.beta(a,b)
                expected=mp.betainc(a,b,0,z,regularized=True)
                self.assertLess(abs(value/expected-1),mp.mpf('1e-90'))

    def test_burr_change_of_variable(self):
        with mp.workdps(160):
            for inverse in (False,True):
                for a,b in [(mp.mpf('1e-8'),mp.mpf(1)),(mp.mpf(1),mp.mpf('1e8'))]:
                    r=burr_reference(a,b,mp.mpf(1000),inverse=inverse)
                    for y in (mp.mpf('.01'),mp.mpf(1),mp.mpf(20)):
                        x,jac=r.chart(mp.log(y))
                        self.assertLess(abs(mp.exp(r.log_density(x)+jac)/ (y*mp.exp(-y))-1),mp.mpf('1e-100'))
                        lp=r.tails(x)[0 if inverse else 1]
                        self.assertLess(abs(lp+y),mp.mpf('1e-100'))

    def test_centered_gamma_against_special_function(self):
        with mp.workdps(160):
            a=mp.mpf(100)
            for z in (mp.mpf(80),a,mp.mpf(120)):
                ll,lu=concentrated_gamma(a,z)
                expected=mp.gammainc(a,0,z)/mp.gamma(a)
                self.assertLess(abs(mp.exp(ll)-expected),mp.mpf('1e-60'))
                self.assertLess(abs(mp.exp(ll)+mp.exp(lu)-1),mp.mpf('1e-100'))

    def test_reciprocal_gamma_quantile_chart(self):
        with mp.workdps(160):
            r=gamma_power_reference(mp.mpf(2),mp.mpf(3),mp.mpf(4),True)
            self.assertLess(r.chart(-1)[0],r.chart(1)[0])
            q=r.quantile(mp.log(mp.mpf('.25')),True)
            self.assertLess(abs(mp.exp(r.tails(q)[0])-mp.mpf('.25')),mp.mpf('1e-14'))

    def test_explicit_ged_density_against_declaration(self):
        from quality.truth import DISTS,read,module_at,declarative
        with mp.workdps(160):
            for name,theta in [('ged',dict(mean=0.,sd=1.,nu=2.)),
                               ('ged',dict(mean=0.,sd=1.,nu=1.5)),
                               ('sged',dict(mean=0.,sd=1.,nu=4.,xi=2.))]:
                folder=DISTS/name
                meta,_=read(folder);route=module_at(folder/'truth.py')
                declared=declarative(folder,meta,theta,route)
                ref=route.build({k:mp.mpf(v) for k,v in theta.items()})
                for x in (-1,0,1):
                    self.assertLess(abs(ref.log_density(mp.mpf(x))-declared(mp.mpf(x))),mp.mpf('1e-90'))
                self.assertLess(abs(ref.normalization_identity-1),mp.mpf('1e-90'))

    def test_skew_t_wedge_against_density_integral(self):
        from quality.truth import DISTS,module_at
        with mp.workdps(160):
            nu=mp.mpf(5);alpha=mp.mpf(3)
            def density(x):
                h=alpha*x*mp.sqrt((nu+1)/(nu+x*x))
                c=mp.betainc((nu+1)/2,mp.mpf('.5'),0,(nu+1)/(nu+1+h*h),regularized=True)/2
                c=c if h<0 else 1-c
                return mp.log(2*c)+mp.loggamma((nu+1)/2)-mp.loggamma(nu/2)-mp.log(nu*mp.pi)/2-(nu+1)*mp.log1p(x*x/nu)/2
            route=module_at(DISTS/'st/truth.py')
            ref=route.build(dict(xi=mp.mpf(0),omega=mp.mpf(1),alpha=alpha,nu=nu),density)
            expected=mp.quad(lambda x:mp.exp(density(x)),[-mp.inf,-6,-1])
            self.assertLess(abs(mp.exp(ref.tails(-1)[0])-expected),mp.mpf('1e-35'))
            self.assertLess(abs(mp.exp(ref.tails(0)[0])-mp.atan(1/alpha)/mp.pi),mp.mpf('1e-100'))

    def test_integer_gamma_positive_polynomial(self):
        from quality.truth_reference import gamma
        with mp.workdps(160):
            a,z=mp.mpf(100),mp.mpf(300)
            expected=mp.gammainc(a,z,mp.inf)/mp.gamma(a)
            self.assertLess(abs(mp.exp(gamma(a,z)[1])/expected-1),mp.mpf('1e-90'))

    def test_bnbinom_factorial_moment_tail(self):
        from quality.test_truth_gaps import reference
        with mp.workdps(160):
            r,a,b=mp.mpf('1e8'),mp.mpf('1e16'),mp.mpf('1e8')
            ref=reference('bnbinom',dict(size=r,alpha=a,beta=b))
            expected=mp.fsum(mp.binomial(r+k-1,k)*mp.beta(a+r,b+k)/mp.beta(a,b) for k in range(101,201))
            self.assertLess(abs(mp.exp(ref.tails(100)[1])/expected-1),mp.mpf('1e-40'))

    def test_cmp_euler_maclaurin_tail_additivity(self):
        from quality.truth import DISTS,module_at
        with mp.workdps(160):
            route=module_at(DISTS/'cmp/truth.py');nu=mp.mpf('1e-8')
            ref=route.unit_rate_reference(nu)
            observed=mp.exp(ref.em_tail(10000))-mp.exp(ref.em_tail(10020))
            expected=mp.fsum(mp.exp(-nu*mp.loggamma(k+1)) for k in range(10000,10020))
            self.assertLess(abs(observed/expected-1),mp.mpf('1e-40'))

    def test_skellam_asymmetric_convolution(self):
        from quality.truth import DISTS,module_at
        with mp.workdps(160):
            route=module_at(DISTS/'skellam/truth.py')
            a,b=mp.mpf(40),mp.mpf(float(.3))
            for A,B,cuts in ((a,b,(-5,0,7,40,50)),(b,a,(-50,-40,-7,0,5))):
                ref=route.asymmetric_reference(A,B)
                for k in cuts:
                    expected=mp.fsum(mp.exp(-B)*B**j/mp.factorial(j)*mp.gammainc(k+j+1,A,mp.inf)/mp.factorial(k+j)
                                     for j in range(max(0,-k),201))
                    self.assertLess(abs(mp.exp(ref.tails(k)[0])/expected-1),mp.mpf('1e-35'))

    def test_discrete_gamma_center_interval(self):
        from quality.truth import DISTS,module_at
        from quality.truth_reference import gamma
        with mp.workdps(160):
            a,r,x=mp.mpf('1e6'),mp.mpf('3.5'),mp.mpf(285714)
            route=module_at(DISTS/'dgamma/truth.py')
            ref=route.build(dict(shape=a,rate=r),None)
            expected=mp.exp(gamma(a,r*(x+1))[0])-mp.exp(gamma(a,r*x)[0])
            self.assertLess(abs(mp.exp(ref.log_density(x))/expected-1),mp.mpf('1e-35'))

    def test_squared_normal_extreme_inverse(self):
        from quality.test_truth_gaps import reference
        with mp.workdps(160):
            for a in (1,-1):
                ref=reference('sn',dict(xi=0,omega=1,alpha=a))
                for lower in (False,True):
                    lp=mp.mpf(-10000)
                    q=ref.quantile(lp,lower)
                    self.assertLess(abs(ref.tails(q)[0 if lower else 1]-lp),mp.mpf('1e-35'))

    def test_derivative_check_follows_density_width(self):
        from quality.truth import verify
        from quality.truth_reference import Reference,normal
        with mp.workdps(160):
            sigma=mp.mpf('1e-100')
            ld=lambda x:-(x/sigma)**2/2-mp.log(sigma)-mp.log(2*mp.pi)/2
            ref=Reference(lambda x:normal(x/sigma),log_density=ld)
            ref.chart=lambda t:(sigma*mp.sinh(t),mp.log(sigma)+mp.log(mp.cosh(t)))
            ref.unchart=lambda x:mp.asinh(x/sigma)
            ref.normalization_identity=mp.mpf(1)
            self.assertEqual(verify(ref,ld),1)

    def test_gamma_power_matches_readme(self):
        with mp.workdps(160):
            a,c,s=mp.mpf('1e-8'),mp.mpf('1e8'),mp.mpf(3)
            for inverse in (False,True):
                r=gamma_power_reference(a,c,s,inverse)
                for x in (s*mp.mpf('.99999999'),s,s*mp.mpf('1.00000001')):
                    z=(s/x)**c if inverse else (x/s)**c
                    expected=mp.log(c)-mp.loggamma(a)+a*mp.log(z)-z-mp.log(x)
                    self.assertLess(abs(r.log_density(x)-expected),mp.mpf('1e-100'))


if __name__=='__main__':unittest.main()
