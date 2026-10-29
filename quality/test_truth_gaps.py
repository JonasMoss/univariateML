"""Independent checks for the four controlled-tail oracle routes."""
import unittest
import mpmath as mp
from quality.truth import DISTS, module_at, read, declarative, encode


def reference(name, values):
    route=module_at(DISTS/name/'truth.py')
    meta,_=read(DISTS/name)
    theta={k:float(v) for k,v in values.items()}
    p={k:mp.mpf(v) for k,v in values.items()}
    logd=declarative(DISTS/name,meta,theta,route)
    return route.build(p,logd)


class ControlledTails(unittest.TestCase):
    def test_nan_truth_cells_rejected(self):
        with self.assertRaises(ArithmeticError):encode(mp.nan)

    def close(self,actual,expected,tol='1e-35'):
        self.assertLess(abs(actual-expected),mp.mpf(tol)*max(abs(expected),mp.mpf('1e-300')))

    def test_bnbinom_beta_geometric_identity(self):
        with mp.workdps(100):
            for a in (mp.mpf(float(.2)),mp.mpf(2),mp.mpf(20)):
                ref=reference('bnbinom',dict(size=1,alpha=a,beta=5))
                for k in (0,299,300,1000,mp.mpf('1e100'),mp.mpf('1e300')):
                    # r=1: S(k)=E[(1-P)^(k+1)], P~Beta(a,b).
                    expected=mp.loggamma(a+5)-mp.loggamma(5)
                    from quality.truth_reference import loggamma_ratio
                    expected+=loggamma_ratio(k+1,5,5+a)
                    self.close(ref.tails(k)[1],expected)

    def test_bnbinom_swapped_beta_quadrature(self):
        with mp.workdps(110):
            for r in (mp.mpf(float(.1)),mp.mpf(float(1.0672234787479615)),mp.mpf(10)):
                a,b=mp.mpf(2),mp.mpf(5)
                ref=reference('bnbinom',dict(size=r,alpha=a,beta=b))
                n=301
                integral=mp.quad(lambda y:y**(r-1)*(1-y)**(n-1)*mp.betainc(a,b,0,y,regularized=True),[0,mp.mpf('.01'),mp.mpf('.1'),1])/mp.beta(r,n)
                self.close(mp.exp(ref.tails(n-1)[1]),integral)
                self.close(ref.log_density(mp.mpf(400)),mp.loggamma(r+400)-mp.loggamma(r)-mp.loggamma(401)+mp.log(mp.beta(a+r,b+400)/mp.beta(a,b)))

    def test_pig_bessel_and_mixture_density(self):
        with mp.workdps(100):
            m,s=mp.mpf(2),mp.mpf(3)
            ref=reference('pig',dict(mean=m,shape=s))
            for k in (0,3,20,400,5000):
                expected=mp.sqrt(2*s/mp.pi)*mp.exp(s/m)*(s/(2+s/m**2))**((mp.mpf(k)-mp.mpf('.5'))/2)*mp.besselk(mp.mpf(k)-mp.mpf('.5'),mp.sqrt(s*(2+s/m**2)))/mp.factorial(k)
                self.close(mp.exp(ref.log_density(mp.mpf(k))),expected)
            for k in (0,3,20):
                mixture=mp.quad(lambda t:mp.exp(-t)*t**k/mp.factorial(k)*mp.sqrt(s/(2*mp.pi*t**3))*mp.exp(-s*(t-m)**2/(2*m*m*t)),[0,1,4,16,mp.inf])
                self.close(mp.exp(ref.log_density(mp.mpf(k))),mixture)

    def test_pig_integral_against_positive_sum(self):
        with mp.workdps(110):
            for m,s,n in ((2,3,400),(20,3,1000)):
                m,s=mp.mpf(m),mp.mpf(s)
                ref=reference('pig',dict(mean=m,shape=s))
                z=mp.sqrt(s*(2+s/m**2))
                def logmass(k):return mp.log(2*s/mp.pi)/2-mp.loggamma(k+1)+s/m+(mp.mpf(k)-mp.mpf('.5'))*mp.log(s/(2+s/m**2))/2+mp.log(mp.besselk(mp.mpf(k)-mp.mpf('.5'),z))
                # Separate high precision K recurrence, initialized by two direct Bessels.
                term=mp.exp(logmass(n)); ratio=mp.exp(logmass(n+1)-logmass(n));total=term
                A=2*m*m/(s+2*m*m);C=m/mp.sqrt(1+2*m*m/s);k=n
                while True:
                    k+=1;term*=ratio;total+=term
                    ratio=A*(1-mp.mpf('1.5')/(k+1))+C*C/(k*(k+1)*ratio)
                    bound=A+max(C-mp.mpf('1.5')*A,0)/(k+1)
                    if term*bound/(1-bound)<total*mp.mpf('1e-55'):break
                self.close(mp.exp(ref.tails(n-1)[1]),total)

    def test_skellam_poisson_convolution_and_symmetry(self):
        with mp.workdps(100):
            for a,b in ((4,3),(mp.mpf(float(.4)),3),(40,3)):
                ref=reference('skellam',dict(mu1=a,mu2=b))
                self.assertEqual(ref.tails(-mp.inf),(-mp.inf,mp.mpf(0)))
                self.assertEqual(ref.tails(mp.inf),(mp.mpf(0),-mp.inf))
                for k in (-5,0,7):
                    # Condition on the negative Poisson count; gamma integral gives Poisson CDF.
                    expected=mp.fsum(mp.exp(-b)*mp.mpf(b)**j/mp.factorial(j)*mp.gammainc(k+j+1,a,mp.inf)/mp.factorial(k+j) for j in range(max(0,-k),300))
                    self.close(mp.exp(ref.tails(k)[0]),expected)
            ref=reference('skellam',dict(mu1=3,mu2=3))
            self.close(mp.exp(ref.tails(-1)[0]),(1-mp.exp(ref.log_density(0)))/2)

    def test_skellam_zero_rate_poisson_limits(self):
        with mp.workdps(100):
            for a,b in ((3,0),(0,3),(0,0)):
                ref=reference('skellam',dict(mu1=a,mu2=b))
                for k in (-5,-1,0,5):
                    if a==b==0:expected=mp.mpf(k>=0)
                    elif b==0:expected=mp.mpf(0) if k<0 else mp.gammainc(k+1,a,mp.inf)/mp.factorial(k)
                    else:expected=mp.mpf(1) if k>=0 else mp.gammainc(-k,0,b)/mp.factorial(-k-1)
                    self.close(mp.exp(ref.tails(k)[0]),expected)
                self.assertEqual(ref.quantile(-mp.inf,True),ref.lower)
                self.assertEqual(ref.quantile(-mp.inf,False),ref.upper)

    def test_sn_shape_one_identity_and_reflection(self):
        with mp.workdps(100):
            for a in (1,-1):
                ref=reference('sn',dict(xi=0,omega=1,alpha=a))
                for z in (-20,-3,0,2,20):
                    phi=mp.erfc(-mp.mpf(z)/mp.sqrt(2))/2
                    # alpha=1: F(z)=Phi(z)^2, alpha=-1: reflect.
                    expected=phi*phi if a==1 else 2*phi-phi*phi
                    self.close(mp.exp(ref.tails(z)[0]),expected)
                for prob in (mp.mpf('.01'),mp.mpf('.5')):
                    expected=mp.sqrt(2)*mp.erfinv(2*mp.sqrt(prob if a==1 else 1-prob)-1)*(1 if a==1 else -1)
                    self.close(ref.quantile(mp.log(prob),True),expected)
            ref=reference('sn',dict(xi=0,omega=1,alpha=3))
            for z in (-3,0,2):
                expected=mp.quad(lambda t:mp.exp(-t*t/2)/mp.sqrt(2*mp.pi)*mp.erfc(-3*t/mp.sqrt(2)),[-mp.inf,-6,z])
                self.close(mp.exp(ref.tails(z)[0]),expected)


if __name__=='__main__':unittest.main()
