"""Family-local symbolic density; df derivatives use numerical differentiation.

The quality harness's symbolic path does not directly provide derivatives of a
Student-t CDF in its degrees of freedom. Here they are independently evaluated
with mpmath differentiation (multiprecision) or centered scipy differences.
"""
import mpmath as mp
import numpy as np
import scipy.special as sc
import sympy as sp


def _is_mp(x):
    return isinstance(x,(mp.mpf,mp.mpc))


def _cdf(x, nu):
    if _is_mp(x) or _is_mp(nu):
        u=mp.betainc(nu/2,mp.mpf('.5'),0,nu/(nu+x*x),regularized=True)/2
        return u if x<0 else 1-u
    return sc.stdtr(nu,x)


def _pdf(x,nu):
    if _is_mp(x) or _is_mp(nu):
        return mp.exp(mp.loggamma((nu+1)/2)-mp.loggamma(nu/2)-mp.log(nu*mp.pi)/2-(nu+1)/2*mp.log1p(x*x/nu))
    return np.exp(sc.gammaln((nu+1)/2)-sc.gammaln(nu/2)-np.log(nu*np.pi)/2-(nu+1)/2*np.log1p(x*x/nu))


def _dnu(x,nu):
    if _is_mp(x) or _is_mp(nu):return mp.diff(lambda n:_cdf(x,n),nu)
    h=np.maximum(1e-5*np.asarray(nu),1e-7)
    h=np.minimum(h,np.asarray(nu)/4)
    return (-_cdf(x,nu+2*h)+8*_cdf(x,nu+h)-8*_cdf(x,nu-h)+_cdf(x,nu-2*h))/(12*h)


def _dnu2(x,nu):
    if _is_mp(x) or _is_mp(nu):return mp.diff(lambda n:_cdf(x,n),nu,2)
    h=np.maximum(5e-4*np.asarray(nu),1e-6)
    h=np.minimum(h,np.asarray(nu)/4)
    return (-_cdf(x,nu+2*h)+16*_cdf(x,nu+h)-30*_cdf(x,nu)+16*_cdf(x,nu-h)-_cdf(x,nu-2*h))/(12*h*h)


class StudentCDF(sp.Function):
    nargs=2
    _imp_=staticmethod(_cdf)
    @classmethod
    def eval(cls,x,nu):
        if x.is_zero:return sp.Rational(1,2)
    def fdiff(self,argindex=1):
        x,nu=self.args
        return StudentPDF(x,nu) if argindex==1 else StudentNu(x,nu)
    def _eval_evalf(self,prec):
        x,nu=self.args
        if x.is_number and nu.is_number:
            with mp.workprec(prec+15):return sp.Float(str(_cdf(mp.mpf(str(x.evalf(prec))),mp.mpf(str(nu.evalf(prec))))),precision=prec)


class StudentPDF(sp.Function):
    nargs=2
    _imp_=staticmethod(_pdf)
    def fdiff(self,argindex=1):
        x,nu=self.args
        if argindex==1:return -(nu+1)*x/(nu+x*x)*self
        return self*(sp.digamma((nu+1)/2)/2-sp.digamma(nu/2)/2-1/(2*nu)-sp.log(1+x*x/nu)/2+(nu+1)*x*x/(2*nu*(nu+x*x)))


class StudentNu(sp.Function):
    nargs=2
    _imp_=staticmethod(_dnu)
    def fdiff(self,argindex=1):
        x,nu=self.args
        if argindex==1:
            return StudentPDF(x,nu)*(sp.digamma((nu+1)/2)/2-sp.digamma(nu/2)/2-1/(2*nu)-sp.log(1+x*x/nu)/2+(nu+1)*x*x/(2*nu*(nu+x*x)))
        return StudentNu2(x,nu)


class StudentNu2(sp.Function):
    nargs=2
    _imp_=staticmethod(_dnu2)


def log_density(p,x):
    xi,omega,alpha,nu=(p[k] for k in ('xi','omega','alpha','nu'))
    z=(x-xi)/omega
    h=alpha*z*sp.sqrt((nu+1)/(nu+z*z))
    return (sp.log(2)-sp.log(omega)+sp.loggamma((nu+1)/2)-sp.loggamma(nu/2)
            -sp.log(nu*sp.pi)/2-(nu+1)*sp.log(1+z*z/nu)/2+sp.log(StudentCDF(h,nu+1)))
