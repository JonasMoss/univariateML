"""Compare prototype with high-precision transformed-tail quadrature."""
import importlib.util
import math
import pathlib
import mpmath as mp
p=pathlib.Path(__file__).parent
spec=importlib.util.spec_from_file_location('prototype',p/'quadrature-prototype.py')
mod=importlib.util.module_from_spec(spec)
spec.loader.exec_module(mod)
mp.mp.dps=70
for qstr in ('-0.01','-1e-8','0','1e-8','0.01'):
    q=mp.mpf(qstr)
    if q:
        a=1/q**2
        rem=mp.loggamma(a)-(a-mp.mpf('0.5'))*mp.log(a)+a-mp.log(2*mp.pi)/2
    else: rem=0
    for w0 in (-100,-40,40,100):
        w=mp.mpf(w0)
        def penalty(v):
            return (mp.expm1(q*v)-q*v)/q**2 if q else v*v/2
        rate=max(1,abs(mp.expm1(q*w)/q) if q else abs(w))
        direction=-1 if w<0 else 1
        baseline=penalty(w)
        integral=mp.quad(lambda t:mp.exp(-(penalty(w+direction*t/rate)-baseline)),[0,1,5,20,100,500])
        ref=-mp.log(2*mp.pi)/2-rem-baseline+mp.log(integral/rate)
        got,_=mod.smaller_tail(float(w),float(q))
        print(qstr,w0,mp.nstr(ref,20),'error',mp.nstr(mp.mpf(got)-ref,6))
