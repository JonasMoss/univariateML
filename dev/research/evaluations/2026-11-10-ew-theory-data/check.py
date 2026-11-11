#!/usr/bin/env python3
"""High precision pointwise checks of EW escape formulas; no optimization claims."""
import json
import mpmath as m
m.mp.dps=100

def ew(x, loga, b, logc):
    t=m.exp(b*(m.log(x)-loga)); c=m.exp(logc)
    q=-m.expm1(-t)
    return c*m.log(q), logc+m.log(b)-m.log(x)+m.log(t)-t+(c-1)*m.log(q)

def inverse(x,k,s):
    z=(s/x)**k
    return -z, m.log(k/s)+(-k-1)*m.log(x/s)-z

def power(x,k,s):
    return k*m.log(x/s),m.log(k/s)+(k-1)*m.log(x/s)

def err(pair, expected):
    return [float(abs(a-b)) for a,b in zip(pair,expected)]
out={'precision_digits':100,'inverse_weibull':[],'power_face':[],'fixed_b_concentration':[]}
for T in [10,20,40,80]:
    # b*T=k, c=exp(T), a=s/T**(1/b)
    k=m.mpf('2'); s=m.mpf('3'); b=k/T; loga=m.log(s)-m.log(T)/b
    out['inverse_weibull'].append({'T':T,'errors_x_2':err(ew(m.mpf('2'),loga,b,m.mpf(T)),inverse(m.mpf('2'),k,s))})
for b in [10,30,100,300]:
    k=m.mpf('2'); s=m.mpf('3'); c=k/b
    out['power_face'].append({'b':b,'errors_x_2':err(ew(m.mpf('2'),m.log(s),m.mpf(b),m.log(c)),power(m.mpf('2'),k,s))})
for T in [10,20,40,80]:
    # b fixed, a chosen so the t at x=2 equals log c = T.
    b=m.mpf('2'); loga=m.log(2)-m.log(T)/b
    out['fixed_b_concentration'].append({'T':T,'log_density_at_2':float(ew(m.mpf(2),loga,b,m.mpf(T))[1]), 'log_density_at_3':float(ew(m.mpf(3),loga,b,m.mpf(T))[1])})
print(json.dumps(out,indent=2))
