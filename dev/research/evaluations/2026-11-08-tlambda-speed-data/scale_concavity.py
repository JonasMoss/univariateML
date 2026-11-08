# m(z) = -z psi(z) = Q(u) D_u(u) / D(u)^2 increasing in z>0 <=> scale-only log-likelihood concave in log sigma.
# Parametrized by t=-log p (p the smaller tail), which is monotone in z. 50 digits.
import mpmath as mp
mp.mp.dps=50
def m_of_t(t,l):
    p=mp.e**(-t); u=1-p
    z=(u**l-p**l)/l
    D=u**(l-1)+p**(l-1); Du=(l-1)*(u**(l-2)-p**(l-2))
    return z, z*Du/D**2
tot=0; real=0; worst=0
for l in [mp.mpf(x)/100 for x in range(-600,100,7) if x!=0]:
    prev=None
    for t in [mp.log(2)+mp.mpf(10)**k for k in mp.linspace(-6,3,300)]:
        z,m=m_of_t(t,l); tot+=1
        if prev is not None and m<prev:
            worst=max(worst,prev-m)
            if prev-m>mp.mpf(10)**-40: real+=1
        prev=m
print('points',tot,'decreases above 1e-40:',real,'largest decrease (rounding):',mp.nstr(worst,3))
