# Closed forms at lambda=-1 and 1/2, L-moments and kurtosis, checked at 40 digits.
import mpmath as mp
mp.mp.dps = 40
def Q(u,l): return mp.log(u/(1-u)) if l==0 else (u**l-(1-u)**l)/l
def D(u,l): return u**(l-1)+(1-u)**(l-1)
def uinv(x,l):
    lo,hi=mp.mpf(0),mp.mpf(1)
    return mp.findroot(lambda u: Q(u,l)-x, (mp.mpf('1e-30'),1-mp.mpf('1e-30')), solver='anderson')
def f(x,l): return 1/D(uinv(x,l),l)
# closed forms
for x in [0, mp.mpf('0.3'), mp.mpf('1.7'), mp.mpf('25')]:
    r=mp.sqrt(4+x**2)
    print('l=-1', x, mp.nstr(f(x,-1)-1/(r*(r+2)),5))
for x in [0, mp.mpf('0.3'), mp.mpf('1.7'), mp.mpf('1.99')]:
    print('l=1/2', x, mp.nstr(f(x,mp.mpf(1)/2)-(4-x**2)/(4*mp.sqrt(8-x**2)),5))
# L-moments
for l in [mp.mpf('-0.7'), mp.mpf('0.14'), mp.mpf('0.6'), mp.mpf('1.5'), mp.mpf(3)]:
    l2=mp.quad(lambda u: Q(u,l)*(2*u-1),[0,0.5,1])
    l4=mp.quad(lambda u: Q(u,l)*(20*u**3-30*u**2+12*u-1),[0,0.5,1])
    print('L', l, mp.nstr(l2-2/((l+1)*(l+2)),5), mp.nstr(l4/l2-(l-1)*(l-2)/((l+3)*(l+4)),5))
# kurtosis via Beta functions
def mom(k,l): return sum(mp.binomial(k,j)*(-1)**j*mp.beta(l*(k-j)+1,l*j+1) for j in range(k+1))/l**k
kurt=lambda l: mom(4,l)/mom(2,l)**2
lm=mp.findroot(lambda l: mp.diff(kurt,l), 1.45)
print('kurt min', mp.nstr(lm,8), mp.nstr(kurt(lm),8), 'kurt(0.14)=',mp.nstr(kurt(mp.mpf('0.14')),6), 'normal-closest', mp.nstr(mp.findroot(lambda l: kurt(l)-3, 0.14),6))
