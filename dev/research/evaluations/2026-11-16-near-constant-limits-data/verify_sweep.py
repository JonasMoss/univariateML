"""Checks sweep.out (sweep.cpp: random near-constant samples, rho 1e-16..1e-6, n 3..6) against 80-digit gamma / inverse-gamma
(betapr) and power / log-gamma (genbeta) face values: the face chosen and its log likelihood. 668 limits, 0 disagreements.
log likelihoods in sweep-before-dd.out are the face suprema (the committed code reports them at the rounded coefficients)."""
import re, mpmath as mp, sys
mp.mp.dps=80
def parse(l):
    m=re.search(r' x=(.*)$',l); return [mp.mpf(float.fromhex(t)) for t in m[1].strip(',').split(',')]
def gface(x):
    n=len(x); m=sum(x)/n; ml=sum(mp.log(v) for v in x)/n; c=mp.log(m)-ml
    a=mp.findroot(lambda t: mp.log(t)-mp.digamma(t)-c, 1/(2*c)+mp.mpf(1)/6, tol=mp.mpf(10)**-60)
    return a, sum((a-1)*mp.log(v)-v*a/m-mp.loggamma(a)-a*mp.log(m/a) for v in x)
def bp(x):
    ag,Lg=gface(x); y=[1/v for v in x]; ai,Li=gface(y); Li-=2*sum(mp.log(v) for v in x)
    return ag,Lg,ai,Li
def gb(x):
    w=[-mp.log(v) for v in x]; n=len(w)
    q,Lg=gface(w); Lg+=sum(w)
    ymin=min(w); gap=sum(v-ymin for v in w)/n; Lp=n*(-mp.log(gap)-1)+sum(w)
    return q,Lg,Lp
import collections
bad=0;cnt=collections.Counter()
for l in open('sweep.out'):
    m=re.match(r'(\w) n=(\d+).* old=(\d+) new=(\d+) .*ll=(\S+) model=(\d+)',l)
    if not m: continue
    fam,old,new,ll,model=m[1],m[3],m[4],float(m[5]) if m[5]!='nan' else None,int(m[6])
    if (fam=='B' and new in('3','4')) or (fam=='G' and new in('1','2')):
        x=parse(l)
        if fam=='B':
            ag,Lg,ai,Li=bp(x); win='inv' if Li>Lg else 'gam'
            mine='inv' if new=='4' else 'gam'
            ref=Li if Li>Lg else Lg
            mp_sign=mp.sign(Li-Lg)
            cnt[(fam,win==mine)]+=1
            if win!=mine or abs(ll-ref)>1e-9: bad+=1; print('BAD',l[:120],mp.nstr(Li-Lg,5),float(ref),ll)
        else:
            q,Lg,Lp=gb(x); win='pow' if Lp>Lg else 'lg'; mine='pow' if new=='2' else 'lg'
            ref=max(Lp,Lg); cnt[(fam,win==mine)]+=1
            if win!=mine or abs(ll-ref)>1e-8: bad+=1; print('BAD',l[:120],float(Lp-Lg),float(ref),ll)
print(cnt,bad)
