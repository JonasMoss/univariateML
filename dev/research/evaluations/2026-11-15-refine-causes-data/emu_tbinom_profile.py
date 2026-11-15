"""Emulate tbinom_detail::profile / mass (binom/tbinom_mle.hpp 55-141) in doubles for window [1000000,1000011], sizes xmax+j."""
import math, json
x=[1000000,1000001,1000003,1000008,1000011]
def mass(eta,lo,hi,size):
    lam=math.exp(eta); mode=lam/(1+lam/size)*(1+1/size); mode=math.floor(max(lo,min(hi,mode)))
    total=1.0; offset=0.0
    for d in (-1,1):
        k=mode; w=1.0
        while True:
            if (d<0 and k<=lo) or (d>0 and k>=hi): break
            corr=(1-k/size) if d>0 else (1-(k-1)/size)
            ratio=lam*corr/(k+1) if d>0 else k/lam/corr
            w*=ratio; k+=d; total+=w; offset+=(k-mode)*w
            if ratio<1 and w*ratio/(1-ratio)<1e-17*total: break
    return math.log(total), mode+offset/total, mode
def profile(lo,upper,size,tol=1e-10):
    hi=min(upper,size); mean=sum(x)/len(x)
    centre=math.log(mean)-math.log1p(-mean/size)
    left,right,width=centre-1,centre+1,1; br=False
    for j in range(512):
        l=mass(left,lo,hi,size); h=mass(right,lo,hi,size)
        if l[1]<mean and h[1]>mean: br=True; break
        width*=2
        if l[1]>=mean: left=centre-width
        if h[1]<=mean: right=centre+width
    if not br: return 'no bracket'
    for it in range(512):
        eta=left+(right-left)/2
        if eta==left or eta==right: break
        z=mass(eta,lo,hi,size)
        if z[1]<mean: left=eta
        else: right=eta
    eta=left+(right-left)/2
    if right-left>math.log1p(tol/4): return 'bracket too wide %g'%(right-left)
    z=mass(eta,lo,hi,size)
    if abs((z[1]-mean)*len(x))>=1e-7: return 'score gate: |(mean_hat-mean)*n|=%g >= 1e-7 (eta=%r)'%(abs((z[1]-mean)*len(x)),eta)
    return 'ok'
if __name__=='__main__':
    out={}
    for j in (0,1,2,3,5,10,20,50,128):
        out[j]=profile(1000000,1000011,1000011+j); print(j,out[j])
    json.dump(out,open('dev/research/evaluations/2026-11-15-refine-causes-data/emu_tbinom_profile.json','w'),indent=1)
