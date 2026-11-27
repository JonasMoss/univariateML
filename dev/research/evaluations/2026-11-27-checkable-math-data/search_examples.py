"""Numerical proposals for exact counterexamples, independently checked later."""
from math import log, sqrt
import numpy as np
import pilot as p

def symmetric_score(radii, weights, q):
    z = radii**2
    lo, hi = 0., max(z)
    for _ in range(46):
        mid = (lo+hi)/2
        if np.dot(weights,z/(q*mid+z)) > 1/(q+1):
            lo = mid
        else:
            hi = mid
    value = p.normalizer(q)-log(sqrt(hi))-(q+1)/2*np.dot(weights,np.log1p(z/(q*hi)))
    return value, sqrt(hi)

def find_student():
    best = (-1, None)
    for n in range(3, 19):
        for k in range(1,n):
            weights = np.array([k,n-k])/n
            for a in np.arange(1, 24)/48:
                radii = np.array([a,1.])
                var = np.dot(weights,radii**2)
                kurt = np.dot(weights,radii**4)/var**2
                if kurt >= 3:
                    continue
                normal = p.NORMAL-log(sqrt(var))
                face, _ = symmetric_score(radii,weights,2)
                for q in (2.125,2.5,3,4,6,8,16,32):
                    value, scale = symmetric_score(radii,weights,q)
                    gap = value-max(normal,face)
                    if gap > best[0]:
                        best = (gap,(n,k,a,kurt,normal,face,q,value,scale))
    print("std best",best,flush=True)


def find_skew_student():
    rng = np.random.default_rng(236)
    for i in range(7500):
        n = (6, 8, 10, 12, 15, 20)[i % 6]
        x = np.sort(np.round(rng.standard_t(5,n)*30).astype(int))
        if np.unique(x).size < 4:
            continue
        y = (x-x[0])/(x[-1]-x[0])
        w = p.skew_student_witness(y)
        N = p.skew_normal_upper(y)
        if w <= N or not p.half_student_excluded(y,w):
            continue
        if p.skew_two_excluded(y,w):
            continue
        # Additional shapes and joins test whether a genuine improved proof
        # can be made on this particular sample.
        centers = np.unique(np.r_[y[1:-1], np.linspace(0,1,201)[1:-1]])
        left = np.maximum(centers[:,None]-y,0)
        right = np.maximum(y-centers[:,None],0)
        for q in (2.02,2.05,2.1,2.15,2.2,2.3,2.4,2.6,3.5,4,6,7,8,12,15,20,30,40,60,100,160,256):
            a,b = p.side_log_widths(left,right,q)
            value = log(2)+p.normalizer(q)-np.logaddexp(a,b)
            value -= (q+1)/2*np.log1p((left/np.exp(a[:,None])+right/np.exp(b[:,None]))**2/q).mean(axis=1)
            j = np.argmax(value)
            if (value[j] > w+0.0001 and p.skew_two_excluded(y,value[j])
                    and p.half_student_excluded(y,value[j])):
                print("sstd candidate",x,w,N,q,centers[j],np.exp(a[j]),np.exp(b[j]),value[j],flush=True)
                return
    raise RuntimeError("no sstd proposal")

if __name__ == "__main__":
    find_student()
    find_skew_student()
