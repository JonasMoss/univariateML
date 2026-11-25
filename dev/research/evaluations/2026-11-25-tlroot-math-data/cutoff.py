"""Compare proved half-tie cutoffs on exact rational sample coordinates."""
from fractions import Fraction
from time import perf_counter

import mpmath as mp

from scenarios import cases


def logq(x):
    x = Fraction(x)
    return mp.log(x.numerator)-mp.log(x.denominator)


def cutoffs(x):
    x = sorted(map(Fraction,x))
    n = len(x)
    k = n//2
    W = x[-1]-x[0]
    distinct = sorted(set(x))
    assert n == 2*k and max(x.count(t) for t in distinct) == k
    logB = max(-k*mp.log(8)-2*sum(logq(abs(z-t)) for z in x if z != t)
               for t in distinct if x.count(t) == k)
    delta = min(b-a for a,b in zip(distinct,distinct[1:]))/2
    old = min(logq(W)-mp.log(8)-8*n,3*logq(delta)+logB/k-mp.log(8))

    # The distance list for one complete pairing or the extreme pair alone.
    pairing = sorted([logq((b-a)/2) for a,b in zip(x[:k],x[k:])],reverse=True)
    prefixes = [(m,sum(pairing[:m]),pairing[m-1]) for m in range(1,k+1)]
    prefixes.append((1,logq(W/2),logq(W/2)))
    alpha = mp.mpf(191)/289
    def both(A):
        beta = 1+1/A
        logC = -beta*mp.log(A)
        score = max(min(small-mp.log(mp.expm1(4)),
                        total/m-(m*480/(289*A)+4*(n-m))/(m*alpha))
                    for m,total,small in prefixes)
        holder = max((logB-mp.log(2)+(n-m)*mp.log(2)-m*logC+beta*total)
                     /(m/A-(n-m)) for m,total,_ in prefixes if m/A>n-m)
        return min(score,holder),score,holder
    fixed = both(mp.mpf(".5"))
    best, A, score, holder = max((cut,A,score,holder)
                                 for i in range(50,100)
                                 for A in [mp.mpf(i)/100]
                                 for cut,score,holder in [both(A)])
    new = max(best,old)
    smax = -logB/n
    return old,fixed[0],new,smax,A,score,holder


if __name__ == '__main__':
    start = perf_counter()
    mp.mp.dps = 80
    examples = [(key, x) for key,x in cases().items() if "half tie" in key]
    examples += [("balanced two clusters", [0,0,1,1])]
    # Exact rational samples in the continuation family of the prior report.
    examples += [(f"transition family 1/{d}", [0,0,Fraction(1,d),1])
                 for d in (4, 16, 256, 10**5, 10**10, 10**15)]
    print("sample | log old | log improved A=1/2 | log new | decades gained | log width old | log width new | A | score cutoff | Holder cutoff")
    for name,x in examples:
        old,fixed,new,smax,A,score,holder = cutoffs(x)
        assert old < fixed <= new < smax
        print(name,"|"," | ".join(mp.nstr(v,10) for v in
              (old,fixed,new,(new-old)/mp.log(10),smax-old,smax-new,A,score,holder)))
    print(f"All cutoff checks passed in {perf_counter()-start:.3f}s")
