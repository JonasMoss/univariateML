"""Compare compiled public results to independent physical-coordinate roots."""
import csv
import sys
import mpmath as mp


def reference(k, gap, reflected, mask):
    width = 2**k + 1
    q = [1/mp.mpf(width), (1-gap)/width]
    logp = sum(mp.log1p(-v) for v in q)/2
    logq = sum(mp.log(v) for v in q)/2
    if reflected and mask<4:
        mask = ((mask & 1) << 1) | ((mask & 2) >> 1)
    if mask == 0:
        scale = 1/mp.exp(logq)
        c, b = mp.findroot(
            lambda c, b: ((mp.digamma(scale*c+b)-mp.digamma(scale*c)+logp)*scale,
                          mp.digamma(scale*c+b)-mp.digamma(b)+logq),
            (1/(2*(mp.log(-logp)-logq)), 1/(2*(mp.log(-logp)-logq))),
        )
        a = scale*c
    elif mask == 1:
        a, b = mp.mpf(1), -1/logq
    elif mask == 2:
        a, b = -1/logp, mp.mpf(1)
    elif mask == 3:
        a = b = mp.mpf(1)
    else:
        a, b = (mp.mpf(3), mp.mpf(2)) if reflected else (mp.mpf(2), mp.mpf(3))
    ll = 2*((a-1)*logp+(b-1)*logq-mp.log(mp.beta(a,b))-mp.log(width))
    return (b, a, ll) if reflected else (a, b, ll)


if __name__ == "__main__":
    print("power,gap,reflected,mask,status,shape1_ref,shape2_ref,logLik_ref,max_error")
    for k, gap, reflected, mask, status, a, b, ll in csv.reader(open(sys.argv[1])):
        with mp.workdps(100):
            low = reference(int(k), mp.mpf(float(gap)), int(reflected), int(mask))
        with mp.workdps(150):
            high = reference(int(k), mp.mpf(float(gap)), int(reflected), int(mask))
            digits = [mp.nstr(v, 50) for v in high]
            assert digits == [mp.nstr(v, 50) for v in low]
            err = max(abs(mp.mpf(float(v))-r)/max(abs(r), 1 if i == 2 else 0)
                      for i, (v, r) in enumerate(zip((a, b, ll), high))) if status == "0" else mp.nan
            print(k, gap, reflected, mask, status, *digits, mp.nstr(err, 6), sep=",")
            if "--check" in sys.argv:
                assert status == "0" and err <= mp.mpf("1e-10")
