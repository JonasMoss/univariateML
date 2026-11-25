"""Binary64 reconnaissance for a uniform signed Weibull transition."""
from pathlib import Path
import sys
import numpy as np

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "2026-11-25-burr-math-data"))
from search import burr


def faces(y, w):
    lo = np.zeros(len(y))
    hi = lo + 512
    mean = (w*y).sum(1)
    for _ in range(50):
        c = (lo+hi)/2
        e = np.exp(c[:,None]*(y-1))
        z = (w*e).sum(1)
        positive = 1/c + mean - (w*e*y).sum(1)/z > 0
        lo = np.where(positive,c,lo)
        hi = np.where(positive,hi,c)
    weibull = np.log(c)+c*(mean-1)-np.log(z)-1
    return -1-np.log(mean), weibull, (w*(e/z[:,None])**2).sum(1)/2-1


def main():
    k = np.geomspace(1/128, 512, 113)
    for label, y, w in [
        ("two-point", np.tile([0,1], (7,1)),
         np.array([[p,1-p] for p in [1/100,1/10,1/5,1/4,1/3,2/5,1/2]])),
        ("negative-slope deformation",
         np.array([[0,5*t/15,7*t/15,8*t/15,1] for t in np.linspace(0,1,21)]),
         np.tile(np.array([1,1,1,2,2])/7, (21,1))),
        ("upper-mass deformation",
         np.array([[0,1-(1-5/15)*t,1-(1-7/15)*t,1-(1-8/15)*t,1]
                   for t in np.linspace(0,1,21)]),
         np.tile(np.array([1,1,1,2,2])/7, (21,1))),
    ]:
        p, we, a = faces(y,w)
        f, score, _ = burr(y,w,k)
        for i in range(len(y)):
            j = np.argmax(f[i])
            print(label, y[i].tolist(), w[i,0], we[i]-p[i], a[i],
                  f[i,j]-we[i], k[j], flush=True)


if __name__ == "__main__":
    main()
