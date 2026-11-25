"""Find rational symmetric profiles with two finite peaks by varying the inner spread."""
import mpmath as mp
from exact_modes import scale, real, F

mp.mp.dps = 35
shapes = [2, 3, 4, 6, 10, 20, 50, 100, 500, 2000, 10000, 100000, 1000000]
normalizer = {v:mp.digamma((mp.mpf(v)+1)/2)-mp.digamma(mp.mpf(v)/2)-1/mp.mpf(v)
              for v in shapes}


def signs(radii, counts):
    q = [r*r for r in radii]
    result = []
    for v in shapes:
        s = real(sum(scale(v, q, counts, 65))/2)
        u = [real(a)/(v*s) for a in q]
        result.append(normalizer[v] + sum(w*(-mp.log1p(a)+(1+1/mp.mpf(v))*a/(1+a))
                                         for a,w in zip(u, counts))/sum(counts))
    return result


if __name__ == "__main__":
    for j in range(1, 31):
        r = [F(j*i, 100) for i in (1,2,3,4)] + [F(99,100), F(101,100), F(1000)]
        g = signs(r, [10**10]*6+[1])
        pattern = ''.join('+' if y>0 else '-' for y in g)
        print(j, pattern)
        if '+-+-' in ''.join(c for i,c in enumerate(pattern) if i==0 or pattern[i-1]!=c):
            print(shapes)
            print([mp.nstr(a, 8) for a in g])
            break

    import numpy as np
    rng = np.random.default_rng(252)
    v = np.array([2, 2.25, 2.5, 3, 4, 5, 6, 8, 10, 15, 20, 30, 50, 100, 200,
                  500, 1000, 3000, 10000, 30000, 100000, 300000, 1000000])[:, None, None]
    Nv = np.array([float(mp.digamma((mp.mpf(float(t))+1)/2) -
                         mp.digamma(mp.mpf(float(t))/2) - 1/mp.mpf(float(t)))
                   for t in v[:,0,0]])[:,None]
    for iteration in range(100):
        count = rng.integers(1, 50, (1, 250, 5))
        dist = np.exp(rng.uniform(-5, 1, (1,250,5)))
        count = np.concatenate((count*10**7, np.ones((1,250,1), int)), axis=2)
        dist = np.concatenate((dist, np.full((1,250,1), 1000)), axis=2)
        q = dist**2
        lo = np.zeros((len(v),250,1))
        hi = np.full_like(lo, 10**6)
        for _ in range(65):
            m = (lo+hi)/2
            greater = ((v+1)*np.sum(count*q/(v*m+q), axis=2, keepdims=True)>
                       np.sum(count, axis=2, keepdims=True))
            lo = np.where(greater, m, lo)
            hi = np.where(greater, hi, m)
        u = q/(v*((lo+hi)/2))
        scores = Nv+np.sum(count*(-np.log1p(u)+(1+1/v)*u/(1+u)), axis=2)/np.sum(count, axis=2)
        found = False
        for j in range(250):
            signs = ['+' if a>0 else '-' for a in scores[:,j]]
            pattern = ''.join(c for k,c in enumerate(signs) if k==0 or signs[k-1]!=c)
            if '+-+-' in pattern:
                print('found', list(np.round(dist[0,j], 5)), list(count[0,j]))
                print(list(zip(v[:,0,0], signs)))
                found = True
                break
        if found:
            break
