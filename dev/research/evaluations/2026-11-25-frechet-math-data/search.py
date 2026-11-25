"""Binary64 exploration. Confirm witnesses separately at high precision."""
import numpy as np

def free(x, p, b):
    b = np.asarray(b)[:, None]
    lo, hi = np.full_like(b, -40), np.full_like(b, 40)
    for _ in range(58):
        y = (lo + hi) / 2
        z = x * np.exp(-y)
        t = np.log1p(b*z) / np.where(b == 0, 1, b)
        t = np.where(b == 0, z, t)
        w = p * np.exp(-t)
        r = 1/(1+b*z)
        score = np.sum(p*((1+b)*z*r-1), axis=1, keepdims=True)
        score -= np.sum(w*z*r, axis=1, keepdims=True)/np.sum(w, axis=1, keepdims=True)
        lo, hi = np.where(score > 0, y, lo), np.where(score > 0, hi, y)
    return (-y[:, 0] - (1+b[:, 0])*np.sum(p*t, axis=1)
            -np.log(np.sum(w, axis=1))-1)

def held(x, p, shapes):
    a = np.asarray(shapes)[:, None]
    lo, hi = np.full_like(a, np.log(p[0]/2)), np.zeros_like(a)
    for _ in range(56):
        u = (lo+hi)/2
        r = 1/(1+x*np.exp(-u/a))
        z = u+a*np.log1p(x*np.exp(-u/a))
        h = np.exp(-z)
        score = np.sum(p*r*(h-1-1/a), axis=1, keepdims=True)
        lo, hi = np.where(score > 0, u, lo), np.where(score > 0, hi, u)
    return np.log(a[:, 0])-np.sum(p*((1+1/a)*z+h),axis=1)

def maxima(values):
    return np.flatnonzero(np.r_[True, values[1:]>values[:-1]]
                          & np.r_[values[:-1]>values[1:], True])

if __name__ == '__main__':
    b = np.linspace(0, 1, 101)
    a = np.geomspace(1, 1e5, 151)
    for weights in [(49,50,1),(9,10,1),(19,20,1),(4,5,1),(2,3,1),
                    (1,2,1),(1,3,1),(1,1,1),(49,49,2),(48,51,1)]:
        p = np.array(weights)/sum(weights)
        for upper in np.geomspace(1.01,1e4,101):
            x = np.array([0.,1.,upper])
            v = free(x,p,b)
            k = maxima(v)
            if len(k[k>0])>1:
                print('free:',weights,upper,b[k],v[k], 'Gumbel',v[0])
                break
    rng = np.random.default_rng(287)
    for iteration in range(4000):
        weights = rng.integers(1, 20, 3)
        p = weights/sum(weights)
        x = np.r_[0., np.sort(10**rng.uniform(-3,2,2))]
        v = held(x,p,a)
        k = maxima(v)
        if len(k)>1:
            print('held:',weights,x,a[k],v[k])
            break
    else:
        print('held: no double maximum on grid')
