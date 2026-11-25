"""Exploratory binary64 scans. High-precision replay is in check.py."""
from itertools import combinations_with_replacement, product
import numpy as np


def sigmoid(z):
    return np.exp(-np.logaddexp(0, -z))


def burr(y, weights, k):
    """Vectorized nested brackets on conditional power and intercept."""
    weights = weights[:, None, :]
    y = y[:, None, :]
    k = k[None, :, None]
    low = np.zeros((len(y), k.size, 1))
    high = low + 128 / np.minimum(k, 1)
    for _ in range(36):
        c = (low + high) / 2
        cy = c * y
        left = -np.log(k) - c
        right = left + c
        for _ in range(36):
            b = (left + right) / 2
            p = sigmoid(cy + b)
            positive = (weights * p).sum(2, keepdims=True) * (1 + k) < 1
            left = np.where(positive, b, left)
            right = np.where(positive, right, b)
        power_score = 1/c + (weights * y * (1 - (1+k)*p)).sum(2, keepdims=True)
        low = np.where(power_score > 0, c, low)
        high = np.where(power_score > 0, high, c)
    z = cy + b
    g = np.logaddexp(0, z)
    values = np.log(c*k) + (weights * (z-(1+k)*g)).sum(2, keepdims=True)
    score = 1-k*(weights*g).sum(2, keepdims=True)
    return values[..., 0], score[..., 0], (c[..., 0], b[..., 0])


def batches():
    for n in range(2, 8):
        samples = [(0, *inner, 8) for inner in combinations_with_replacement(range(9), n-2)]
        yield f'small-{n}', np.array(samples)/8, np.full((len(samples), n), 1/n)
    for n in [3, 4, 5]:
        grid = [1/1024, 1/64, 1/8, 1, 8, 64, 1024]
        gaps = np.array([(1, *p) for p in product(grid, repeat=n-2)])
        y = np.column_stack((np.zeros(len(gaps)), gaps.cumsum(1)))
        y /= y[:, -1:]
        rng = np.random.default_rng(286+n)
        indices = rng.integers(0, len(y), 256)
        w = rng.choice([1,2,5,20,100,1000], (256,n))
        yield f'cluster-{n}', y[indices], w/w.sum(1, keepdims=True)
    eps = 1/10000
    yield 'diagonal-free-lifts', np.array([[0, 1], [0, 1]]), np.array([[eps,1-eps], [1-eps,eps]])
    yield 'diagonal-split-lifts', np.array([[0,t,1] for t in [1/1024,1/64,1/8,7/8,63/64,1023/1024]]), np.tile([eps,eps,1-2*eps], (6,1))


def main():
    k = np.geomspace(1/4096, 4096, 97)
    lograte = np.linspace(-85, 10, 381)
    maxima = []
    for label, y, weights in batches():
        counts = [0, 0]
        for start in range(0, len(y), 24):
            a = y[start:start+24]
            w = weights[start:start+24]
            _, score, _ = burr(a, w, k)
            hits = (score[:,:-1] > 1e-6) & (score[:,1:] < -1e-6)
            counts[0] = max(counts[0], int(hits.sum(1).max()))
            for i in np.where(hits.sum(1) >= 2)[0]:
                maxima.append((label, 'burr', a[i].tolist(), w[i].tolist(), k[:-1][hits[i]].tolist()))
            for spread in [1,4,8,16,32,64]:
                z = lograte[None,:,None] + spread*a[:,None,:]
                g = (w[:,None,:]*np.logaddexp(0,z)).sum(2)
                p = (w[:,None,:]*sigmoid(z)).sum(2)
                score = g*(1-p)-p
                hits = (score[:,:-1] > 1e-12) & (score[:,1:] < -1e-12)
                counts[1] = max(counts[1], int(hits.sum(1).max()))
                for i in np.where(hits.sum(1) >= 2)[0]:
                    maxima.append((label, f'held-{spread}', a[i].tolist(), w[i].tolist(), lograte[:-1][hits[i]].tolist()))
        print(label, len(y), counts, flush=True)
    for hit in maxima[:30]:
        print(hit)
    print('multiple crossing records', len(maxima))


if __name__ == '__main__':
    main()
