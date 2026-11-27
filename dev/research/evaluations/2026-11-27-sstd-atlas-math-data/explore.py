"""Grid reconnaissance for symmetric bulk samples."""
import math
import numpy as np

def grid(nodes, counts, nu):
    modes = np.linspace(0, nodes[-1], 181)
    weights = np.linspace(.001, .999, 151)
    widths = np.geomspace(.15, 12, 191)
    w = widths[None, None, :] * weights[None, :, None]
    b = widths[None, None, :] * (1 - weights[None, :, None])
    normal = math.isinf(nu)
    k = math.sqrt(2 / math.pi) if normal else 2 * math.exp(
        math.lgamma((nu + 1) / 2) - math.lgamma(nu / 2)) / math.sqrt(nu * math.pi)
    ll = np.full((181, 151, 191), math.log(k)) - np.log(widths)
    for x, count in zip(nodes, counts):
        r = (x - modes)[:, None, None]
        u = (r / np.where(r < 0, w, b)) ** 2
        ll -= count / sum(counts) * (u / 2 if normal else
                                    ((nu + 1) / 2) * np.log1p(u / nu))
    i, j, l = np.unravel_index(ll.argmax(), ll.shape)
    return tuple(round(float(v), 7) for v in (ll[i, j, l], modes[i], weights[j], widths[l]))

if __name__ == "__main__":
    for edge, count in [(1, 0), (2, 1), (2, 2), (3, 1)]:
        nodes, counts = [-edge, -1, 1, edge], [count, 10, 10, count]
        print(edge, count, [(nu, grid(nodes, counts, nu)) for nu in (2, 3, 10, math.inf)], flush=True)
    for edge, count in [(1.5,1),(1.75,1),(2.25,1),(1.5,2),(1.75,2),(2,3)]:
        nodes, counts = [-edge, -1, 1, edge], [count, 10, 10, count]
        bound = -.5 * (1 + math.log(2 * math.pi)) - count / (10 + count) * math.log(edge)
        print(edge, count, bound, [(nu, grid(nodes, counts, nu)) for nu in (2, 30, math.inf)], flush=True)
