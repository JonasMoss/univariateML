"""Replay endpoint factors, mixed-path bounds and stationary-face slopes."""
import mpmath as mp

from check import likelihood, logdata, threshold


def evaluate(dps):
    mp.mp.dps = dps
    y = logdata((1, 1, 2, 4))
    k, face = threshold(y)
    held_face = face - 2 * mp.log(2)
    for b in map(mp.mpf, (32, 128, 512)):
        h = b / k
        fixed = likelihood(y, b, -mp.log(h), h)
        assert fixed < held_face
        moving = likelihood(y, b, mp.sqrt(b) - mp.log(h), h)
        assert moving < face
        print(dps, 'threshold', b, 'held loss',
              mp.nstr(held_face - fixed, 10), 'moving loss',
              mp.nstr(face - moving, 10))
    for sample in [(1, 2), (1,) * 31 + (1024,)]:
        y = logdata(sample)
        score = lambda v: sum(
            mp.exp(v + yi) * mp.expm1(mp.exp(v + yi)) for yi in y
        ) - len(y)
        lo, hi = -mp.log(max(sample)) - 2, mp.mpf(2)
        for _ in range(4 * dps):
            v = (lo + hi) / 2
            if score(v) > 0:
                hi = v
            else:
                lo = v
        v = (lo + hi) / 2
        u = [mp.exp(v + yi) for yi in y]
        d = sum(-ui + ui**2 * mp.expm1(ui) / 2 for ui in u)
        step = mp.mpf('1e-15')
        fd = (likelihood(y, 1, v, step) -
              likelihood(y, 1, v, 0)) / step
        assert abs(fd - d) < mp.mpf('1e-12')
        assert (d > 0) == (max(sample) == 1024)
        print(dps, 'held shape 1', 'n', len(sample), 'max', max(sample),
              'slope', mp.nstr(d, 13))


if __name__ == '__main__':
    for precision in (75, 105):
        evaluate(precision)
