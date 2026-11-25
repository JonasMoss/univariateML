"""Enclose strict witness comparisons and replay the Fréchet transfer."""
import mpmath as mp

from replay import high, sample


def fixed_value(data, mu, sigma, shape, ctx):
    values = []
    for x in data:
        z = (x - mu) / sigma
        if shape == -1:
            values.append(-ctx.log(sigma) - 1 + z)
        else:
            ell = ctx.log(1 + shape*z) / shape if shape else z
            values.append(-ctx.log(sigma) - (1 + shape)*ell - ctx.exp(-ell))
    return sum(values) / len(data)


def endpoint_witnesses():
    ctx = mp.iv
    ctx.dps = 50
    i = ctx.mpf
    t = i(1)/10000
    x = sample(t)
    reverse_sigma = i(5)/4 - t
    reverse_value = -1 - ctx.log(reverse_sigma)
    for mu in (i(1)/200, 3*t/4):
        advantage = fixed_value(x, mu, i(1)/100, 1, ctx) - reverse_value
        assert advantage.a > 0
        print('Small-gap witness advantage over all-free reverse face', advantage)
    x = [i(0), i(100), i(100), i(100)]
    advantage = fixed_value(x, i(0), i(1), 1, ctx) + 25
    assert advantage.a > 0
    print('Held-scale witness advantage', advantage)


def transfer():
    mp.mp.dps = 70
    one = mp.mpf(1)
    r = mp.findroot(lambda r: high([0]*4+[one]*5+[r], one)[0] -
                    high([0]*4+[one]*5+[r], 0)[0],
                    (mp.mpf('1.45'), mp.mpf('1.47')))
    data = [0]*4+[one]*5+[r]
    v, _, _, score = high(data, 0)
    assert abs(v-high(data, one)[0]) < mp.mpf('1e-60')
    assert high(data, mp.mpf('-.001'))[0] > v
    print('Fréchet finite–Gumbel equality, r and GEV seam score',
          mp.nstr(r, 40), mp.nstr(score, 25))
    data = [0, one, mp.mpf('2.650471807225057')]
    v = high(data, -1)[0]
    print('Fréchet finite tie vicinity, GEV face and positive boundary',
          mp.nstr(v, 25), mp.nstr(high(data, one)[0], 25))


if __name__ == '__main__':
    endpoint_witnesses()
    transfer()
