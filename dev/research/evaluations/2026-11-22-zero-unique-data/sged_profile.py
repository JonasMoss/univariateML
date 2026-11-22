"""Check the interval inequalities in the centered SGED proof."""
from mpmath import iv, mp

iv.dps = mp.dps = 70
nu = iv.mpf(9)/4
delta = nu - 2
g1, g2, g3 = (iv.gamma(i/nu) for i in (1, 2, 3))
a = 1 - g2**2/(g1*g3)
c = (g2/g1)**nu
target = (4*a - 1)/(2*nu*c)


def h(u):
    return u**delta*(2-u)*(1-u+a*u*u)


checks = {
    'K > 0': target,
    'H(1) > K': a - target,
    'Q(1) < 0': 1 - (delta+1)*a,
    'Q second derivative positive': 2*(delta+2)-2*a*(delta+5),
    'root above .0008': target - h(iv.mpf('0.0008')),
    'root below .001': h(iv.mpf('0.001')) - target,
}
for label, difference in checks.items():
    assert difference.a > 0, label
    print(label, 'lower margin', mp.nstr(mp.mpf(difference.a), 12))
