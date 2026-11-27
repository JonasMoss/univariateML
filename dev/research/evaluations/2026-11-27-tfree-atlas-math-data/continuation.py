"""Independent 75-digit numerical solutions near the proved crossings."""
import mpmath as mp

import crossing as joint
import scalar_crossing as scalar

mp.mp.dps = 75
counts = joint.COUNTS


def avg(values):
    return mp.fdot(counts, values)/joint.TOTAL


def radii(t, start, delta):
    return [mp.mpf(str(a))+t*mp.mpf(str(d)) for a, d in zip(start, delta)]


def stats(v, scale, r, delta):
    q = [x*x/(scale*scale) for x in r]
    log = avg([mp.log1p(x/(v-delta)) for x in q])
    spread = avg([x/(v-delta+x) for x in q])
    value = (mp.loggamma((v+1)/2)-mp.loggamma(v/2)
             -mp.log((v-delta)*mp.pi)/2-mp.log(scale)-(v+1)*log/2)
    shape = (mp.digamma((v+1)/2)-mp.digamma(v/2)-1/(v-delta)
             -log+(v+1)*spread/(v-delta))
    return value, shape, (v+1)*spread-1


def joint_eq(t, v, u, s, b):
    r = radii(t, joint.LEFT, joint.DELTA)
    lv, dv, sv = stats(v, s, r, 0)
    lu, du, su = stats(u, b, r, 0)
    return lv-lu, dv, du, sv, su


def scalar_eq(t, v, u):
    r = radii(t, scalar.LEFT, scalar.DIRECTION)
    lv, dv, _ = stats(v, 1, r, 2)
    lu, du, _ = stats(u, 1, r, 2)
    return lv-lu, dv, du


def solve(equations, initial):
    solution = mp.findroot(equations, tuple(map(mp.mpf, initial)))
    assert max(map(abs, equations(*solution))) < mp.mpf('1e-65')
    return [mp.nstr(x, 25) for x in solution]


if __name__ == '__main__':
    print('joint (t, v_left, v_right, s_left, s_right)',
          solve(joint_eq, ['.73', '5.6', '150', '.35', '.42']))
    print('scalar (t, v_left, v_right)',
          solve(scalar_eq, ['.05', '8.5', '142']))
