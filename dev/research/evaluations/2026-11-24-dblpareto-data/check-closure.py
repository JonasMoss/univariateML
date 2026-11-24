"""Pointwise checks of the four declared limit edges in observation coordinates."""
import math

import mpmath as mp


def double_pareto(x, b, left, right):
    power = left - 1 if x <= b else -right - 1
    return left * right / (b * (left + right)) * (x / b) ** power


def gb2(x, b, power, left, right):
    t = power * mp.log(x / b)
    p, q = left / power, right / power
    softplus = max(t, 0) + mp.log1p(mp.exp(-abs(t)))
    return power / (x * mp.beta(p, q)) * mp.exp(p*t - (p+q)*softplus)


def main():
    with mp.workdps(80):
        scale, a, b, offset = (mp.mpf(v) for v in ('2', '1.7', '3.1', '-0.25'))
        probes = [scale/3, scale, scale*2]
        for power in map(mp.mpf, (10, 100, 1000, 10000)):
            err_gb2 = max(abs(gb2(x, scale, power, a, b) / double_pareto(x, scale, a, b) - 1)
                          for x in probes)
            err_shifted = max(abs(gb2(x-offset, scale, power, a, b) /
                                  double_pareto(x-offset, scale, a, b) - 1)
                              for x in [v+offset for v in probes])
            err_pareto = max(abs(double_pareto(x, scale, power, b) /
                                 (b/scale*(x/scale)**(-b-1)) - 1) for x in probes[1:])
            err_power = max(abs(double_pareto(x, scale, a, power) /
                                (a/scale*(x/scale)**(a-1)) - 1) for x in probes[:2])
            values = tuple(float(v) for v in (err_gb2, err_shifted, err_pareto, err_power))
            assert all(math.isfinite(v) and v < 4 / power for v in values)
            print(int(power), *(f'{v:.9g}' for v in values), sep='\t')


if __name__ == '__main__':
    print('power\tgb2\tfpareto\tpareto\tpower_face')
    main()
