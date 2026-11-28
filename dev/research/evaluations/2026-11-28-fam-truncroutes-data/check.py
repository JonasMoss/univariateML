"""Replay the exact inputs and compute the normal coefficient reference."""

from decimal import Decimal, localcontext
from fractions import Fraction
from math import expm1, log1p, sqrt


def normal_scale(gap):
    with localcontext() as context:
        context.prec = 75
        target = Decimal(1) / 3 - Decimal(gap.numerator) / gap.denominator

        def integral(k, power):
            # Uniform moment of x^power exp(-k*x*x), for even power.
            term = Decimal(1)
            value = term / (power + 1)
            for j in range(1, 20):
                term *= -k / j
                value += term / (2*j + power + 1)
            return value

        lo, hi = Decimal(0), Decimal(".000002")
        assert integral(hi, 2) < target * integral(hi, 0)
        for _ in range(180):
            mid = (lo + hi) / 2
            if integral(mid, 2) > target * integral(mid, 0):
                lo = mid
            else:
                hi = mid
        return 1 / (lo + hi).sqrt()


def centered_moments(sample):
    data = tuple(map(Decimal, sample))
    mean = sum(data) / len(data)
    return mean, sum((x-mean)**2 for x in data) / len(data)


def pinned_face_signs():
    """Recreate the binary64 quantile grids from the existing C++ tests."""
    with localcontext() as context:
        context.prec = 60
        n = 200
        grid = [(-log1p(-(i+0.5)/n))**1.1 for i in range(n)]
        mean = sum(x/n for x in grid)
        variance = sum((x-mean)**2/n for x in grid)
        for ratio in (0.9999, 1.0+1e-9):
            target = sqrt(ratio)*mean
            sample = [5+mean+(x-mean)*target/sqrt(variance) for x in grid]
            m, v = centered_moments(sample)
            gap = (m-5)**2-v
            assert (gap > 0) == (ratio < 1)
            print(f"Half-line variance gap, ratio {ratio}: {gap:.12g}")

        n = 400
        grid = [-1+log1p((i+0.5)/n*expm1(0.7*4))/0.7 for i in range(n)]
        mean = sum(x/n for x in grid)

        def face_moments(t):
            # Moments on the unit interval, reflected at its upper endpoint.
            w = (-t).exp()
            return 1-1/t+w/(1-w), 1/t**2-w/(1-w)**2

        for shrink in (1.0, 1.0001):
            sample = [mean+shrink*(x-mean) for x in grid]
            m, v = centered_moments(sample)
            left, right = Decimal(2), Decimal(3)
            for _ in range(150):
                t = (left+right)/2
                if face_moments(t)[0] < (m+1)/4:
                    left = t
                else:
                    right = t
            gap = 16*face_moments((left+right)/2)[1]-v
            assert (gap > 0) == (shrink == 1)
            print(f"Finite-window variance gap, shrink {shrink}: {gap:.12g}")


def main():
    d = Fraction(349525, 1048576)
    gap = Fraction(1, 3) - (1 + 3*d*d)/4
    assert gap == Fraction(2097151, 13194139533312)
    assert 0 < gap < Fraction(8, 10000)
    assert gap**2 / (2 * Fraction(99, 100) * Fraction(4, 45)) < Fraction(144, 10**15)
    print("Normal uniform-to-empirical moment gap:", gap)
    print("Normal scale:", normal_scale(gap))

    weights = (2, 3)
    mean = Fraction(weights[0] + 2*weights[1], sum(weights))
    assert mean == Fraction(8, 5) < Fraction(5, 3)
    print("NB likelihood at the face:", Fraction(2, 5)*Fraction(3, 5)**2)

    for size in (2, 3, 17, 10**8):
        prob = Fraction(1, size)
        assert (size-1)*prob/(2*(1-prob)) == Fraction(1, 2)
    print("Binomial conditional likelihood:", Fraction(2, 3)**2 * Fraction(1, 3))
    pinned_face_signs()


if __name__ == "__main__":
    main()
