"""Exact arithmetic for the independent TASK-455 examples."""

from fractions import Fraction as F


def normal_margin():
    # A density ratio >= exp(-1/100) > 99/100 bounds the variance
    # via Var(Z) = min_c E[(Z-c)^2]. Here Z=X^2 on [-1,1].
    curvature = F(99, 100) * (F(1, 5) - F(1, 3)**2)
    assert curvature > F(2, 25)
    for second_moment in (
        (1 + 3 * F(349525, 1048576)**2) / 4,
        F(57735, 100000)**2,
    ):
        delta = F(1, 3) - second_moment
        assert 0 < delta < curvature / 100
        gain_upper = delta**2 / (2 * curvature)
        assert gain_upper < F(1, 10**8)
        print(f"Normal inward score {delta}, gain per observation < {gain_upper}")

    assert (1 + 3 * F(1, 3)**2) / 4 == F(1, 3)
    mean = F(0 + 2, 2)
    variance = F((0 - mean)**2 + (2 - mean)**2, 2)
    assert variance == mean**2


def zeta_domain_comparison():
    # At s=1 on [2,3], mean log count is below the log mean of (2,3)
    # since the model probability at 3 is 2/5 < 1/2. This also gives
    # an outward score for (2,3,3,3), with observed fraction 3/4.
    p2, p3 = F(3, 5), F(2, 5)
    assert p3 < F(1, 2) < F(3, 4)
    # On [1,3] write r=-s, y=(2/3)^r, z=(1/3)^r.
    # r<=0: p1>=p2>=p3, so p2*p3 <=1/9.
    # 0<=r<=2: y<=1, z>=1/9 and y/(1+y+1/9)^2 increases to y=1.
    # r>=2: y<=4/9, z>=0 and y/(1+y)^2 increases to y=1.
    bounds = F(1, 9), 1 / (2 + F(1, 9))**2, F(4, 9) / (1 + F(4, 9))**2
    assert max(bounds) < p2 * p3 == F(6, 25)
    # At r=1 on [1,3], the four-observation likelihood beats the
    # entire tight-window branch, including its excluded harmonic face.
    assert F(2, 6) * F(3, 6)**3 == F(1, 24) > p2 * p3**3
    print("Zeta branches win in opposite directions for the two exact samples")


def other_examples():
    # On [1,4], with density proportional to x^(-3/2).
    mass = 2 * (1 - F(1, 2))
    mean = 2 * (2 - 1) / mass
    mean_reciprocal = F(2, 3) * (1 - F(1, 8)) / mass
    assert mean == 2 < F(5, 2)
    assert mean_reciprocal == F(7, 12) < F(1, 2) * (1 + F(1, 4))
    assert F(1, 4) * (1 - F(1, 4)) == F(3, 4) * (1 - F(3, 4))

    assert F(2 + 2 * 3, 2 + 3) == F(8, 5) < F(5, 3)
    for size in (2, 3, 8, 128):
        prob = F(1, size)
        assert (size - 1) * prob / (2 * (1 - prob)) == F(1, 2)
    print("Inverse Gaussian and beta corners, NB carrier and binomial odds passed")


if __name__ == "__main__":
    normal_margin()
    zeta_domain_comparison()
    other_examples()
