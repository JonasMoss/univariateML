"""Numerical replays for TASK-406. The report supplies the proofs."""
import mpmath as mp


def negative_density(a, z):
    if z == 0:
        return mp.power(2, -a - 2)
    lower = mp.log(2)
    upper = mp.log(a*z + mp.power(2, a))/a
    for _ in range(300):
        y = (lower + upper)/2
        q = -mp.expm1(-y)
        if mp.exp(a*y) - q**(-a) < a*z:
            lower = y
        else:
            upper = y
    p = mp.exp(-(lower + upper)/2)
    return 1/(p**(-a-1) + (1-p)**(-a-1))


def check_scores():
    count = 0
    for a in [mp.mpf(1)/8, mp.mpf(1)/2, mp.mpf(1), mp.mpf(2), mp.mpf(7)]:
        previous = mp.inf
        for j in range(1, 50):
            p = mp.mpf(j)/100
            q = 1-p
            t = p/q
            z = (p**(-a) - q**(-a))/a
            d = p**(-a-1) + q**(-a-1)
            direct = z*(a+1)*(p**(-a-2) - q**(-a-2))/d**2
            factored = (a+1)/a*(1-t**a)*(1-t**(a+2))/(1+t**(a+1))**2
            assert mp.almosteq(direct, factored)
            assert 0 < factored < previous
            previous = factored
            count += 1
    return count


def check_collar():
    count = 0
    for k, m in [(4, 1), (2, 1), (1, 1), (2, 3), (1, 3), (1, 6)]:
        a = mp.mpf(m)/k
        r = 1+1/a
        modal = mp.power(2, -a-2)
        alpha = negative_density(a, 1/a)/modal
        away = [mp.mpf(j) for j in range(1, m+1)]
        face = modal**k*a**(-r*m)*mp.fprod(d**(-r) for d in away)
        eta = (1-alpha**(mp.mpf(k)/(r*m)))/2
        for delta_fraction in [-1, -mp.mpf(1)/2, 0, mp.mpf(1)/2, 1]:
            delta = delta_fraction*eta
            for exponent in [-10, -4, -1, 0, 2]:
                sigma = mp.power(10, exponent)
                center = negative_density(a, abs(delta)/sigma)/sigma
                tail = [negative_density(a, abs(x-delta)/sigma)/sigma for x in away]
                assert center**k*mp.fprod(tail) < face
                count += 1
    return count


def main():
    mp.mp.dps = 80
    print(f"80 decimal digits: {check_scores()} elasticity identity and order checks")
    print(f"80 decimal digits: {check_collar()} critical collapse collar checks")
    t = mp.power(2, -20)
    witness = negative_density(2, 1)*negative_density(2, 1/t)/t**2
    assert witness > mp.mpf(1)/4
    print(f"Held location transition: likelihood at (shape, scale)=(-2, 2^-20) is {mp.nstr(witness, 20)} > 1/4")


if __name__ == "__main__":
    main()
