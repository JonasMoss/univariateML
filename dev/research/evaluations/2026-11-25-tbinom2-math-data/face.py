"""Evaluate the moment envelope and a latent-integral upper bound."""
import mpmath as mp

mp.mp.dps = 80


def model_gain(rate, mu, size):
    lo, hi = mp.mpf(0), mu
    for _ in range(220):
        mid = (lo + hi) / 2
        u = -size * mp.log1p(-mid / size)
        if mid / (-mp.expm1(-u)) < mu:
            lo = mid
        else:
            hi = mid
    v = (lo + hi) / 2
    u = -size * mp.log1p(-v / size)
    return (mu * (mp.log(v / rate) - mp.log1p(-v / size))
            - (u - rate) - mp.log((-mp.expm1(-u)) / (-mp.expm1(-rate))))


def cost(size, k):
    return ((k - 1) * mp.log(size) - mp.loggamma(size)
            + mp.loggamma(size - k + 1)) / (k - 1)


def latent_gain(rate, size):
    h = mp.mpf(size - 1)

    def integrand(w):
        return mp.exp(w-rate) * (w + (h-w)*mp.log1p(-w/h))

    return mp.quad(integrand, [0, rate]) / (-mp.expm1(-rate))


def main():
    rates = set(map(mp.mpf, ('1e-9', '1e-6', '.001', '.01', '.1', '.3', '.5',
                              '1', '2', '10', '30', '100', '300', '1000', '10000')))
    for k in range(1, 41):
        rates.update(mp.mpf(k) + mp.mpf(offset) for offset in
                     ('-.999', '-.501', '-.001', '0', '.001', '.499', '.999'))
    max_excess = -mp.inf
    max_scaled_excess = -mp.inf
    max_latent = -mp.inf
    tested = latent_checks = 0
    for rate in sorted(rates):
        mu = rate / (-mp.expm1(-rate))
        nu = rate * mu / (mu - 1)
        k = int(mp.floor(nu))
        r = nu - k
        minimum = int(mp.ceil(nu))
        for size in sorted({minimum + j for j in (0, 1, 2, 5, 10, 40, 100)}
                           | {10 * minimum, 1000 * minimum, 10**8 * minimum}):
            penalty = (mu - 1) * ((1-r)*cost(size, k)
                                   + (r*cost(size, k+1) if r else 0))
            excess = model_gain(rate, mu, size) - penalty
            assert excess < 0, (rate, size, excess)
            tested += 1
            max_excess = max(max_excess, excess)
            max_scaled_excess = max(max_scaled_excess, size**2 * excess / (mu - 1)**2)
            if rate <= 40 and size in (minimum, minimum + 2, 1000 * minimum):
                latent = latent_gain(rate, size) - penalty
                if latent > max_latent:
                    max_latent = latent
                    latent_witness = (rate, size)
                if rate <= 1:
                    assert latent < 0, (rate, size, latent)
                latent_checks += 1
    print(tested, "80-digit moment-envelope checks; largest excess",
          mp.nstr(max_excess, 20), "largest scaled excess", mp.nstr(max_scaled_excess, 20))
    print(latent_checks, "latent bound checks; largest excess", mp.nstr(max_latent, 20),
          "at rate and size", mp.nstr(latent_witness[0]), latent_witness[1])


if __name__ == "__main__":
    main()
