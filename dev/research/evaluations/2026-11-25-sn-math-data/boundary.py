"""Numerical checks for the two-piece Q enclosure and the skew-t endpoint bounds."""
import mpmath as mp

from profile import student_cdf, student_log_base

mp.mp.dps = 70


def side_sums(x, c):
    return (sum((v-c)**2 for v in x if v < c),
            sum((v-c)**2 for v in x if v > c))


def check_q():
    x = list(map(mp.mpf, [-2]+[-1]*10+[1]*10+[2]))
    count = 0
    for j in range(32):
        a, b = -2+mp.mpf(j)/8, -2+mp.mpf(j+1)/8
        al, bl = side_sums(x, a)
        ar, br = side_sums(x, b)
        lower = mp.cbrt(al)+mp.cbrt(br)
        upper = mp.cbrt(ar)+mp.cbrt(bl)
        for k in range(9):
            t = a+(b-a)*k/8
            at, bt = side_sums(x, t)
            assert lower <= mp.cbrt(at)+mp.cbrt(bt) <= upper
            count += 1
    print('snorm Q interval checks:', count)


def check_st_endpoint():
    x = [mp.mpf(v) for v in (0, 1, 3)]
    omega = mp.mpf(1)
    k = 1
    for nu in map(mp.mpf, ('0.5', '1', '4', '20')):
        m = nu+1
        # Nuisance test box has xi in [-1/2, 1]. Fixed-nu derivative bound.
        H = mp.mpf(1)
        K = len(x)*m/(2*mp.sqrt(nu))
        delta = min(H, k*mp.log(2)/(2*K))
        q_delta = delta*mp.sqrt(m/(nu+delta**2))
        cutoff = mp.exp((K*H+1)/k)/q_delta
        face = sum(mp.log(2/omega)+student_log_base(v/omega, nu) for v in x)
        for a in (cutoff, 2*cutoff):
            for j in range(-4, 9):
                xi = mp.mpf(j)/8
                z = [(v-xi)/omega for v in x]
                ll = sum(mp.log(2/omega)+student_log_base(v, nu)
                         + mp.log(student_cdf(a*v*mp.sqrt(m/(nu+v*v)), m))
                         for v in z)
                assert ll < face
    print('st moving-endpoint tail checks: 104 points')


def check_st_collapse():
    # e=0, xi=0 held, with two zero residuals and two off-tie signs.
    nu = mp.mpf(1)
    m = nu+1
    alpha = mp.mpf('0.3')
    c = mp.exp(student_log_base(mp.mpf(0), nu))

    def extended(u):
        value = c**2
        for d in (-1, 2):
            q = d*mp.sqrt(m/(nu*u+d*d))
            value *= (2*c*(u+d*d/nu)**(-m/2)*student_cdf(alpha*q, m))
        return value

    for w in map(mp.mpf, ('1', '0.1', '0.001', '1e-8', '1e-12')):
        actual = c**2/w**2
        for d in (-1, 2):
            z = d/w
            q = z*mp.sqrt(m/(nu+z*z))
            actual *= 2*mp.exp(student_log_base(z, nu))*student_cdf(alpha*q, m)/w
        assert abs(actual/extended(w*w)-1) < mp.mpf('1e-64')
    assert abs(extended(mp.mpf('1e-24'))/extended(0)-1) < mp.mpf('1e-22')
    print('st held-location equality collapse checks: 5 positive scales and the limit')


if __name__ == '__main__':
    check_q()
    check_st_endpoint()
    check_st_collapse()
