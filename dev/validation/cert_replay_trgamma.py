"""Numerical replay of transformed-gamma convex-conjugate covers."""

from functools import lru_cache

from mpmath import mp


def solve_shape(target, free_scale):
    """Invert log(k)-digamma(k) or digamma(k) in log shape."""
    if free_scale:
        start = mp.log((1 + mp.sqrt(1 + 2 * target)) / (4 * target))

        def score(z):
            k = mp.exp(z)
            return z - mp.digamma(k) - target

        def derivative(z):
            k = mp.exp(z)
            return 1 - k * mp.polygamma(1, k)
    else:
        start = target if target > 0 else -mp.log(1 - target)

        def score(z):
            return mp.digamma(mp.exp(z)) - target

        def derivative(z):
            k = mp.exp(z)
            return k * mp.polygamma(1, k)

    z = start
    for _ in range(100):
        slope = derivative(z)
        residual = score(z)
        step = residual / slope
        # At the normal seam log(k) and digamma(k) nearly coincide.
        # Stop when cancellation limits further improvement in the residual.
        precision_floor = 32*mp.eps*(1+abs(z)+abs(target))/abs(slope)
        if abs(step) <= max(mp.eps**mp.mpf('0.6'), precision_floor):
            return mp.exp(z)
        z -= max(-2, min(2, step))
    raise ValueError('inverse digamma did not converge')


class Profile:
    def __init__(self, logs, logscale):
        self.n = len(logs)
        self.free_scale = logscale is None
        self.mean = mp.fsum(logs) / self.n
        residuals = [v - (self.mean if self.free_scale else logscale) for v in logs]
        self.unit = (mp.sqrt(mp.fsum(v*v for v in residuals) / self.n)
                     if self.free_scale else max(map(abs, residuals)))
        self.x = [v / self.unit for v in residuals]
        self.maximum = max(self.x)
        self.m = mp.fsum(self.x) / self.n
        self.constant = mp.log(2 * mp.pi) / 2
        self.offset = self.constant + mp.mpf('0.5') if self.free_scale else -mp.log(self.unit)
        if self.free_scale:
            self.face = max(mp.mpf(0), self.constant - mp.mpf('0.5') - mp.log(self.maximum))
        elif self.maximum <= 0 and self.m < 0:
            self.face = mp.log(-1/self.m) - 1 + self.offset - self.x.count(0)/mp.mpf(self.n)
        else:
            self.face = mp.ninf

    def moments(self, t):
        weights = [mp.exp(t*x) for x in self.x]
        total = mp.fsum(weights)
        slope = mp.fsum(x*v for x, v in zip(self.x, weights))
        square = mp.fsum(x*x*v for x, v in zip(self.x, weights))
        if self.free_scale:
            k1 = slope / total
            return mp.log(total/self.n), k1, square/total - k1*k1
        return total/self.n, slope/self.n, square/self.n

    @lru_cache(maxsize=8192)
    def point(self, t):
        e, e1, variance = self.moments(t)
        if self.free_scale:
            k = solve_shape(e, True)
            conjugate = k*(mp.log(k)-1-e) - mp.loggamma(k)
            return mp.log(t) + conjugate + self.offset, 1/t-k*e1, conjugate, e, e1, variance, k
        k = solve_shape(t*self.m, False)
        conjugate = k*t*self.m - mp.loggamma(k)
        return mp.log(t) + conjugate - e + self.offset, 1/t+k*self.m-e1, conjugate, e, e1, variance, k

    def returned(self, t, shape, logscale):
        p = t/self.unit
        if self.free_scale:
            e, _, _ = self.moments(t)
            a = e + p*(self.mean-logscale) - mp.log(shape)
            loss = shape*(mp.expm1(a)-a)
            return (mp.log(t) + shape*(mp.log(shape)-1-e) - mp.loggamma(shape)
                    + self.offset - loss)
        e, _, _ = self.moments(t)
        return mp.log(t) + shape*t*self.m - mp.loggamma(shape) - e + self.offset

    def curvature(self, lo, hi):
        left, right = self.point(lo), self.point(hi)
        if self.free_scale:
            k = left[-1]
            vmin = left[-2]*mp.exp(-(hi-lo)*(self.maximum-min(self.x)))
            return -1/hi**2 + right[4]**2/(mp.polygamma(1, k)-1/k) - right[-1]*vmin
        vmax = max(left[-1], right[-1])
        vmin = mp.fsum(x*x*mp.exp(x*(lo if x > 0 else hi)) for x in self.x)/self.n
        return -1/hi**2 + self.m**2/mp.polygamma(1, vmax) - vmin

    def upper(self, lo, hi):
        left, right = self.point(lo), self.point(hi)
        if self.free_scale:
            slope = (right[2]-left[2])/(right[3]-left[3])
            if slope >= 0:
                raise ValueError('log-gamma conjugate secant is unresolved')
            dl, dr = 1/lo+slope*left[4], 1/hi+slope*right[4]
        else:
            slope = (right[2]-left[2])/(hi-lo)
            dl, dr = 1/lo-left[4]+slope, 1/hi-right[4]+slope
        if dl <= 0 or dr >= 0:
            return max(left[0], right[0])
        offset = (right[0]-left[0]-dr*(hi-lo))/(dl-dr)
        return left[0] + dl*max(0, min(hi-lo, offset))

    def normal_seam(self, t):
        h = t * max(map(abs, self.x))
        if t*t*mp.exp(h)/2 > mp.mpf('0.5'):
            return mp.inf
        skew = mp.fsum(x**3 for x in self.x)/self.n
        fourth = mp.fsum(x**4 for x in self.x)/self.n
        c = fourth*mp.exp(-h)/12-mp.exp(2*h)/4
        d = 1-abs(skew)*t/3-abs(c)*t*t
        if d <= 0:
            return mp.inf
        q = (-c/2+(abs(skew)/3+abs(c)*t)**2/(4*d*d)
             -d/(6*(1+mp.sqrt(1+2*t*t*mp.exp(h)/3)))+t**4*mp.exp(3*h)/360)
        a = -skew/6
        vertex = -a/(2*q) if q < 0 else -1
        return max(0, a*t+q*t*t, -a*a/(4*q) if 0 < vertex < t else 0)

    def tails(self, target):
        if self.free_scale:
            lo = mp.mpf('0.05')/max(map(abs, self.x))
            for _ in range(256):
                if self.normal_seam(lo) < target:
                    hi = 4*(self.n+mp.log(self.n)+10)/self.maximum
                    # K >= t * max(x) - log(n) > n. Since
                    # log(k) - psi(k) < 1/k, the maximizing k is below 1/n.
                    # log Gamma(1+k) >= -Euler*k then bounds the conjugate
                    # by max_k[log(k) - k*t*max(x)] = -1-log(t*max(x)).
                    power = self.constant-mp.mpf('0.5')-mp.log(self.maximum)
                    if power < target:
                        return lo, hi
                    break
                lo /= 2
            raise ValueError('normal seam or power-tail bound did not close')

        lo = mp.mpf('1e-6')
        for _ in range(256):
            s = max(0, lo*self.m)
            k = solve_shape(s, False)
            if mp.log(lo) + k*s - mp.loggamma(k) + self.offset < target:
                break
            lo /= 2
        else:
            raise ValueError('small-power tail did not close')
        hi = mp.mpf(1)
        for _ in range(256):
            if self.m < 0 and -hi*self.m > mp.euler:
                bound = (mp.log(-1/self.m)-1+self.offset
                         -mp.log1p(mp.euler/(hi*self.m)))
                bound -= (self.x.count(0) if self.maximum <= 0 else mp.exp(hi*self.maximum))/mp.mpf(self.n)
                if bound < target:
                    return lo, hi
            if self.m >= 0 and self.maximum > self.m and hi*(self.maximum-self.m) > mp.log(2*self.n):
                e = mp.exp(hi*self.maximum)/self.n
                bound = (mp.log(hi)+mp.exp(hi*self.m)+1+(hi*self.m+mp.log(2))/2
                         -self.constant-e+self.offset)
                if self.maximum*e/2 > self.m/2+1/hi and bound < target:
                    return lo, hi
            hi *= 2
        raise ValueError('large-power tail did not close')

    def cover(self, lo, hi, target, budget, basin):
        # Special-function evaluations are numerical. A small guard prevents
        # a near equality at the working precision from deciding a verdict.
        guard = mp.eps**mp.mpf('0.4')*(1+abs(target))
        if basin:
            ends = [lo, *basin, hi]
            pending = [(ends[0], ends[1]), (ends[2], ends[3])]
        else:
            pending = [(lo, hi)]
        count = 0
        while pending and count < budget:
            count += 1
            a, b = pending.pop()
            left, sl, *_ = self.point(a)
            right, sr, *_ = self.point(b)
            for t, value in ((a, left), (b, right)):
                if value > target+guard:
                    return 'fails', f'profile({mp.nstr(t, 18)}) = {mp.nstr(value, 20)}'
            rise = max(0, self.curvature(a, b))*(b-a)
            if (sr > rise and right < target-guard) or (sl < -rise and left < target-guard):
                continue
            if self.upper(a, b) < target-guard:
                continue
            mid = mp.sqrt(a*b)
            if not a < mid < b:
                return 'open', 'cell collapsed at working precision'
            pending.extend(((a, mid), (mid, b)))
        if not pending:
            return 'holds', f'{count} cells closed'
        return 'open', f'{count} cells checked without closing the cover'
