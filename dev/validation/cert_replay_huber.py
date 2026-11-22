"""Numerical epsilon cover with supporting planes for the Huber nuisance fit."""

from bisect import bisect_left, bisect_right
from functools import lru_cache
from itertools import accumulate

from mpmath import mp


class Profile:
    def __init__(self, data, mu, sigma):
        origin = data[len(data)//2]
        unit = max(abs(v-origin) for v in data)
        if mu is not None:
            unit = max(unit, abs(mu-origin))
        if not unit:
            unit = sigma
        self.x = sorted((v-origin)/unit for v in data)
        self.mu = None if mu is None else (mu-origin)/unit
        self.sigma = None if sigma is None else sigma/unit
        self.n = len(data)
        self.shift = self.n*mp.log(unit)
        self.origin, self.unit = origin, unit
        self.prefix = list(accumulate(self.x, initial=mp.mpf(0)))
        self.squares = list(accumulate((x*x for x in self.x), initial=mp.mpf(0)))
        self.normal_mu = self.prefix[-1]/self.n if self.mu is None else self.mu
        ss = mp.fsum((v-self.normal_mu)**2 for v in self.x)
        self.normal_sigma = mp.sqrt(ss/self.n) if self.sigma is None else self.sigma
        self.normal = -self.n*(mp.log(self.normal_sigma)+mp.log(2*mp.pi)/2)-ss/(2*self.normal_sigma**2)-self.shift
        self.lap_mu = self.x[self.n//2] if self.mu is None else self.mu
        self.lap_scale = mp.fsum(abs(x-self.lap_mu) for x in self.x)/self.n
        self.laplace = (-self.n*(mp.log(2*self.lap_scale)+1)-self.shift
                        if self.sigma is None else mp.ninf)

    def sums(self, m, s, c):
        """Clipped location score, log-scale score before subtracting n, and loss."""
        left = bisect_left(self.x, m-c*s)
        right = bisect_right(self.x, m+c*s)
        count = right-left
        tails = self.n-count
        central = self.prefix[right]-self.prefix[left]
        first = (central-count*m)/s
        second = (self.squares[right]-self.squares[left]-2*m*central+count*m*m)/s**2
        absolute_tails = ((2*left+count-self.n)*m + self.prefix[-1]
                          -self.prefix[right]-self.prefix[left])/s
        h = first+c*(self.n-right-left)
        g = second+c*absolute_tails
        loss = second/2+c*absolute_tails-tails*c*c/2
        return h, g, loss, count

    def location(self, s, c):
        if self.mu is not None:
            return self.mu
        lo, hi = self.x[0], self.x[-1]
        m = (lo+hi)/2
        for _ in range(160):
            h, _, _, count = self.sums(m, s, c)
            if abs(h) < mp.mpf('1e-22')*c*self.n:
                return m
            if h > 0:
                lo = m
            else:
                hi = m
            candidate = m+h*s/count if count else lo
            m = candidate if lo < candidate < hi else (lo+hi)/2
        raise ValueError('location score did not close')

    def likelihood(self, m, s, c):
        z = mp.sqrt(2*mp.pi)*mp.erf(c/mp.sqrt(2))+2*mp.exp(-c*c/2)/c
        return -self.n*(mp.log(s)+mp.log(z))-self.sums(m, s, c)[2]-self.shift

    @lru_cache(maxsize=8192)
    def point(self, c):
        s = self.sigma
        if s is None:
            lo, hi = self.lap_scale*c/2, self.normal_sigma
            for _ in range(128):
                if self.sums(self.location(lo, c), lo, c)[1] > self.n:
                    break
                lo /= 2
            else:
                raise ValueError('scale score not bracketed below')
            for _ in range(128):
                if self.sums(self.location(hi, c), hi, c)[1] < self.n:
                    break
                hi *= 2
            else:
                raise ValueError('scale score not bracketed above')
            for _ in range(96):
                s = (lo+hi)/2
                g = self.sums(self.location(s, c), s, c)[1]
                if abs(g-self.n) < mp.mpf('1e-20')*self.n:
                    break
                if g > self.n:
                    lo = s
                else:
                    hi = s
        m = self.location(s, c)
        h, g, _, _ = self.sums(m, s, c)
        lower = self.likelihood(m, s, c)
        shift = (max(h*(self.x[0]-m), h*(self.x[-1]-m))/s
                 if self.mu is None else 0)
        gap = shift
        if self.sigma is None:
            d = (g-shift)/self.n-1
            if d <= -1:
                raise ValueError('supporting plane has nonpositive inverse-scale penalty')
            gap += self.n*(d-mp.log1p(d))
        return lower, lower+gap

    def laplace_proved(self, c):
        left = bisect_left(self.x, self.lap_mu)
        right = bisect_right(self.x, self.lap_mu)
        ties = right-left
        if not ties:
            return False
        d = 0 if self.mu is not None else self.n-left-right
        deficit = ties-mp.mpf(d*d)/ties
        nearest = min(abs(v-self.lap_mu) for v in self.x if v != self.lap_mu)
        return (0 < c*c < min(1, 3*deficit/self.n)
                and c*c*self.lap_scale*(1+mp.mpf(abs(d))/ties) < nearest)

    def cover(self, incumbent, target, budget, laplace_limit):
        # Special-function evaluations are numerical. A small guard prevents
        # a near equality at the working precision from deciding a verdict.
        guard = mp.eps**mp.mpf('0.4')*(1+abs(target))
        high = max(abs(x-self.normal_mu) for x in self.x)/self.normal_sigma
        if high == 0:
            return ('holds' if self.normal <= target else 'fails',
                    f'zero residual Gaussian tail {mp.nstr(self.normal, 20)}')
        high = (high+mp.mpf('1e-6'))*(1+mp.mpf('1e-6'))
        low = min(mp.mpf('0.01'), high/2)
        if self.sigma is None:
            if laplace_limit and not self.laplace_proved(low):
                return 'open', 'median-cluster Laplace tail did not close at the original seam'
            if self.laplace_proved(low):
                if not laplace_limit and self.laplace >= target:
                    return 'open', 'Laplace tail overlaps target'
            else:
                for _ in range(128):
                    if self.laplace + self.n*low**2/2 < target:
                        break
                    low /= 2
                else:
                    return 'open', 'small-epsilon Laplace bound overlaps target'
        else:
            low = min(low, mp.exp(incumbent/self.n+self.shift/self.n+
                                  mp.log(2*self.sigma)-2))
            tail = self.n*(mp.log(low)-mp.log(2*self.sigma)+low**2/2)-self.shift
            if tail >= target:
                return 'open', 'small-epsilon held-scale bound overlaps target'
        pending = [(low, high)]
        count = 0
        while pending and count < budget:
            count += 1
            a, b = pending.pop()
            la, ua = self.point(a)
            lb, ub = self.point(b)
            t, witness = (a, la) if la > lb else (b, lb)
            if witness > target+guard:
                return 'fails', f'epsilon {mp.nstr(t, 18)}, likelihood {mp.nstr(witness, 20)}'
            if max(ua, ub)+self.n*(1+2/a**2)*(b-a)**2/8 < target-guard:
                continue
            mid = mp.sqrt(a*b)
            if not a < mid < b:
                return 'open', 'cell collapsed at working precision'
            pending.extend(((a, mid), (mid, b)))
        if not pending:
            return 'holds', f'{count} cells closed'
        return 'open', f'{count} cells checked without closing the cover'
