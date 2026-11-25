"""Conditional global comparison for the GB2 golden sample."""
import argparse
from collections import Counter
from hashlib import sha256
import json
import math
from pathlib import Path
import sys
from time import perf_counter

import mpmath as mp
import numpy as np

HERE = Path(__file__).resolve().parent
PRIOR = HERE.parent / "2026-11-25-gbshape-math-data"
sys.path.insert(0, str(PRIOR))
import gb2_cover as prior
from gamma_cover import fit_side, standardized

LD = np.longdouble
# Absolute guards for dimensionless scores and values; scaled for intervals.
PAD = LD("2e-12")
SHIFTS = np.arange(16, dtype=LD)
STIRLING = (LD(1)/12, -LD(1)/360, LD(1)/1260, -LD(1)/1680,
            LD(1)/1188, -LD(691)/360360)
TRIGAMMA = (LD(1)/6, -LD(1)/30, LD(1)/42, -LD(1)/30,
            LD(5)/66, -LD(691)/2730)


class Interval:
    """Array or scalar interval with a practical guard on computed endpoints."""
    def __init__(self, lo, hi=None):
        self.lo = np.asarray(lo, dtype=LD)
        self.hi = self.lo if hi is None else np.asarray(hi, dtype=LD)

    def __add__(x, y):
        y = interval(y)
        return Interval(x.lo + y.lo, x.hi + y.hi)

    __radd__ = __add__

    def __neg__(x):
        return Interval(-x.hi, -x.lo)

    def __sub__(x, y):
        return x + -interval(y)

    def __rsub__(x, y):
        return interval(y) + -x

    def __mul__(x, y):
        y = interval(y)
        terms = (x.lo*y.lo, x.lo*y.hi, x.hi*y.lo, x.hi*y.hi)
        return Interval(np.minimum.reduce(terms), np.maximum.reduce(terms))

    __rmul__ = __mul__

    def reciprocal(x):
        assert np.all(x.lo*x.hi > 0)
        return Interval(1/x.hi, 1/x.lo)

    def __truediv__(x, y):
        return x*interval(y).reciprocal()

    def __rtruediv__(x, y):
        return interval(y)*x.reciprocal()

    def avg(x):
        return Interval(x.lo.mean(), x.hi.mean())

    def padded(x):
        return Interval(x.lo-PAD*(1+abs(x.lo)), x.hi+PAD*(1+abs(x.hi)))

    def signed(x):
        return x.lo > PAD or x.hi < -PAD


def interval(x):
    return x if isinstance(x, Interval) else Interval(x)


def softplus(x):
    return np.logaddexp(LD(0), x)


def sigmoid(x):
    return np.exp(-softplus(-x))


def kernel(z, r, derivative=False):
    """Evaluate on positive rays, reflecting negative rays."""
    right = z >= 0
    r, complement = np.where(right, r[0], 1-r[1]), np.where(right, 1-r[0], r[1])
    x = abs(z)
    v = -np.expm1(-x)
    denominator = r + complement*np.exp(-x)
    if derivative:
        return np.where(right, v/denominator, -v/denominator)
    result = np.empty_like(x)
    # Finite cells have either interior ratios or an exact upper endpoint.
    high = r >= LD(".5")
    h = complement[high]*v[high]
    correction = np.divide(np.log1p(-h), complement[high],
                           out=-v[high].copy(), where=complement[high] != 0)
    result[high] = (x[high]+correction)/r[high]
    low = ~high
    # log(1+r(expm1(x))) = x + log(r+(1-r)exp(-x)).
    # Use log1p directly at modest x to avoid subtractive cancellation.
    modest = low & (x < 40)
    s = r[modest]*np.expm1(x[modest])
    correction = np.divide(np.log1p(s), r[modest],
                           out=np.expm1(x[modest]), where=r[modest] != 0)
    result[modest] = (correction-x[modest])/complement[modest]
    large = low & ~modest
    s = r[large]
    positive = s > 0
    xl = x[large]
    answer = np.empty_like(xl)
    # Near r=0 use the actual product r*expm1(x). If it is large,
    # use softplus in log-product coordinates.
    if np.any(positive):
        xr = xl[positive]
        sr = s[positive]
        log_product = np.log(sr)+xr+np.log(-np.expm1(-xr))
        val = softplus(log_product)/sr
        answer[positive] = (val-xr)/(1-sr)
    answer[~positive] = np.expm1(xl[~positive])-xl[~positive]
    result[large] = answer
    return result


def curvature(z, r):
    nonnegative = z >= 0
    fraction = np.where(nonnegative,r[0],1-r[1])
    complement = np.where(nonnegative,1-r[0],r[1])
    v = np.exp(-abs(z))
    denominator = fraction+complement*v
    # This ordering also works when squaring a tiny denominator underflows.
    return (v/denominator)/denominator


def stirling_remainder(x):
    """Stirling remainder R(x), and -x R'(x), with x >= 16 after shift."""
    n = max(0, 16-int(x)) if x < 16 else 0
    t = x+n
    v = 1/t
    # The first omitted Bernoulli terms bound the truncation for x >= 16.
    value = sum(b*v**(2*j+1) for j,b in enumerate(STIRLING))
    gradient = x*sum((2*j+1)*b*v**(2*j+2) for j,b in enumerate(STIRLING))
    if n:
        t = x+SHIFTS[:n]
        if x < 1:
            h = np.concatenate(([np.log1p(x)-np.log(x)], np.log1p(1/t[1:])))
        else:
            h = np.log1p(1/t)
        value += ((t+LD(".5"))*h-1).sum()
        gradient += x*((t+LD(".5"))/(t*(t+1))-h).sum()
    return value, gradient


def digamma(x):
    return np.log(x)-(LD(".5")+stirling_remainder(x)[1])/x


def trigamma(x):
    n = max(0, 16-int(x)) if x < 16 else 0
    v = 1/(x+n)
    return (v+v*v/2+sum(b*v**(2*j+3) for j,b in enumerate(TRIGAMMA))
            +np.sum((1/(x+SHIFTS[:n]))**2))


def height(k, r):
    p, q = k/(1-r), k/r
    a, ap = stirling_remainder(p)
    b, bp = stirling_remainder(q)
    c, cp = stirling_remainder(p+q)
    return np.log(k/(2*PI))/2-a-b+c, (LD(".5")+ap+bp-cp)/k


PI = LD("3.141592653589793238462643383279502884")


class Cover:
    def __init__(self, y, level, center, radius):
        self.y = np.array(y, dtype=LD)
        self.left, self.right = min(self.y), max(self.y)
        self.span = self.right-self.left
        self.level = level
        knots = np.unique(self.y)
        distance = knots[:,None]-self.y[None,:]
        self.left_losses = np.maximum(distance,0).mean(axis=1)
        self.right_losses = np.maximum(-distance,0).mean(axis=1)
        sqrt_r = np.sqrt(self.right_losses)
        self.laplace_ratios = sqrt_r/(sqrt_r+np.sqrt(self.left_losses))
        self.calls = Counter()
        self.reasons = Counter()
        self.min_margin = {}
        self.max_depth = 0
        self.face_leaves = Counter()
        self.face_minimum_width = {}
        self.winning_boxes = []
        self.radius = radius
        while self.radius > LD("1e-8"):
            self.slab = [Interval(v-self.radius,v+self.radius) for v in center]
            self.slab_pivots = self.concavity_pivots(self.slab)
            if self.slab_pivots:
                break
            self.radius /= 2
        assert self.slab_pivots

    def loss(self, a, m, r, derivative=False):
        self.calls["kernel_rows"] += len(self.y)
        return kernel(a*(self.y-m), r, derivative)

    def mode(self, a, r, iterations=24):
        lo, hi = self.left, self.right
        mid = (lo+hi)/2
        for _ in range(iterations):
            derivative = self.loss(a, mid, r, True)
            score = derivative.mean()
            if abs(score) <= LD("1e-13")*(1+abs(derivative).mean()):
                break
            if score > 0:
                lo = mid
            else:
                hi = mid
            self.calls["curvature_rows"] += len(self.y)
            second = curvature(a*(self.y-mid),r).mean()
            proposal = mid+score/(a*second)
            mid = proposal if lo < proposal < hi else (lo+hi)/2
        return mid

    def loss_bound(self, a, r, s):
        choices = (s, r)
        m = self.mode(a, choices)
        loss = self.loss(a, m, choices).mean()
        derivative = self.loss(a, m, choices, True)
        slope = -a*derivative.mean()
        # Whole-range tangent. In particular the mode proposal can be inexact.
        return (loss + min(slope*(self.left-m), slope*(self.right-m))
                - PAD*(1+loss+a*self.span*abs(derivative).mean()))

    def accept(self, name, margin):
        self.reasons[name] += 1
        self.min_margin[name] = min(self.min_margin.get(name, math.inf), float(margin))
        return True

    def value(self, a, b, r, s, d):
        if d <= 0:
            return False
        normal = -np.log(4*PI*np.exp(LD(1))*d)/2
        upper = min(np.log(b)+normal, np.log(a)+normal+(b-a)*self.span/2,
                    np.log(a/d)-1)
        if upper+PAD < self.level:
            return self.accept("elementary_value", self.level-upper)
        if 0 < r and s < 1 and a >= 8:
            fractions = np.clip(self.laplace_ratios,r,s)
            mixed = np.min(self.left_losses/(1-fractions)+self.right_losses/fractions)
            upper = -1-np.log(mixed)+(PI/d)**2/(6*r*(1-s))
            if upper+PAD < self.level:
                return self.accept("two_small_shapes", self.level-upper)
        midpoint = (r+s)/2
        kmin = 2*PI*np.exp(2*self.level)/b**2
        error = (s-r)*min(1, 1/(8*kmin))
        if midpoint <= 0 or midpoint >= 1:
            return False
        lo, hi = LD(0), 1/d
        k = LD(".65")/d
        for _ in range(12):
            self.calls["height"] += 1
            value, slope = height(k, midpoint)
            slope -= d
            tangent = value-k*d+max(slope*(lo-k), slope*(hi-k))
            upper = np.log(b)+tangent+error
            if upper+PAD*(1+abs(upper)) < self.level:
                return self.accept("normalizer_tangent", self.level-upper)
            if slope > PAD*(1+d):
                lo = k
            elif slope < -PAD*(1+d):
                hi = k
            else:
                break
            if hi-lo < LD(".001")*k or (s-r) > LD(".25"):
                break
            k = (lo+hi)/2
        return False

    def location(self, a, b, r, s):
        mid = (a+b)/2
        halfwidth = (b-a)/2
        ends = []
        for ratio in (s, r):
            m = self.mode(mid, (ratio, ratio))
            delta = LD("1e-7")
            lo = max(self.left, m-delta)
            hi = min(self.right, m+delta)
            at_lo = self.loss(mid, lo, (ratio, ratio), True)
            at_hi = self.loss(mid, hi, (ratio, ratio), True)
            if at_lo.mean() <= PAD*(1+abs(at_lo).mean()):
                lo = self.left
            if at_hi.mean() >= -PAD*(1+abs(at_hi).mean()):
                hi = self.right
            # t'=weighted mean of y; |t''| <= span^2/4.
            rate_bound = max(abs(self.left),abs(self.right))
            root_error = mid*max(m-lo,hi-m)
            if root_error < LD(".1"):
                self.calls["curvature_rows"] += len(self.y)
                weights = curvature(mid*(self.y-m),(ratio,ratio))
                slope = (weights*self.y).sum()/weights.sum()
                slope_error = self.span*np.expm1(2*root_error)
                rate_bound = min(rate_bound,abs(slope)+slope_error
                                 +self.span**2*halfwidth/8)
            drift = halfwidth*rate_bound
            ends.append((mid*lo-drift,mid*hi+drift,m))
        return Interval(ends[0][0],ends[1][1]).padded(), (ends[0][2]+ends[1][2])/2

    def concentration(self, r, s, d, upper):
        # h(x)=-xR'(x) decreases, by Binet's Laplace formula after change of variable.
        lo, hi = LD(".5")/upper, 1/d
        for side in (0, 1):
            left, right = lo, hi
            for _ in range(14):
                k = np.sqrt(left*right)
                p = Interval(k/(1-r), k/(1-s))
                q = Interval(k/s, k/r)
                mid = min(s,max(r,LD(".5")))
                total = Interval(k/(mid*(1-mid)),k/min(r*(1-r),s*(1-s)))
                h = []
                for x in (p,q,total):
                    self.calls["remainder"] += 2
                    h.append(Interval(stirling_remainder(x.hi)[1], stirling_remainder(x.lo)[1]))
                score = (LD(".5")+h[0]+h[1]-h[2])/k-Interval(d,upper)
                guard = PAD*(1+upper+1/k)
                score = Interval(score.lo-guard,score.hi+guard)
                if (score.lo if side == 0 else score.hi) > 0:
                    left = k
                else:
                    right = k
            if side == 0:
                lo = left
            else:
                hi = right
        return Interval(lo,hi).padded()

    def hessian(self, box):
        self.calls["hessian_rows"] += len(self.y)
        a, c, p, q = box
        z = a*self.y-c
        w = Interval(sigmoid(z.lo), sigmoid(z.hi)).padded()
        # logistic variance is even and decreases in |z|.
        far = np.maximum(abs(z.lo),abs(z.hi))
        near = np.maximum(0,np.maximum(z.lo,-z.hi))
        v = Interval(np.exp(-far)/(1+np.exp(-far))**2,
                     np.exp(-near)/(1+np.exp(-near))**2).padded()
        def trig(x):
            self.calls["trigamma"] += 2
            lo, hi = trigamma(x.hi), trigamma(x.lo)
            return Interval(lo,hi).padded()

        def diagonal(x,y):
            self.calls["trigamma"] += 4
            lo = trigamma(x.lo+y.hi)-trigamma(x.lo)
            hi = trigamma(x.hi+y.lo)-trigamma(x.hi)
            return Interval(lo,hi).padded()

        total = p+q
        h = trig(total)
        av = (total*v*self.y).avg()
        return [[-1/a/a-(total*v*self.y**2).avg(),av,((1-w)*self.y).avg(),-(w*self.y).avg()],
                [av,-(total*v).avg(),-(1-w).avg(),w.avg()],
                [((1-w)*self.y).avg(),-(1-w).avg(),diagonal(p,q),h],
                [-(w*self.y).avg(),w.avg(),h,diagonal(q,p)]]

    def score(self, box):
        self.calls["score_rows"] += len(self.y)
        a,c,p,q = box
        z = a*self.y-c
        w = sigmoid(z)
        v = p-(p+q)*w
        f = [digamma(x) for x in (p, q, p+q)]
        self.calls["digamma"] += 3
        return [1/a+(self.y*v).mean(),-v.mean(),
                f[2]-f[0]-softplus(-z).mean(),f[2]-f[1]-softplus(z).mean()]

    def taylor_value(self, center, steps, hessian, score):
        a,c,p,q = center
        ratio, k = p/(p+q), p*q/(p+q)
        mode = (c+np.log(p/q))/a
        self.calls["height"] += 1
        height_value, _ = height(k,ratio)
        value = np.log(a)+height_value-k*self.loss(a,mode,(ratio,ratio)).mean()
        radii = [max(abs(step.lo),abs(step.hi)) for step in steps]
        for i,radius in enumerate(radii):
            g = abs(score[i])+PAD
            h = min(0,hessian[i][i].hi+PAD)
            displacement = min(radius,-g/h) if h < 0 else radius
            value += g*displacement+h*displacement**2/2
            for j in range(i):
                cross = hessian[i][j]
                value += (PAD+max(abs(cross.lo),abs(cross.hi)))*radius*radii[j]
        if value+PAD*(1+abs(value)) < self.level:
            return self.accept("taylor_value",self.level-value)
        return False

    def in_slab(self, box, reason):
        if self.slab is not None and all(bound.lo < x.lo and x.hi < bound.hi
                                         for x,bound in zip(box,self.slab)):
            self.winning_boxes.append([[float(x.lo),float(x.hi)] for x in box])
            margin = min(min(x.lo-bound.lo,bound.hi-x.hi) for x,bound in zip(box,self.slab))
            return self.accept(reason, margin)
        return False

    def entropy_score(self, box, k):
        a,c,p,q = box
        logistic_coordinate = a*self.y-c
        near = np.maximum(0,np.maximum(logistic_coordinate.lo,-logistic_coordinate.hi))
        far = np.maximum(abs(logistic_coordinate.lo),abs(logistic_coordinate.hi))
        def entropy(x):
            e = np.exp(-x)
            return np.log1p(e)+x*e/(1+e)

        self.calls["entropy_rows"] += len(self.y)
        self.calls["trigamma"] += 3
        lower = 2*k.lo*trigamma(1+p.hi+q.hi)
        upper = k.hi*(trigamma(1+p.lo)+trigamma(1+q.lo))
        comparison = (Interval(entropy(far).mean(),entropy(near).mean())
                      -Interval(lower,upper)).padded()
        if comparison.signed():
            return self.accept("entropy_score",min(abs(comparison.lo),abs(comparison.hi)))
        return False

    def scores(self, a, b, r, s, d):
        if not (LD(".002") <= r < s <= LD(".998")):
            return False
        t, mode = self.location(a,b,r,s)
        d_hi = self.loss(b,mode,(r,s)).mean()
        k = self.concentration(r,s,d,d_hi+PAD*(1+d_hi))
        power = Interval(a,b)
        ratio = Interval(r,s)
        logit = Interval(np.log(r)-np.log1p(-r),np.log(s)-np.log1p(-s)).padded()
        box = [power,t-logit,k/(1-ratio),k/ratio]
        if np.any(k.lo <= 0):
            return False
        if a >= 4 and self.entropy_score(box,k):
            return True
        if self.in_slab(box,"winning_slab"):
            return True
        center = [(x.lo+x.hi)/2 for x in box]
        steps = [x-v for x,v in zip(box,center)]
        hessian = self.hessian(box)
        residual = self.score(center)
        if self.taylor_value(center,steps,hessian,residual):
            return True
        for f,row in zip(residual,hessian):
            bound = (f+sum(x*h for x,h in zip(steps,row))).padded()
            if bound.signed():
                return self.accept("mean_value_score", min(abs(bound.lo),abs(bound.hi)))
        # Any fixed linear combination of a zero score is zero. Use an
        # approximate inverse only to propose combinations for this check.
        midpoint = np.array([[(h.lo+h.hi)/2 for h in row] for row in hessian], dtype=float)
        if abs(np.linalg.det(midpoint)) > 1e-16:
            inverse = np.linalg.inv(midpoint).astype(LD)
            contracted = []
            for i,row in enumerate(inverse):
                f = sum(v*g for v,g in zip(row,residual))
                mixed = [sum(h*v for h,v in zip(col,row)) for col in zip(*hessian)]
                bound = (f+sum(x*h for x,h in zip(steps,mixed))).padded()
                extra = PAD*(1+sum(abs(row)))
                if bound.lo > extra or bound.hi < -extra:
                    return self.accept("preconditioned_score",min(abs(bound.lo),abs(bound.hi))-extra)
                mixed[i] = mixed[i]-1
                enclosure = (center[i]-f-sum(x*h for x,h in zip(steps,mixed))).padded()
                lo,hi = max(box[i].lo,enclosure.lo-extra), min(box[i].hi,enclosure.hi+extra)
                if lo > hi:
                    return self.accept("stationary_contraction",lo-hi)
                contracted.append(Interval(lo,hi))
            p,q = contracted[2:]
            projected = Interval(p.lo/(p.lo+q.hi),p.hi/(p.hi+q.lo)).padded()
            if projected.lo > s or projected.hi < r:
                return self.accept("contracted_ratio",max(projected.lo-s,r-projected.hi))
            if self.in_slab(contracted,"contracted_winning_slab"):
                return True
        return False

    def concavity_pivots(self, box):
        H = [[-v for v in row] for row in self.hessian(box)]
        pivots = []
        for i in range(4):
            pivot = H[i][i].padded()
            if pivot.lo <= 0:
                return []
            pivots.append(str(pivot.lo))
            for j in range(i+1,4):
                for k in range(i+1,4):
                    H[j][k] = H[j][k]-H[j][i]*H[i][k]/pivot
        return pivots

    def run(self, stack, limit):
        unresolved = []
        while stack and self.calls["cells"] < limit:
            a,b,r,s,depth = stack.pop()
            self.calls["cells"] += 1
            self.max_depth = max(self.max_depth,depth)
            d = self.loss_bound(a,r,s)
            if (self.value(a,b,r,s,d)
                    or (b/a < LD("1.16") and s-r < LD(".06") and d > 0
                        and self.scores(a,b,r,s,d))):
                for face, touches in (("direct",r == 0),("reciprocal",s == 1)):
                    if touches:
                        self.face_leaves[face] += 1
                        self.face_minimum_width[face] = min(
                            self.face_minimum_width.get(face,LD(1)),s-r)
                continue
            if depth >= 65:
                unresolved.append(tuple(map(str,(a,b,r,s))))
                continue
            if min(np.log(b/a),(b-a)*self.span) > 3*(s-r) and (a<32 or s-r<LD(".005")):
                mid=np.sqrt(a*b)
                assert a < mid < b
                stack.extend(((a,mid,r,s,depth+1),(mid,b,r,s,depth+1)))
            else:
                mid=(r+s)/2
                assert r < mid < s
                stack.extend(((a,b,r,mid,depth+1),(a,b,mid,s,depth+1)))
            if self.calls["cells"] % 1000 == 0:
                print(self.calls["cells"], (a,b,r,s,depth), self.reasons,
                      "unresolved", len(unresolved), flush=True)
        unresolved += [tuple(map(str,box[:4])) for box in stack]
        return unresolved


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--digits",type=int,default=65)
    parser.add_argument("--limit",type=int,default=100000)
    parser.add_argument("--binary64",action="store_true",
                        help="treat the fixture observations as exact binary64 numbers")
    parser.add_argument("--output",type=Path,default=HERE/"results.json")
    args=parser.parse_args()
    start=perf_counter()
    mp.mp.dps=args.digits
    np.seterr(over="raise", invalid="raise", divide="raise")
    fixture=Path(prior.__file__).resolve().parents[4]/"univariateML-cpp/include/cppunivariate/distributions/gb2/golden.json"
    # Preserve the decimal fixture tokens in the multiprecision replay.
    parse_float = (lambda token: mp.mpf(float(token))) if args.binary64 else mp.mpf
    source=json.loads(fixture.read_text(),parse_float=parse_float)
    logs=[mp.log(x) for x in source["data"]]
    y,sd=standardized(logs)
    y_float=[LD(mp.nstr(x,30)) for x in y]
    p,q=source["estimates"]["shape3"],source["estimates"]["shape1"]
    a=source["estimates"]["shape2"]*sd
    c=a*(mp.log(source["estimates"]["scale"])-prior.mean(logs))/sd
    r=p/(p+q)
    k=p*q/(p+q)
    witness=mp.log(a)+prior.height(k,r)[0]-k*prior.mean(
        [prior.kernel(a*x-c-mp.log(p/q),r)[0] for x in y])
    level=witness-mp.mpf("1e-15")
    exterior_level=witness-mp.mpf("1e-5")
    low,low_upper=prior.normal_tail(y,exterior_level)
    high=prior.large_tail(y,exterior_level)
    local=prior.local_ball(y,(a,c,p,q))
    normal=-mp.log(2*mp.pi*mp.e)/2
    faces={side:fit_side(z,exterior_level-normal)
           for side,z in (("direct",y),("reciprocal",[-v for v in y]))}
    center=list(map(lambda x:LD(mp.nstr(x,30)),local["root"]))
    cover=Cover(y_float,LD(mp.nstr(level,30)),center,LD(".002"))
    seeds=[LD(mp.nstr(low,30))-PAD,*map(LD,(".25",".5","1","2","4","8","16","32","64")),
           LD(mp.nstr(high["cutoff"],30))+PAD]
    stack=[(a,b,LD(0),LD(1),0) for a,b in zip(seeds,seeds[1:])]
    assert cover.slab_pivots
    max_displacement = seeds[-1]*cover.span
    assert np.log(np.finfo(LD).max) > max_displacement+1
    exterior_seconds = perf_counter()-start
    print("Exterior checks passed, starting finite cover",flush=True)
    unresolved=cover.run(stack,args.limit)
    finite_seconds = perf_counter()-start-exterior_seconds
    power, intercept, lower, upper = local["root"]
    fitted = dict(shape1=upper,shape2=power/sd,shape3=lower,
                  scale=mp.exp(prior.mean(logs)+sd*intercept/power))
    deviation = {key: abs(value/source["estimates"][key]-1) for key,value in fitted.items()}
    root_radius = (local["residual"]+mp.mpf("1e-40"))/mp.mpf("1e-9")
    logscale_radius = sd*root_radius*(1+abs(intercept)/power)/(power-root_radius)
    extra = dict(shape1=root_radius/upper,shape2=root_radius/power,
                 shape3=root_radius/lower,scale=mp.expm1(logscale_radius))
    relative_to_stored = {key: deviation[key]+extra[key]*(1+deviation[key])
                          for key in deviation}
    coefficient_error = {key: v/(1-v) for key,v in relative_to_stored.items()}
    assert max(coefficient_error.values()) < mp.mpf("1e-10")
    result=dict(precision=args.digits, finite_kernel_precision_bits=np.finfo(LD).nmant+1,
                fixture_arithmetic="binary64" if args.binary64 else "decimal",
                fixture_sha256=sha256(fixture.read_bytes()).hexdigest(),
                fitted=fitted, stored_relative_error_upper=coefficient_error,
                root_radius_using_residual=root_radius,
                witness=witness, level=level, low_cutoff=low, low_upper=low_upper,
                large_power=high, faces=faces, local=local, cells=dict(cover.calls),
                leaf_counts=cover.reasons, minimum_margins=cover.min_margin,
                max_depth=cover.max_depth, winning_boxes=len(cover.winning_boxes),
                slab_radius=cover.radius, slab_pivot_lower=cover.slab_pivots,
                power_seeds=list(map(str,seeds)), max_kernel_displacement=str(max_displacement),
                face_leaves=cover.face_leaves, face_minimum_width=cover.face_minimum_width,
                original_log_likelihood=len(y)*(witness-mp.log(sd))-mp.fsum(logs),
                exterior_seconds=exterior_seconds, finite_seconds=finite_seconds,
                unresolved=unresolved, wall_seconds=perf_counter()-start)
    args.output.write_text(json.dumps(result,indent=2,default=lambda x:mp.nstr(x,30))+"\n")
    print({key:result[key] for key in ("cells","leaf_counts","winning_boxes","wall_seconds")},
          "unresolved",len(unresolved),flush=True)
    assert not unresolved and cover.winning_boxes
    assert cover.calls["cells"] == 2*sum(cover.reasons.values())-(len(seeds)-1)


if __name__ == "__main__":
    main()
