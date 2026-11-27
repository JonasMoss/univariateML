"""Numerical replay of the sample-only tests. All log likelihoods are per datum."""
import csv
from functools import lru_cache
from math import e, exp, lgamma, log, pi, sqrt
from pathlib import Path

import numpy as np

OUT = Path(__file__).parent
NORMAL = -0.5 * (1 + log(2 * pi))
MARGIN = 1e-9


@lru_cache(maxsize=None)
def normalizer(q):
    return lgamma((q + 1) / 2) - lgamma(q / 2) - log(q * pi) / 2


def student(x, q, center, scale):
    residual = (x - center) / scale
    return normalizer(q) - log(scale) - (q + 1) / 2 * np.log1p(residual**2/q).mean()


def normal_score(x):
    return NORMAL - log(x.std())


def student_two_upper(x):
    """Fixed two: geodesic tangent on the order-statistic rectangle."""
    n = len(x)
    spread = np.max([(x[k:] - x[:-k]).min() / 2 * sqrt(1-1.5*k/n)
                     for k in range(1, (2*n-1)//3+1)])
    if spread <= 0:
        return float("inf")
    height_max = sqrt(3)*(x[-1] - x[0])
    height_min = sqrt(2)*spread
    center = np.quantile(x, [0.25, 0.5, 0.75])
    scale = np.full_like(center, x.std())
    for _ in range(128):
        weight = 3/(2+((x-center[:, None])/scale[:, None])**2)
        center = (weight*x).sum(axis=1)/weight.sum(axis=1)
        scale = np.sqrt((weight*(x-center[:, None])**2).mean(axis=1))
    height = sqrt(2)*scale
    res = x - center[..., None]
    den = res**2+height[..., None]**2
    val = -log(2) - np.log(height) - 1.5*np.log1p((res/height[..., None])**2).mean(axis=-1)
    gradient = np.hypot(3*height*(res/den).mean(axis=-1),
                         1-3*(res**2/den).mean(axis=-1))
    distance = np.zeros_like(height)
    for c in (x[0], x[-1]):
        for a in (height_min, height_max):
            distance = np.maximum(distance, np.arccosh(1+((c-center)**2+(a-height)**2)/(2*a*height)))
    return (val+gradient*distance).min()


def symmetric_tests(x, q0):
    """Exact tests in real arithmetic; simulation data have no exact ties."""
    z = x - (x[0]+x[-1])/2
    if not np.array_equal(z, -z[::-1]):
        return "gap"
    h = np.count_nonzero(z) / len(z)
    d = z[-1]
    variance = np.mean(z**2)
    if d*d <= 2*variance:
        return "normal"
    if q0 == 2 and np.all((z == 0) | (abs(z) == d)):
        if 3*h <= 1:
            return "collapse" if 3*h == 1 else "unbounded"
        r = 3*h-1
        lower = -log(2*d) + r*log(r)/2 - 3*h/2*log(1+r)
        normal = NORMAL-log(d*sqrt(h))
        if lower > normal+MARGIN:
            return "two"
        if normal > lower+MARGIN:
            return "normal"
        return "two+normal"
    return "gap"


def critical_student_escape(x, q0):
    values, counts = np.unique(x, return_counts=True)
    n = len(x)
    for atom in values[counts*(q0+1) == q0*n]:
        distances = abs(x[x != atom]-atom)
        normalized_far = distances.max()/exp(np.log(distances).mean())
        a0 = 2**(-q0-1)
        power = (q0+1)/n
        short = exp(normalizer(n))*sqrt(n)*max(
            a0**(power-1)/normalized_far**power, 1/normalized_far)
        long = sqrt(2*n/pi)/normalized_far
        if max(short, long) < exp(normalizer(q0))*sqrt(q0)-MARGIN:
            return True
    return False


def symmetric_student(x, q0):
    _, counts = np.unique(x, return_counts=True)
    if q0*(len(x)-counts.max()) < counts.max():
        return "unbounded"
    escape = symmetric_tests(x, q0)
    if escape != "gap":
        return escape
    if critical_student_escape(x, q0):
        return "collapse"
    z = (x-x.mean())/x.std()
    kurtosis = np.mean(z**4)
    if kurtosis <= 3+MARGIN:
        return "gap"
    if q0 == 1:
        atoms, counts = np.unique(x, return_counts=True)
        for atom in atoms[counts*2 == len(x)]:
            collapse = -log(pi)-np.log(abs(x[x != atom]-atom)).mean()
            if normal_score(x) < collapse-MARGIN:
                return "gap"
        return "attain"
    if 3*counts.max() == 2*len(x):
        atom = x[np.argmin(abs(x-np.median(x)))]
        upper = -log(2)-np.log(abs(x[x != atom]-atom)).mean()
    else:
        upper = student_two_upper(x)
    if normal_score(x) > upper+MARGIN:
        return "attain"
    # The normal inward slope beats N. A different witness may beat D.
    center = np.quantile(x, [0.25, 0.5, 0.75, 1/3, 2/3])
    scale = x.std()*np.geomspace(0.03125, 2, 21)
    for q in (2.125, 2.5, 3, 5, 8, 16, 64):
        residual = (x-center[:, None, None])/scale[:, None]
        values = normalizer(q)-np.log(scale)-(q+1)/2*np.log1p(residual**2/q).mean(axis=-1)
        if values.max() > upper+MARGIN:
            return "attain"
    return "gap"


@lru_cache(maxsize=None)
def ged_peak(p):
    return -(1+log(p))/p-lgamma(1+1/p)


def ged_grid(x):
    centers = np.unique(np.r_[x[1:-1], (x[:-1]+x[1:])/2])
    left = np.maximum(centers[:, None]-x, 0)
    right = np.maximum(x-centers[:, None], 0)
    finite = float("-inf")
    for p in (1, 1.25, 1.5, 2, 3, 4, 6, 8, 12, 24, 48, 96, 192):
        # At an interior join the profiled widths are both strictly positive.
        total = ((left**p).mean(axis=-1)**(1/(p+1)) +
                 (right**p).mean(axis=-1)**(1/(p+1)))
        finite = max(finite, ged_peak(p)-(1+1/p)*log(total.min()))
    return finite


def ged_half_upper(x, threshold):
    """Use monotone power means and the extreme-count tail bound."""
    if threshold <= MARGIN:
        return False
    distances = np.stack((x, 1-x))
    n = len(x)
    h = min(np.mean(x == 0), np.mean(x == 1))
    # For p >= 1/h the global log bound is strictly negative.
    intervals = [(1., 1/h)]
    for _ in range(384):
        if not intervals:
            return True
        a, b = intervals.pop()
        bound = ged_peak(b)-np.log((distances**a).mean(axis=-1)).min()/a
        if bound < threshold-MARGIN:
            continue
        mid = sqrt(a*b)
        lower = ged_peak(mid)-np.log((distances**mid).mean(axis=-1)).min()/mid
        if lower >= threshold-MARGIN:
            return False
        intervals.extend(((a, mid), (mid, b)))
    return False


def ged_uniform_coarsen(x):
    """Sufficient uniform test, including an explicit strict tail."""
    n = len(x)
    h = min(np.mean(x == 0), np.mean(x == 1))
    if h >= 1/e:
        return True
    # Jensen within each contiguous rank group.
    splits = np.arange(1, n)
    partial = np.cumsum(x)
    left = partial[:-1]/splits
    right = (partial[-1]-partial[:-1])/(n-splits)
    gap = right-left
    small = np.minimum(splits, n-splits)/n
    tail = 1/h
    gain = np.maximum(-1-np.log(small), ged_peak(tail)-np.log(small)/tail)
    return np.any(gain < np.log(gap)-MARGIN)


def skew_ged(x):
    x = (x-x[0])/(x[-1]-x[0])
    values, counts = np.unique(x, return_counts=True)
    if len(values) == 2:
        return "uniform" if counts.min()/len(x) >= 1/e else "half_laplace"
    if ged_uniform_coarsen(x):
        return "uniform"
    if x.mean() == 0.5:
        knots = np.unique(x)
        stop_loss = np.maximum(x-knots[:, None], 0).mean(axis=-1)
        if np.all(stop_loss >= (1-knots)**2/2):
            return "uniform"
    witness = ged_grid(x)
    return "attain" if ged_half_upper(x, witness) else "gap"


def skew_normal_upper(x):
    """Fixed join intervals with exact profiling of the two widths."""
    cuts = np.unique(np.r_[x, np.linspace(0, 1, 257)])
    left = np.maximum(cuts[:-1, None]-x, 0)
    right = np.maximum(x-cuts[1:, None], 0)
    bound = (left**2).mean(axis=1)**(1/3)+(right**2).mean(axis=1)**(1/3)
    return log(sqrt(2/pi))-0.5-1.5*log(bound.min()) if np.all(bound > 0) else float("inf")


def side_log_widths(left, right, q):
    """Propose two widths by Newton steps on a concave log-width objective."""
    ba = np.mean(left**2, axis=1)**(1/3)
    bb = np.mean(right**2, axis=1)**(1/3)
    a = np.log(np.maximum(ba*np.sqrt(ba+bb), 1e-12))
    b = np.log(np.maximum(bb*np.sqrt(ba+bb), 1e-12))
    for _ in range(14):
        fraction = 1/(1+np.exp(b-a))
        wa = (left/np.exp(a[:, None]))**2
        wb = (right/np.exp(b[:, None]))**2
        u, v = wa/(q+wa), wb/(q+wb)
        ga = (q+1)*u.mean(axis=1)-fraction
        gb = (q+1)*v.mean(axis=1)-(1-fraction)
        cross = fraction*(1-fraction)
        haa = 2*(q+1)*(u*(1-u)).mean(axis=1)+cross+1e-12
        hbb = 2*(q+1)*(v*(1-v)).mean(axis=1)+cross+1e-12
        det = haa*hbb-cross**2
        a += np.clip((hbb*ga+cross*gb)/det, -1, 1)
        b += np.clip((cross*ga+haa*gb)/det, -1, 1)
    return a, b


def entropy(p):
    with np.errstate(divide="ignore", invalid="ignore"):
        return -np.where(p>0, p*np.log(p), 0)-np.where(p<1, (1-p)*np.log1p(-p), 0)


def feasible_weights(u, active, mass):
    """An affine correction preserves [0,1] (exactly in real arithmetic)."""
    total = u.sum(axis=-1, keepdims=True)
    room = active.sum(axis=-1, keepdims=True)-total
    with np.errstate(divide="ignore", invalid="ignore"):
        return np.where(total >= mass, u*mass/total, u+(active-u)*(mass-total)/room)


def skew_student_witness(x):
    centers = np.unique(np.r_[x[1:-1], (x[:-1]+x[1:])/2])
    left = np.maximum(centers[:, None]-x, 0)
    right = np.maximum(x-centers[:, None], 0)
    best = float("-inf")
    for q in (2.25, 3., 5., 10., 25., 80.):
        a, b = side_log_widths(left, right, q)
        val = log(2)+normalizer(q)-np.logaddexp(a, b)
        penalty = np.log1p((left/np.exp(a[:, None]))**2/q)
        penalty += np.log1p((right/np.exp(b[:, None]))**2/q)
        best = max(best, (val-(q+1)/2*penalty.mean(axis=1)).max())
    return best


def skew_two_excluded(x, target):
    cuts = np.unique(np.r_[x, np.linspace(0, 1, 17)])
    boxes = np.stack((cuts[:-1], cuts[1:]), axis=1)
    for _ in range(12):
        if len(boxes) > 1024:
            return False
        left = np.maximum(boxes[:, :1]-x, 0)
        right = np.maximum(x-boxes[:, 1:], 0)
        d = left+right
        use = np.count_nonzero(d, axis=1) > len(x)/3
        a, b = side_log_widths(left[use], right[use], 2)
        z = (left[use]/np.exp(a[:, None])+right[use]/np.exp(b[:, None]))**2/2
        p = feasible_weights(z/(1+z), z>0, len(x)/3)
        w = np.clip(3*(p*(left[use]>0)).mean(axis=1), 0., 1.)
        slog = np.log(np.where(d[use]>0, d[use], 1))
        val = log(2)+normalizer(2)-entropy(w)-1.5*(p*(2*slog-log(2))+entropy(p)).mean(axis=1)
        use[use] = val < target-MARGIN
        boxes = boxes[~use]
        if not len(boxes):
            return True
        mid = boxes.mean(axis=1)
        boxes = np.concatenate((np.stack((boxes[:, 0], mid), axis=1),
                                np.stack((mid, boxes[:, 1]), axis=1)))
    return False


def half_student_excluded(x, target):
    distances = np.stack((x, 1-x))
    square = distances**2
    n = len(x)
    knots = 2*np.exp(np.linspace(0, log(256), 49))
    bounds = np.stack((knots, [*knots[1:], np.inf]), axis=1)
    for _ in range(10):
        if len(bounds) > 384:
            return False
        qa, qb = bounds.T[:, :, None]
        factor = qa*(1+1/qb)
        if np.any(factor*np.count_nonzero(square, axis=1)/n <= 1):
            return False
        z = square[None, :, :]/qa[..., None]
        lo = np.full((len(bounds), 2), -40.)
        hi = np.zeros_like(lo)
        while np.any(factor*(z/(z+np.exp(2*lo[..., None]))).mean(axis=2) <= 1):
            if lo.min() < -300:
                return False
            lo *= 2
        for _ in range(44):
            mid = (lo+hi)/2
            a = z/(z+np.exp(2*mid[..., None]))
            sign = factor*a.mean(axis=2) > 1
            lo = np.where(sign, mid, lo)
            hi = np.where(sign, hi, mid)
        k = np.array([sqrt(2/pi) if b == np.inf else 2*exp(normalizer(b))
                      for b in qb[:, 0]])
        val = np.log(k[:, None])-lo-factor/2*np.log1p(z/np.exp(2*hi[..., None])).mean(axis=2)
        bounds = bounds[val.max(axis=1) >= target-MARGIN]
        if not len(bounds):
            return True
        if np.any(~np.isfinite(bounds[:, 1])):
            return False
        mid = np.sqrt(bounds.prod(axis=1))
        bounds = np.concatenate((np.stack((bounds[:, 0], mid), axis=1),
                                 np.stack((mid, bounds[:, 1]), axis=1)))
    return False


def skew_student(x):
    _, counts = np.unique(x, return_counts=True)
    if 3*counts.max() > 2*len(x):
        return "unbounded"
    if 3*counts.max() == 2*len(x) and counts.max() in (
            np.count_nonzero(x == x[0]), np.count_nonzero(x == x[-1])):
        return "collapse"
    if len(counts) == 2:
        return "half"
    if 3*counts.max() == 2*len(x):
        return "gap"
    x = (x-x[0])/(x[-1]-x[0])
    witness = skew_student_witness(x)
    if witness <= skew_normal_upper(x)+MARGIN:
        return "gap"
    if half_student_excluded(x, witness) and skew_two_excluded(x, witness):
        return "attain"
    return "gap"


def weibull(x):
    """A bracketed root proposal and a concave tangent upper bound."""
    def evaluate(c):
        z = c*(x-1)
        weights = np.exp(z)
        value = log(c) + z.mean() - log(weights.mean())-1
        derivative = 1/c+x.mean()-np.dot(x, weights)/weights.sum()
        return value, derivative, weights/weights.mean()

    lo, hi = 1., 2.
    while evaluate(hi)[1] > 0:
        hi *= 2
    for _ in range(48):
        mid = (lo+hi)/2
        if evaluate(mid)[1] > 0:
            lo = mid
        else:
            hi = mid
    c = (lo+hi)/2
    value, derivative, v = evaluate(c)
    return c, value, value+abs(derivative)*(hi-lo), v


def burr_dual(y, k, c, eta):
    """Use conditional Newton steps only to propose entropy weights."""
    k = k[:, None]
    c = np.full_like(k, c)
    eta = np.full_like(k, eta)
    for _ in range(22):
        z = c*y+eta-np.log(k)
        p = np.exp(-np.logaddexp(0, -z))
        weight = (1+k)*p*(1-p)
        gc = 1/c+(y*(1-(1+k)*p)).mean(axis=1, keepdims=True)
        ge = 1-(1+k)*p.mean(axis=1, keepdims=True)
        cc = 1/c**2+(weight*y*y).mean(axis=1, keepdims=True)
        ce = (weight*y).mean(axis=1, keepdims=True)
        ee = weight.mean(axis=1, keepdims=True)+1e-20
        det = cc*ee-ce**2
        c += np.clip((ee*gc-ce*ge)/det, -c/2, c/2)
        eta += np.clip((cc*ge-ce*gc)/det, -1., 1.)
    p = np.exp(-np.logaddexp(0, np.log(k)-eta-c*y))
    p = feasible_weights(p, np.ones_like(p), len(y)/(1+k))
    dual_a = (((k+1)*p-1)*y).mean(axis=1, keepdims=True)
    return (np.log(k/np.where(dual_a>0, dual_a, np.nan))-1
            -(1+k)*entropy(p).mean(axis=1, keepdims=True))[:,0]


def burr_weibull_excluded(y, c, lower, v):
    first = np.mean(v*v)/2-1
    if first >= -MARGIN:
        return False
    r = min(1/16, c/2)
    features = np.stack((y, np.ones_like(y)))
    norm = np.linalg.norm(features, axis=0)
    a, b = v*np.exp(-r*norm), v*np.exp(r*norm)
    direction = features@(v*(v-1))/len(y)
    error = np.mean(v*v*abs(v-1)*norm)
    for steps in range(1, 25):
        h = 2.**(-steps)
        weights = a/(1+h*b)**2
        matrix = (features*weights)@features.T/len(y)+np.diag([1/(c+r)**2, 0])
        mu = np.linalg.det(matrix)/np.trace(matrix)
        D = np.linalg.norm(direction)+h*error
        if h*D < mu*r-MARGIN and h*(1/6+D*D/(2*mu)) < -first/2-MARGIN:
            break
    else:
        return False
    k0 = np.count_nonzero(y==0)/np.count_nonzero(y>0)
    knots = np.geomspace(k0, 1/h, 49)
    boxes = np.stack((knots[:-1], knots[1:]), axis=1)
    eta = -np.log(np.exp(c*(y-1)).mean())-c
    for _ in range(12):
        if len(boxes) > 384:
            return False
        knots, inverse = np.unique(boxes, return_inverse=True)
        with np.errstate(divide="ignore", invalid="ignore", over="ignore"):
            upper = burr_dual(y, knots, c, eta)[inverse].reshape(boxes.shape)
        left, right = boxes.T
        chord = upper.max(axis=1)+(right-left)**2/(8*left**2)
        boxes = boxes[~(chord < lower-MARGIN)]
        if not len(boxes):
            return True
        mid = np.sqrt(boxes.prod(axis=1))
        boxes = np.concatenate((np.stack((boxes[:, 0], mid), axis=1),
                                np.stack((mid, boxes[:, 1]), axis=1)))
    return False


def upper_majorizes(y, reference):
    y = y / y.sum()
    ref = np.sort(reference)/np.sum(reference)
    knots = np.unique(np.r_[np.arange(1, len(y))/len(y), np.arange(1, len(ref))/len(ref)])
    def lorenz(a):
        return np.interp(knots, np.arange(len(a)+1)/len(a), np.r_[0., np.cumsum(a)])
    a, b = lorenz(y), lorenz(ref)
    return np.all(((a == 0) & (b == 0)) | (a < b-MARGIN))


PARETO_REFERENCE = np.array([0, .23826/64, 7/960, 8/960, 8/960, 1, 1])


def burr(x):
    y = (x-x[0])/(x[-1]-x[0])
    if np.count_nonzero(y == 0)*2 >= len(y) or upper_majorizes(y, PARETO_REFERENCE):
        return "pareto"
    pareto = -log(y.mean())-1
    c, lower, upper, v = weibull(y)
    if lower > pareto+MARGIN and np.mean(v*v)/2 > 1+MARGIN:
        return "attain"
    # Evaluate a finite stencil; k is profiled algebraically at each point.
    centers = np.r_[0, np.quantile(y, [0.05, .1, .2, .35, .5, .65, .8, .95]), 1.]
    for power in c*np.geomspace(0.5, 64, 23):
        logits = power*(y-centers[:, None])
        softplus = np.logaddexp(0, logits).mean(axis=1)
        other = np.logaddexp(0, -logits).mean(axis=1)
        values = log(power)-np.log(softplus)-1-other
        if values.max() > max(pareto, upper)+MARGIN:
            return "attain"
    if lower > pareto+MARGIN and burr_weibull_excluded(y, c, lower, v):
        return "weibull"
    return "gap"


DESIGNS = {
    "std": ((3,), (6,), (20,)),
    "lst": ((1,), (3,), (12,)),
    "sstd": ((3, 1), (6, 2), (20, 0.5)),
    "sged": ((1, 1), (2, 2), (8, 0.5)),
    "burr": ((0.25, 2), (1, 1), (8, 2)),
    "invburr": ((0.25, 2), (1, 1), (8, 2)),
}


def draw(rng, family, params, n):
    if family in ("std", "lst"):
        return np.sort(rng.standard_t(params[0], n))
    if family in ("sstd", "sged"):
        p, xi = params
        side = rng.random(n) >= 1/(1+xi*xi)
        radius = abs(rng.standard_t(p, n)) if family == "sstd" else rng.gamma(1/p, 1, n)**(1/p)
        return np.sort(radius*np.where(side, xi, -1/xi))
    k, c = params
    # Generate log data directly. For inverse Burr the physical logs have
    # the opposite sign, and applying reciprocation maps them back here.
    return np.sort(np.log(np.expm1(rng.exponential(1/k, n)))/c)


def simulate(reps=200):
    rng = np.random.default_rng(42520261127)
    tests = {"std": lambda x: symmetric_student(x, 2), "lst": lambda x: symmetric_student(x, 1),
             "sged": skew_ged, "sstd": skew_student, "burr": burr, "invburr": burr}
    with (OUT/"simulation.csv").open("w", newline="") as file:
        writer = csv.writer(file)
        writer.writerow(("family", "parameters", "n", "repetitions", "attain", "escape", "gap"))
        for family, designs in DESIGNS.items():
            for par in designs:
                for n in (10, 30, 100):
                    counts = {"attain": 0, "escape": 0, "gap": 0}
                    for _ in range(reps):
                        result = tests[family](draw(rng, family, par, n))
                        counts[result if result in ("attain", "gap") else "escape"] += 1
                    row = [family, "/".join(map(str, par)), n, reps,
                           counts["attain"], counts["escape"], counts["gap"]]
                    writer.writerow(row)
                    print(*row, flush=True)


if __name__ == "__main__":
    simulate()
