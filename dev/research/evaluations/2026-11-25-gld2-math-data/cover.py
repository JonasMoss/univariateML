"""Monotone spacing cover for the TASK-241 extreme half tie."""
import heapq
import itertools
import json
import math
import time


def integral(lo, hi, shape):
    if lo == hi:
        return 0.0
    if lo == 0:
        return hi**shape / shape if shape > 0 else math.inf
    if shape == 0:
        return math.log(hi / lo)
    return lo**shape * math.expm1(shape * math.log(hi / lo)) / shape


def spacing(p, r, a, b):
    return integral(p, r, a) + integral(1-r, 1-p, b)


def bracket(p, gap, a, b):
    lo, hi = p, 1.0
    for _ in range(26):
        mid = (lo + hi) / 2
        if spacing(p, mid, a, b) <= gap:
            lo = mid
        else:
            hi = mid
    return lo, hi


def power(p, shape):
    return p**(shape-1) if p else (1.0 if shape == 1 else math.inf)


def primitive_max(vl, vh, w, c, e):
    if e == 0:
        return c+w-vl
    if e == 2:
        return 1.0 + max(vl*(c-1/w), vh*(c-1/w))
    points = [vl, vh]
    if abs(e-1) < 1e-12:
        critical = math.log(w)+c-2
    else:
        den = e*(w**(1-e)-(e-1)*c)
        critical = math.log((2-e)/den)/(e-1) if den > 0 else math.inf
    if math.log(max(vl, 1e-300)) < critical < math.log(max(vh, 1e-300)):
        points.append(math.exp(critical))
    return max(
        (v**(e/2) * (c+integral(v, w, 1-e))) if v > 0 else 0.0
        for v in points
    )


def pair_bound(box, d):
    (pl, ph), (sl, sh), (al, ah), (bl, bh) = box
    gl, gh = d * math.exp(-sh), d * math.exp(-sl)
    if gl > spacing(pl, 1., al, bl):
        return 0.0
    rl = bracket(pl, gl, al, bl)[0]
    rh = bracket(ph, gh, ah, bh)[1]
    vl, vh = 1-rh, 1-rl
    head = power(ph, ah) + power(1-pl, bh)
    tail = power(rh, ah) + power(vh, bh)
    bound = min(rh-pl, gh / math.sqrt(head*tail))
    # Left and right terms separately satisfy the reciprocal-square chord
    # bound. Using the exact right primitive can improve the right term.
    left = (rh-pl) * math.sqrt(power(rl, al)/tail) if rl else math.inf
    bound = min(bound, left+1/math.sqrt(head))
    c = integral(pl, rh, al)
    if math.isfinite(c):
        # Convexity in the right exponent, with the left primitive fixed.
        w = 1-pl
        primitive = max(primitive_max(vl, vh, w, c, e) for e in (1-bh, 1-bl))
        bound = min(bound, primitive/math.sqrt(head*(1+vl**(1-bl))))
    if bh <= 0 and al > 0 and vh <= 0.2:
        cap = 1/al - 1
        h0 = max(0.0, head-2)
        vstar = min(vh, 1/math.e**2)
        one = (math.sqrt(vstar)*math.log(1/vstar) + math.sqrt(vh)/al)/math.sqrt(head)
        two = max(
            (1-vl*vl+vh*min(h, cap))/math.sqrt(2+h)
            for h in (h0, max(h0, cap))
        )
        bound = min(bound, max((1-theta)*one+theta*two for theta in (-bh, -bl)))
    return bound


def cover(limit=150_001):
    start = time.perf_counter()
    # Delta=1/2, D(p)<=24/5 in the small-scale t=0 sector. The
    # cutoff proof in the report excludes s<=1/128.
    # Modal upper scale is sqrt(6)/2. At level B the chord also
    # requires p <= 1-1/sqrt(2) < 0.3.
    initial = ((0., .3), (-math.log(128), math.log(math.sqrt(6)/2)),
               (-1., 1.), (-1., 1.))
    norms = [hi-lo for lo, hi in initial]
    queue = []
    serial = itertools.count()
    evaluated = rejected = 0
    margin = 2e-8

    def add(box):
        nonlocal evaluated, rejected
        evaluated += 1
        (pl, ph), (sl, sh), (_, ah), _ = box
        # Any level-B candidate has |mu| <= sqrt(3 sigma), by
        # pairing observations 1 and 3 and bounding both factors at 0.
        mu_max = math.sqrt(3*math.exp(sh))
        cap = min(12., 3*math.exp(-2*sl))
        if mu_max < 3:
            cap = min(cap, (6/(3-mu_max))**2)
        cap += 1e-12*cap
        if ah < 1:
            lower = math.exp(-math.log(cap-1)/(1-ah))
            pl = max(pl, lower*(1-1e-12))
            if pl > ph:
                rejected += 1
                return
            box = ((pl, ph),) + box[1:]
        upper = pair_bound(box, 1) * pair_bound(box, 3)
        if upper < .5-margin:
            rejected += 1
        else:
            heapq.heappush(queue, (-upper, next(serial), box))

    add(initial)
    while queue and evaluated+2 <= limit:
        _, _, box = heapq.heappop(queue)
        axis = max(range(4), key=lambda i: (box[i][1]-box[i][0])/norms[i])
        lo, hi = box[axis]
        mid = (lo+hi)/2
        add(box[:axis] + ((lo, mid),) + box[axis+1:])
        add(box[:axis] + ((mid, hi),) + box[axis+1:])
    return {
        'sample': [0, 0, 1, 3], 'evaluated': evaluated, 'discarded': rejected,
        'pending': len(queue), 'root_upper_threshold': .5, 'margin': margin,
        'pending_upper': -queue[0][0] if queue else None,
        'largest_pending_box': queue[0][2] if queue else None,
        'seconds': time.perf_counter()-start,
    }


if __name__ == '__main__':
    import sys
    print(json.dumps(cover(int(sys.argv[1])) if len(sys.argv)>1 else cover(), indent=2))
