"""Check v1 maximizer values from independent pilot recipes and snapshots."""
from decimal import Decimal, localcontext
from fractions import Fraction
from itertools import product
import math

COUNT_MAX = 2**53 - 1


def number(x):
    if x in ('-inf', 'inf'):
        return float(x)
    if type(x) not in (int, float) or not math.isfinite(x):
        raise ValueError('invalid coefficient')
    if isinstance(x, int) and int(float(x)) != x:
        raise ValueError('coefficient is outside binary64')
    return float(x)


def vector(values, dimension):
    if not isinstance(values, list) or len(values) != dimension:
        raise ValueError('wrong coefficient dimension')
    decoded = tuple(map(number, values))
    if not all(map(math.isfinite, decoded)):
        raise ValueError('nonfinite coefficient')
    return decoded


def axis(value):
    lo, hi = (number(value[k]) for k in ('lower', 'upper'))
    lc, hc = value['closed']
    lr, hr = value.get('rounded', [False, False])
    integer = value['integer']
    if any(type(bit) is not bool for bit in (lc, hc, lr, hr, integer)):
        raise ValueError('invalid axis flags')
    if lo == math.inf or hi == -math.inf or (lo == -math.inf and lc) or (
            hi == math.inf and hc) or lo > hi:
        raise ValueError('invalid bounds')
    if integer:
        if lr or hr:
            raise ValueError('rounded lattice boundary')
        if math.isfinite(lo):
            lo = math.ceil(lo) if lc else math.floor(lo) + 1
            lc = True
        if math.isfinite(hi):
            hi = math.floor(hi) if hc else math.ceil(hi) - 1
            hc = True
    return (lo, hi, lc, hc, integer, lr, hr)


def includes(a, x):
    lo, hi, lc, hc, integer, _, _ = a
    return (lo < x or lc and lo == x) and (x < hi or hc and x == hi) and (
        not integer or x == int(x))


def fixed(x):
    return (x, x, True, True, False, False, False)


def normalize(wire, names, holds=None):
    if type(wire['version']) is not int or wire['version'] != 1:
        raise ValueError('unknown version')
    if (wire['coordinates'] != list(names) or len(set(names)) != len(names) or
            not 1 <= len(names) <= 5 or any(type(x) is not str or not x for x in names)):
        raise ValueError('invalid coordinate names or order')
    dim = len(names)
    boxes, relations, enclosed = [], [], []
    for piece in wire['pieces']:
        kind = piece['kind']
        if kind == 'points':
            if not piece['points']:
                raise ValueError('empty points piece')
            for point in piece['points']:
                estimate = vector(point['estimate'], dim)
                if holds and any(estimate[names.index(k)] != v for k, v in holds.items()):
                    continue
                enclosure = point.get('enclosure')
                if enclosure is None:
                    boxes.append((tuple(map(fixed, estimate)), ()))
                else:
                    lower, upper = (vector(enclosure[key], dim) for key in ('lower', 'upper'))
                    if not all(a <= x <= b for a, x, b in zip(lower, estimate, upper)):
                        raise ValueError('point falls outside enclosure')
                    if any(all(a <= d and c <= b for a, b, c, d in
                               zip(lower, upper, other[1], other[2])) for other in enclosed):
                        raise ValueError('overlapping enclosures')
                    enclosed.append((estimate, lower, upper))
            continue
        axes = tuple(map(axis, piece['axes']))
        if len(axes) != dim:
            raise ValueError('wrong axis dimension')
        if holds:
            if any(not includes(axes[names.index(k)], v) for k, v in holds.items()):
                continue
            axes = tuple((*fixed(holds[k])[:4], a[4], False, False) if k in holds else a
                         for k, a in zip(names, axes))
        if kind == 'box':
            orders = set()
            for i, j in piece['order']:
                if not (type(i) is type(j) is int and 0 <= i < dim and 0 <= j < dim and
                        axes[i][4] and axes[j][4]):
                    raise ValueError('invalid count order')
                if i != j:
                    orders.add((i, j))
            boxes.append((axes, tuple(sorted(orders))))
        elif kind == 'relation':
            i, j = piece['driver'], piece['dependent']
            if not (type(i) is type(j) is int and 0 <= i < dim and 0 <= j < dim and i != j):
                raise ValueError('invalid relation coordinates')
            form = piece['form']
            if form == 'semicircle':
                a, b = vector(piece['anchors'], 2)
                if not a < b:
                    raise ValueError('invalid semicircle anchors')
                level, complement = (a, b), ()
            elif form in ('product', 'part_over_sum', 'count_ratio', 'adjacent_binomial'):
                v = piece['level']
                if isinstance(v, dict):
                    if not all(type(v[k]) is str and v[k].isascii() and v[k].isdigit()
                               for k in ('numerator', 'denominator')):
                        raise ValueError('invalid rational encoding')
                    level = Fraction(int(v['numerator']), int(v['denominator']))
                else:
                    level = Fraction(number(v))
                complement = tuple(piece.get('complement', ()))
                if form == 'product' and (len(complement) != 2 or any(type(c) is not bool for c in complement)):
                    raise ValueError('invalid complements')
                if not level > 0 or form == 'part_over_sum' and level >= 1:
                    raise ValueError('invalid relation level')
            else:
                raise ValueError('unknown relation')
            driver, dependent = axes[i], axes[j]
            if form == 'semicircle':
                if (driver[4] or dependent[4] or driver[0] < a or driver[1] > b or
                        includes(driver, a) or includes(driver, b) or
                        dependent[0] < 0 or includes(dependent, 0)):
                    raise ValueError('invalid semicircle domain')
            elif form == 'count_ratio':
                if (not isinstance(v, dict) or level > 1 or level.denominator > COUNT_MAX or
                        not driver[4] or not dependent[4] or driver[0] < 1 or dependent[0] < 1):
                    raise ValueError('invalid count ratio domain')
            elif form == 'adjacent_binomial':
                lower_count = number(piece['lower_count'])
                if (not isinstance(v, dict) or not math.isfinite(lower_count) or
                        lower_count < 0 or math.floor(lower_count) != lower_count or
                        not driver[4] or driver[0] <= lower_count or dependent[4] or
                        dependent[0] < 0 or dependent[1] > 1 or
                        includes(dependent, 0) or includes(dependent, 1)):
                    raise ValueError('invalid adjacent-binomial domain')
                level = (int(lower_count), level)
            else:
                if driver[4] or dependent[4]:
                    raise ValueError('real relation uses a lattice')
                for k, domain in enumerate((driver, dependent)):
                    if form == 'product' and complement[k]:
                        if domain[1] > 1 or includes(domain, 1):
                            raise ValueError('nonpositive complemented factor')
                    elif domain[0] < 0 or includes(domain, 0):
                        raise ValueError('nonpositive factor')
            if form != 'product' and complement:
                raise ValueError('unexpected complement bits')
            rounded = piece.get('level_rounded', False)
            if type(rounded) is not bool or (form == 'adjacent_binomial' and rounded):
                raise ValueError('invalid level rounding flag')
            if form == 'product' and j < i:
                i, j, complement = j, i, complement[::-1]
            if form == 'part_over_sum' and j < i and not rounded:
                i, j, level = j, i, 1 - level
            signature = (form, i, j, level, complement, rounded)
            if holds and (names[i] in holds or names[j] in holds):
                for lo, hi, *_ in relation_intervals(signature, axes[i], axes[j]):
                    if lo != hi:
                        raise ValueError('held relation has a non-singleton fiber')
                    x, y = relation_point(signature, lo)
                    if names[i] in holds:
                        x = holds[names[i]]
                    if names[j] in holds:
                        y = holds[names[j]]
                    estimate = tuple(x if k == i else y if k == j else ax[0]
                                     for k, ax in enumerate(axes))
                    boxes.append((tuple(map(fixed, estimate)), ()))
            else:
                relations.append((signature, axes))
        else:
            raise ValueError('unknown piece kind')
    return boxes, relations, enclosed


def cells(endpoints, lattice, continuous):
    """Each cell has constant interval membership, including each endpoint."""
    endpoints = sorted(set(endpoints) | {-math.inf, math.inf})
    for lo, hi in zip(endpoints, endpoints[1:]):
        if math.isfinite(lo):
            if continuous or lo == int(lo):
                yield (lo, lo, True, True, lo == int(lo), False, False)
        if lattice:
            a = math.floor(lo) + 1 if math.isfinite(lo) else -math.inf
            b = math.ceil(hi) - 1 if math.isfinite(hi) else math.inf
            if a <= b:
                yield (a, b, math.isfinite(a), math.isfinite(b), True, False, False)
        if continuous:
            yield (lo, hi, False, False, False, False, False)


def covers_axis(piece, cell):
    lo, hi, lc, hc, integer, _, _ = piece
    a, b, ac, bc, lattice, _, _ = cell
    return (not integer or lattice) and (lo < a or lo == a and (lc or not ac)) and (
        b < hi or b == hi and (hc or not bc))


def feasible(cell, constraints):
    """x + d <= y, with integer strict edges represented with d=1."""
    size = len(cell)
    # Difference constraints include a fixed zero vertex and virtual root.
    edges = [(j, i, -d) for i, j, d in constraints]
    for j, (lo, hi, *_rest) in enumerate(cell):
        if math.isfinite(lo):
            edges.append((j, size, -int(lo)))
        if math.isfinite(hi):
            edges.append((size, j, int(hi)))
    distance = [0] * (size + 1)
    for _ in distance:
        changed = False
        for i, j, weight in edges:
            new = distance[i] + weight
            if new < distance[j]:
                distance[j], changed = new, True
        if not changed:
            return True
    return False


def equal_boxes(a, b, dim):
    combined = a + b
    if not combined:
        return True
    partitions = []
    for i in range(dim):
        axes = [axes[i] for axes, _ in combined]
        endpoints = [endpoint for ax in axes for endpoint in ax[:2]]
        lattice = any(ax[4] for ax in axes)
        continuous = any(not ax[4] and ax[0] != ax[1] for ax in axes)
        partitions.append(list(cells(endpoints, lattice, continuous)))
    for cell in product(*partitions):
        def active(boxes):
            return [order for axes, order in boxes if all(map(covers_axis, axes, cell))]
        left, right = active(a), active(b)
        if left == right:
            continue
        orders = sorted(set(e for constraints in left + right for e in constraints))
        for truth in product((False, True), repeat=len(orders)):
            flags = dict(zip(orders, truth))
            in_left = any(all(flags[e] for e in order) for order in left)
            in_right = any(all(flags[e] for e in order) for order in right)
            if in_left == in_right:
                continue
            constraints = [(i, j, 0) if t else (j, i, 1) for (i, j), t in flags.items()]
            if feasible(cell, constraints):
                return False
    return True


def intersect(first, second):
    a, b, ac, bc = first
    c, d, cc, dc = second
    lo, hi = max(a, c), min(b, d)
    left_closed = (lo != a or ac) and (lo != c or cc)
    right_closed = (hi != b or bc) and (hi != d or dc)
    if lo > hi or lo == hi and not (left_closed and right_closed):
        return None
    return lo, hi, left_closed, right_closed


def merge(intervals, lattice=False):
    out = []
    for lo, hi, lc, hc in sorted(intervals, key=lambda x: (x[0], not x[2], x[1], not x[3])):
        if out:
            a, b, ac, bc = out[-1]
            if lo < b or lo == b and (lc or bc) or lattice and lo == b + 1:
                if hi > b or hi == b and hc:
                    out[-1] = a, hi, ac, hc
                continue
        out.append((lo, hi, lc, hc))
    return tuple(out)


def relation_intervals(signature, driver, dependent):
    """Restrict the driver by the dependent axis, splitting the semicircle."""
    form, _, _, level, bits, _ = signature
    if form == 'count_ratio':
        n, d = level.numerator, level.denominator
        x0, x1 = max(1, driver[0]), min(COUNT_MAX, driver[1])
        y0, y1 = max(1, dependent[0]), min(COUNT_MAX, dependent[1])
        lo = max((x0 + n - 1) // n, (y0 + d - 1) // d)
        hi = min(x1 // n, y1 // d)
        return [(lo, hi, True, True)] if lo <= hi else []
    if form == 'adjacent_binomial':
        lower_count, odds = level

        def invert(prob):
            return lower_count + odds * (1 / Fraction(prob) - 1) if prob else math.inf

        lo, hi = invert(dependent[1]), invert(dependent[0])
        first = math.ceil(lo) if dependent[3] else math.floor(lo) + 1
        last = hi if hi == math.inf else math.floor(hi) if dependent[2] else math.ceil(hi) - 1
        bound = intersect((first, last, True, last != math.inf), driver[:4])
        return [] if bound is None else [bound]
    if form == 'semicircle':
        with localcontext() as ctx:
            # Cover the exponent range of subtracting two exact binary64
            # values, including subnormals, before taking a square root.
            ctx.prec = 1200
            a, b = map(Decimal, level)
            lo, hi = map(Decimal, dependent[:2])
            radius = (b - a) / 2
            domain = tuple(map(Decimal, driver[:2])) + tuple(driver[2:4])
            cut = intersect((lo, hi, *dependent[2:4]), (Decimal(0), radius, False, True))
            if cut is None:
                return []
            lo, hi, lc, hc = cut

            def distance(y):
                offset = ((radius - y) * (radius + y)).sqrt()
                return (y / (radius + offset)) * y

            near, far = distance(lo), distance(hi)
            arcs = [(a + near, a + far, lc, hc), (b - far, b - near, hc, lc)]
            return [part for arc in arcs if (part := intersect(arc, domain)) is not None]

    def rational(x):
        return Fraction(x) if math.isfinite(x) else x

    d0, d1 = map(rational, dependent[:2])
    driver = tuple(map(rational, driver[:2])) + tuple(driver[2:4])

    def invert(y):
        if form == 'part_over_sum':
            return y * level / (1 - level)
        factor = 1 - y if bits[1] else y
        x = level / factor if factor else math.inf
        return 1 - x if bits[0] else x

    x, y = invert(d0), invert(d1)
    lc, hc = dependent[2:4]
    if form == 'product' and bits[0] == bits[1]:
        x, y, lc, hc = y, x, hc, lc
    restriction = intersect((x, y, lc, hc), driver)
    return [] if restriction is None else [restriction]


def relation_point(signature, driver):
    form, _, _, level, bits, _ = signature
    if form == 'count_ratio':
        return int(driver * level.numerator), int(driver * level.denominator)
    if form == 'semicircle':
        with localcontext() as ctx:
            ctx.prec = 1200
            a, b = map(Decimal, level)
            driver = Decimal(driver)
            return float(driver), float(((driver - a) * (b - driver)).sqrt())
    x = Fraction(driver)
    if form == 'adjacent_binomial':
        lower_count, odds = level
        return int(x), float(odds / (x - lower_count + odds))
    if form == 'product':
        y = level / (1 - x if bits[0] else x)
        return float(x), float(1 - y if bits[1] else y)
    return float(x), float(x * (1 - level) / level)


def relation_contains_exact(signature, x, y):
    form, _, _, level, bits, _ = signature
    x, y = Fraction(x), Fraction(y)
    if form == 'semicircle':
        a, b = map(Fraction, level)
        return a < x < b and y > 0 and y*y == (x - a) * (b - x)
    if form == 'count_ratio':
        return x >= 1 and y >= 1 and x.denominator == y.denominator == 1 and x == level * y
    if form == 'adjacent_binomial':
        lower_count, odds = level
        return x.denominator == 1 and x > lower_count and 0 < y < 1 and (x-lower_count)*y == odds*(1-y)
    if form == 'part_over_sum':
        return x > 0 and y > 0 and x == level * (x + y)
    x, y = 1 - x if bits[0] else x, 1 - y if bits[1] else y
    return x > 0 and y > 0 and x*y == level


def absorb_isolated(boxes, coverage, domains):
    unmatched = []
    coverage = {key: list(parts) for key, parts in coverage.items()}
    for axes, order in boxes:
        if order or any(a[0] != a[1] or not (a[2] and a[3]) for a in axes):
            unmatched.append((axes, order))
            continue
        coordinates = tuple(a[0] for a in axes)
        for signature, others in domains:
            form, i, j, level, *_ = signature
            if any(coordinates[k] != axis[0] for k, axis in others):
                continue
            if not relation_contains_exact(signature, coordinates[i], coordinates[j]):
                continue
            x = coordinates[i]
            x = (int(x) // level.numerator if form == 'count_ratio' else
                 Decimal(x) if form == 'semicircle' else Fraction(x))
            coverage.setdefault((signature, others), []).append((x, x, True, True))
            break
        else:
            unmatched.append((axes, order))
    return unmatched, {key: merge(parts, lattice=key[0][0] in ('count_ratio', 'adjacent_binomial'))
                       for key, parts in coverage.items()}


def relation_coverage(relations):
    groups = {}
    for signature, axes in relations:
        form, i, j, *_ = signature
        fixed_axes = tuple((k, a) for k, a in enumerate(axes) if k not in (i, j))
        if any(a[0] != a[1] for _, a in fixed_axes):
            raise ValueError('relation has an extra free coordinate')
        groups.setdefault((signature, fixed_axes), []).extend(relation_intervals(signature, axes[i], axes[j]))
    return {key: merge(parts, lattice=key[0][0] in ('count_ratio', 'adjacent_binomial')) for key, parts in groups.items() if parts}


def member(value, decoded, ulps=8):
    boxes, relations, enclosed = decoded
    if any(all(map(includes, axes, value)) and all(value[i] <= value[j] for i, j in order)
           for axes, order in boxes):
        return True
    if any(all(a <= x <= b for x, a, b in zip(value, lower, upper)) for _, lower, upper in enclosed):
        return True
    for (form, i, j, level, complement, _), axes in relations:
        if not all(map(includes, axes, value)):
            continue
        x, y = value[i], value[j]
        if form == 'count_ratio':
            if Fraction(x) == Fraction(y) * level:
                return True
            continue
        if form == 'semicircle':
            lhs, rhs = y, math.sqrt(x - level[0]) * math.sqrt(level[1] - x)
        elif form == 'adjacent_binomial':
            _, rhs = relation_point((form, i, j, level, complement, False), x)
            lhs = y
        elif form == 'product':
            lhs = (1 - x if complement[0] else x) * (1 - y if complement[1] else y)
            rhs = float(level)
        else:
            lhs, rhs = x / (x + y), float(level)
        if abs(lhs - rhs) <= ulps * max(math.ulp(lhs), math.ulp(rhs)):
            return True
    return False


def compare(actual, expected, holds, *, location_scale=None):
    names = expected['coordinates']
    got = normalize(actual, names)
    want = normalize(expected, names, holds)
    for (estimate, lower, upper) in got[2]:
        for i, name in enumerate(names):
            allowance = 1e-10 * max(abs(estimate[i]), location_scale or 0) if i == 0 else (
                1e-10 * abs(estimate[i]))
            if max(estimate[i] - lower[i], upper[i] - estimate[i]) > allowance + 2 * math.ulp(estimate[i]):
                raise ValueError('enclosure exceeds coefficient tolerance')
            if name in holds and not estimate[i] == lower[i] == upper[i] == holds[name]:
                raise ValueError('enclosure changes an exact hold')
    if got[2]:
        # For the pilot the independent high-precision references are rounded
        # once to binary64. Check branch identity with an eight-ulp allowance.
        references = [(i, tuple(ax[0] for ax in axes))
                      for i, (axes, order) in enumerate(want[0])
                      if not order and all(ax[0] == ax[1] for ax in axes)]
        matched = set()
        for _, lower, upper in got[2]:
            branches = [i for i, ref in references if all(
                a - 8 * math.ulp(x) <= x <= b + 8 * math.ulp(x) for x, a, b in zip(ref, lower, upper))]
            if len(branches) != 1 or branches[0] in matched:
                raise ValueError('enclosure does not match a distinct reference branch')
            matched.add(branches[0])
        if len(matched) != len(references):
            raise ValueError('missing enclosed branch')
        want = ([piece for i, piece in enumerate(want[0]) if i not in matched], want[1], want[2])
    left_relations = relation_coverage(got[1])
    right_relations = relation_coverage(want[1])
    domains = tuple(dict.fromkeys((*left_relations, *right_relations)))
    left_boxes, left_relations = absorb_isolated(got[0], left_relations, domains)
    right_boxes, right_relations = absorb_isolated(want[0], right_relations, domains)
    if left_relations != right_relations:
        raise ValueError('relation forms, levels or axis coverage disagree')
    if not equal_boxes(left_boxes, right_boxes, len(names)):
        raise ValueError('maximizer union coverage differs')
    def rounded_endpoints(boxes):
        return {(i, side, a[side]) for axes, _ in boxes for i, a in enumerate(axes)
                for side in range(2) if a[5 + side]}
    if rounded_endpoints(left_boxes) != rounded_endpoints(right_boxes):
        raise ValueError('analytic endpoint rounding flags disagree')
    return got
