"""Find small exact examples. Samples printed here count black draws."""
from fractions import Fraction as Q
from itertools import combinations_with_replacement
from math import prod


def white_ratio(m, n, r, ys):
    a = m + n + 1
    return prod(Q((a-m)*(a-r-y), a*(a-m-y)) for y in ys)


def black_ratio(m, n, r, ys):
    a = m + n + 1
    return prod(Q((m+1)*(a-r-y), (m+1-r)*a) for y in ys)


def examples():
    for held in ('white', 'black'):
        tie = None
        different = None
        for r in range(1, 5):
            for count in range(1 if held == 'black' else r, 12):
                peaks = {}
                for h in range(2, 6):
                    max_y = count if held == 'black' else 7
                    for ys in combinations_with_replacement(range(max_y+1), h):
                        if ys[0] == ys[-1]:
                            continue
                        if held == 'white':
                            pairs = ((count, n) for n in range(ys[-1], count*(r+ys[-1])//r + 1))
                            ratio = white_ratio
                        else:
                            pairs = ((m, count) for m in range(r, count*r*h//sum(ys) + r + 1))
                            ratio = black_ratio
                        for m, n in pairs:
                            q = ratio(m, n, r, ys)
                            if q <= 1:
                                N = m+n
                                break
                        else:
                            raise AssertionError(ys)
                        modes = (N, N+1) if q == 1 else (N,)
                        if q == 1 and tie is None:
                            tie = (r, count, ys, modes)
                        key = (h, ys[-1], sum(ys))
                        if key in peaks and not set(modes).intersection(peaks[key][1]) and different is None:
                            different = (r, count, peaks[key], (ys, modes))
                        peaks[key] = ys, modes
                        if tie and different:
                            break
                    if tie and different:
                        break
                if tie and different:
                    break
            if tie and different:
                break
        assert tie and different
        print(held, 'nonconstant tie:', tie, 'same size, sum and max:', different)


if __name__ == '__main__':
    examples()
