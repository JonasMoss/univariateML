"""Numerical nomination of a zero-mean coexistence interval."""
import mpmath as mp

mp.mp.dps = 60


def roots(k, weight):
    def score(z):
        return (1-weight)*z*mp.besseli(1,z)/mp.besseli(0,z) + weight*(
            k + z*mp.besseli(k+1,z)/mp.besseli(k,z)) - z

    grid = [mp.exp(mp.mpf(i)/16) for i in range(-48, 113)]
    values = [score(x) for x in grid]
    out = []
    for a, b, sa, sb in zip(grid, grid[1:], values, values[1:]):
        if sa > 0 > sb:
            root = mp.findroot(score, (a,b), solver="bisect", maxsteps=240)
            value = (1-weight)*mp.log(mp.besseli(0,root)) + weight*mp.log(
                mp.besseli(k,root)) - root
            out.append((root, value))
    return out


if __name__ == "__main__":
    for k in (10, 20, 40, 80, 160):
        for numerator in range(50, 63):
            w = mp.mpf(numerator)/(100*k)
            modes = roots(k,w)
            if len(modes) == 2:
                print(k, numerator, *(mp.nstr(z, 12) for z,_ in modes),
                      mp.nstr(modes[1][1]-modes[0][1], 14), flush=True)
