"""Exploratory binary64 grid; candidate discovery, with fixed seed."""
import numpy as np

rng = np.random.default_rng(344)
grid = np.linspace(-7, 7, 401)


def profile(v, weights):
    t = np.exp(np.clip(v, -700, 700))
    with np.errstate(divide="ignore", invalid="ignore", over="ignore"):
        h = np.log(-np.expm1(-np.minimum(t, 1)))
        tail = t > 1
        h[tail] = np.log1p(-np.exp(-t[tail]))
        s = -(h @ weights)
        return ((v - t - h) @ weights) - np.log(s) - 1


def peaks(values):
    return np.flatnonzero(
        np.isfinite(values[1:-1])
        & (values[1:-1] > values[:-2] + 1e-7)
        & (values[1:-1] > values[2:] + 1e-7)
    ) + 1


def scan():
    for mask in ("held scale", "held shape"):
        for trial in range(10000):
            levels = np.unique(rng.integers(-24, 25, rng.integers(2, 9)))
            if len(levels) < 2 or (mask == "held scale" and max(levels) <= 0):
                continue
            counts = rng.integers(1, 61, len(levels))
            weights = counts / sum(counts)
            y = levels * np.log(2)
            if mask == "held scale":
                values = grid + profile(np.exp(grid[:, None]) * y, weights)
            else:
                b = 2.0 ** int(rng.integers(-9, 3))
                values = profile(grid[:, None] + b * y, weights) + np.log(b)
            hits = peaks(values)
            if len(hits) >= 2:
                hold = 1 if mask == "held scale" else b
                print(mask, trial, hold, levels.tolist(), counts.tolist(),
                      list(zip(grid[hits], values[hits])))
                break
        else:
            print(mask, "no pair")


if __name__ == "__main__":
    scan()
