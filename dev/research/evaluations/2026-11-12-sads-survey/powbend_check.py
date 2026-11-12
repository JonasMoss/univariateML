"""Power-bend ML behaviour checks (2026-11-12). High-precision comparison, not proof.

p(x) = x^{-s} exp(-w x) / Li_s(exp(-w)),  x = 1, 2, ...

Run: python3 -I powbend_check.py   (needs mpmath)
"""
import mpmath as mp

mp.mp.dps = 40


def loglik(x, s, w):
    z = mp.zeta(s) if w == 0 else mp.polylog(s, mp.e ** (-w))
    return sum(-s * mp.log(v) - w * v for v in x) - len(x) * mp.log(z)


def s_hat(x, w, s0):
    return mp.findroot(lambda t: mp.diff(lambda u: loglik(x, u, w), t), s0)


def profile(x, ws, s0):
    for w in ws:
        s0 = s_hat(x, w, s0)
        print("  w=%-6g s=%9.4f loglik=%.6f" % (w, float(s0), float(loglik(x, s0, w))))


# 1. Escape to zeta: zeta MLE s > 2 and xbar >= zeta(s-1)/zeta(s).
x = [1] * 40 + [2] * 6 + [3] * 2 + [60]
s = s_hat(x, 0, 3)
print("escape: zeta s=%.4f xbar=%.4f E_s X=%.4f" % (
    float(s), sum(x) / len(x), float(mp.zeta(s - 1) / mp.zeta(s))))
profile(x, [0, 1e-6, 1e-4, 1e-2, 0.1, 0.3], 3)   # decreasing in w: max on w = 0

# 2. Interior maximum with s < 0 (outside sads's s >= 0).
x = [1] * 20 + [2] * 10 + [3] * 6 + [4] * 3 + [5] * 2 + [7]
print("interior:")
profile(x, [0.3, 0.6, 0.9, 1.5], 1)
score = lambda s, w: [mp.diff(lambda u: loglik(x, u, w), s),
                      mp.diff(lambda u: loglik(x, s, u), w)]
r = mp.findroot(score, (mp.mpf(-0.3), mp.mpf(0.8)))
print("  joint MLE s=%.6f w=%.6f loglik=%.6f" % (float(r[0]), float(r[1]),
      float(loglik(x, r[0], r[1]))))   # s = -0.250316, w = 0.754202

# 3. Two adjacent values: supremum 10 log(1/2) approached along a ray, not attained.
x = [3] * 5 + [4] * 5
print("adjacent: sup =", mp.nstr(10 * mp.log(0.5), 10))
for w in [1, 5, 20, 80]:
    s = -w / mp.log(mp.mpf(4) / 3)   # keeps p(3) = p(4)
    print("  w=%-3g loglik=%s" % (w, mp.nstr(loglik(x, s, w), 10)))
