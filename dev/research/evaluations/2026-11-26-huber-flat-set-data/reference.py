"""Independent 100-digit values for the pinned Huber fits."""
import mpmath as mp

mp.mp.dps = 100
x = [-10, -9, 9, 10]

def normalizer(c):
    return mp.sqrt(2 * mp.pi) * mp.erf(c / mp.sqrt(2)) + 2 * mp.exp(-c*c/2) / c

def loglik(x, m, s, c):
    residuals = [abs((mp.mpf(v)-m)/s) for v in x]
    return -len(x)*mp.log(s*normalizer(c)) - sum(
        z*z/2 if z <= c else c*(z-c/2) for z in residuals)

def case(name, x, s, c, locations):
    print(name)
    for key, value in [('sigma', s), ('epsilon', c), ('lower', x[1]+c*s),
                       ('upper', x[2]-c*s)]:
        print(key, mp.nstr(value, 60))
    for m in locations:
        print('logLik at', m, mp.nstr(loglik(x, m, s, c), 60))

case('held scale and shape', x, mp.mpf(1), mp.mpf(1.345), [-5, 0, 5])
case('held scale and shape, equality', x, mp.mpf(1), mp.mpf(9), [0])
case('held shape, free scale', x, mp.mpf('4.75'), mp.mpf('.5'), [-5, 0, 5])
c = mp.findroot(lambda c: mp.diff(lambda t: loglik(x, 0, mp.mpf(1), t), c),
                (mp.mpf('.1'), mp.mpf('.11')))
case('held scale, free shape', x, mp.mpf(1), c, [-5, 0, 5])

# This sample is symmetric, so zero profiles location for every shape.
# Search the one-dimensional held-scale likelihood independently of the C++ bounds.
roots = []
score = lambda t: mp.diff(lambda z: loglik(x, 0, mp.mpf(1), z), t)
grid = [mp.mpf(10)**(-3+mp.mpf(k)/100) for k in range(401)]
for left, right in zip(grid, grid[1:]):
    if score(left)*score(right)<0:
        root = mp.findroot(score, (left, right), solver='bisect', maxsteps=360,
                           tol=mp.mpf('1e-95'), verify=False)
        roots.append(root)
for root in roots:
    print('profile stationary shape and value', mp.nstr(root, 60),
          mp.nstr(loglik(x, 0, mp.mpf(1), root), 60))
print('held-scale normal limit', mp.nstr(-2*mp.log(2*mp.pi)-mp.mpf(181), 60))
scale = mp.sqrt(mp.mpf('90.5'))
print('free-scale normal limit', mp.nstr(-4*mp.log(scale)-2*mp.log(2*mp.pi)-2, 60))
print('free-scale Laplace limit', mp.nstr(-4*(1+mp.log(19)), 60))
