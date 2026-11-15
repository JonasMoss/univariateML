"""Running-error arithmetic that mirrors the C++ operation order.

An R value carries the computed number v (a Python float, or numpy longdouble
for the x87 extended intermediates of the native build) and a bound e on
|v - exact|, where 'exact' is the real-number evaluation of the same formula on
the true inputs (including data errors given at the leaves).

Rounding of one operation is accounted in one of three ways (CTX.mode):
  'actual'  the exact rounding error of this operation, from a 500-bit mpmath
            evaluation of the operation on the two computed operands. No
            assumption on libm: the error of exp, log1p, ... at this argument
            is measured against mpmath.
  'bound'   the a priori Higham-style bound u |v| per +,-,*,/ and RHO |v| per
            libm call (RHO = 1 ulp = 2u assumed for glibc exp/log/log1p/expm1).
  'none'    rounding ignored: pure propagation of the leaf errors.
Propagation through a function f uses |f'| at the computed argument times the
argument's bound (first order; the second-order term is below 1e-12 relative
in every use here).
"""
import math
import numpy as np
import mpmath as mp

mp.mp.prec = 500
D = mp.mpf

class CTX:
    mode = 'actual'
    typ = 'd'          # 'd' double, 'x' long double
    @staticmethod
    def u(): return D(2)**-53 if CTX.typ == 'd' else D(2)**-64

def mpv(v):
    """exact mpmath value of a float or longdouble"""
    if isinstance(v, (float, int, np.float64)):
        return D(float(v))
    m, e = np.frexp(v)
    return D(int(m*np.longdouble(2)**64))*D(2)**(int(e)-64)

def T(v):
    return np.longdouble(v) if CTX.typ == 'x' else float(v)

def _round_err(exact, z):
    if CTX.mode == 'none': return D(0)
    if CTX.mode == 'bound': return CTX.u()*abs(mpv(z))
    return abs(mpv(z)-exact)

def _lib_err(exact, z):
    if CTX.mode == 'none': return D(0)
    if CTX.mode == 'bound': return 2*CTX.u()*abs(mpv(z))
    return abs(mpv(z)-exact)

class R:
    __slots__ = ('v', 'e')
    def __init__(self, v, e=0):
        self.v = T(v) if not isinstance(v, (np.longdouble,)) else v
        if CTX.typ == 'd': self.v = float(self.v)
        self.e = D(e)
    @staticmethod
    def of(a): return a if isinstance(a, R) else R(a, 0)
    def __add__(s, o):
        o = R.of(o); z = s.v+o.v
        return R(z, s.e+o.e+_round_err(mpv(s.v)+mpv(o.v), z))
    __radd__ = __add__
    def __sub__(s, o):
        o = R.of(o); z = s.v-o.v
        return R(z, s.e+o.e+_round_err(mpv(s.v)-mpv(o.v), z))
    def __rsub__(s, o): return R.of(o)-s
    def __mul__(s, o):
        o = R.of(o); z = s.v*o.v
        e = abs(mpv(s.v))*o.e+abs(mpv(o.v))*s.e+s.e*o.e
        return R(z, e+_round_err(mpv(s.v)*mpv(o.v), z))
    __rmul__ = __mul__
    def __truediv__(s, o):
        o = R.of(o)
        if o.v == 0: return R(math.nan if s.v == 0 else math.copysign(math.inf, s.v), mp.inf)
        z = s.v/o.v
        ov = abs(mpv(o.v))
        e = (s.e+abs(mpv(s.v)/mpv(o.v))*o.e)/(ov-o.e)
        return R(z, e+_round_err(mpv(s.v)/mpv(o.v), z))
    def __rtruediv__(s, o): return R.of(o)/s
    def __neg__(s): return R(-s.v, s.e)
    def __abs__(s): return R(abs(s.v), s.e)
    def __gt__(s, o): return s.v > R.of(o).v
    def __ge__(s, o): return s.v >= R.of(o).v
    def __lt__(s, o): return s.v < R.of(o).v
    def __le__(s, o): return s.v <= R.of(o).v
    def __repr__(s): return 'R(%r, %s)' % (s.v, mp.nstr(s.e, 3))

def _lib(name, x):
    x = R.of(x); xv = mpv(x.v)
    if CTX.typ == 'd':
        try: z = {'exp': math.exp, 'log': math.log, 'log1p': math.log1p, 'expm1': math.expm1, 'sqrt': math.sqrt}[name](x.v)
        except OverflowError: return R(math.inf, mp.inf)       # IEEE: C++ returns inf
        except ValueError: return R(math.nan if x.v == x.v and x.v != 0 else -math.inf, mp.inf)
    else:
        z = {'exp': np.exp, 'log': np.log, 'log1p': np.log1p, 'expm1': np.expm1, 'sqrt': np.sqrt}[name](x.v)
    f, d = {'exp': (mp.exp, lambda t: mp.exp(t)), 'log': (mp.log, lambda t: 1/t), 'log1p': (mp.log1p, lambda t: 1/(1+t)),
            'expm1': (mp.expm1, lambda t: mp.exp(t)), 'sqrt': (mp.sqrt, lambda t: 1/(2*mp.sqrt(t)))}[name]
    exact = f(xv)
    err = _lib_err(exact, z) if name != 'sqrt' else _round_err(exact, z)
    return R(z, abs(d(xv))*x.e+err)

def exp(x): return _lib('exp', x)
def log(x): return _lib('log', x)
def log1p(x): return _lib('log1p', x)
def expm1(x): return _lib('expm1', x)
def sqrt(x): return _lib('sqrt', x)

def sigmoid(z):
    z = R.of(z)
    if z.v >= 0: return 1/(1+exp(-z))
    t = exp(z); return t/(1+t)

# ---- compensated sums with their own error account -------------------------

def dd_sum(items):
    """transformed_detail::Sum (double-double accumulation of doubles, then hi+lo). items: list of R (double)."""
    hi = 0.0; lo = 0.0; exact = D(0); mag = D(0); e = D(0)
    for r in items:
        r = R.of(r); x = r.v
        if not (math.isfinite(x) and math.isfinite(hi)):      # Sum::push: non-finite values are added plainly
            hi += x; lo = 0.0; exact = D(math.nan); mag = D(0); e = mp.inf; continue
        s = hi+x; v = s-hi
        ee = (hi-(s-v))+(x-v)+lo+0.0
        h = s+ee; lo = ee-(h-s); hi = h
        exact += mpv(x); mag += abs(mpv(x)); e += r.e
    z = hi+lo
    if CTX.mode == 'actual': rnd = abs(mpv(hi)+mpv(lo)-exact)      # double-double accumulation error (sign decision uses hi+lo; its rounding cannot flip a sign)
    elif CTX.mode == 'bound': rnd = len(items)*4*D(2)**-106*mag
    else: rnd = D(0)
    return R(z, e+rnd), (mpv(hi)+mpv(lo))

def kahan_sum(items):
    """the compensated mean numerator of gumbel/mle.hpp:103-107; items R of the working type."""
    s = T(0); c = T(0); e = D(0); exact = D(0); mag = D(0)
    for r in items:
        r = R.of(r); d = r.v
        y = d-c; t = s+y; c = (t-s)-y; s = t
        e += r.e; exact += mpv(d); mag += abs(mpv(d))
    n = len(items)
    if CTX.mode == 'actual': rnd = abs(mpv(s)-exact)
    elif CTX.mode == 'bound': rnd = (2*CTX.u()+4*n*CTX.u()**2)*mag
    else: rnd = D(0)
    return R(s, e+rnd)
