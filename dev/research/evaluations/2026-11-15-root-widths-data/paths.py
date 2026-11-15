"""Emulations of the five root finders, with error accounting (rea.py).

Every function takes the data as the C++ sees it and returns a dict with the
emulated returned coefficient (to compare bit for bit with the snapshot), the
exact root from the true data, and the width terms. Data: x (floats). For the
frechet hold, the C++ sees fl(x - mu); `exact` carries the true x - mu.
"""
import math
import numpy as np
import mpmath as mp
import rea
from rea import R, D, CTX, mpv, exp, log, log1p, expm1, sqrt, sigmoid, dd_sum, kahan_sum

EPS = 2.0**-52

def log_ratio(x, anchor):
    d = (x-anchor)/anchor
    if abs(d.v) < .5: return log1p(d)
    return log(x)-log(anchor)

def ulp(z):
    z = abs(float(z))
    return 2.0**(math.floor(math.log2(z))-52) if z > 0 else 5e-324

def true_root(S, z0):
    """root of the decreasing/increasing function S (mp) near z0, Newton with numeric derivative"""
    z = D(z0)
    for _ in range(40):
        s = S(z); d = mp.diff(S, z)
        step = s/d; z -= step
        if abs(step) < D(10)**-45: break
    return z, mp.diff(S, z)

# ------------------------------------------------------------------ location_fit, scale held, free power

def location_scale_held(xs, xexact, scale, gumbel, tol=1e-10, maxit=2000, coord='b'):
    """core/transformed_location.hpp:30-70 (free power, held scale), a = 1, sign = +1. Returns dict."""
    CTX.typ = 'd'; CTX.mode = 'actual'
    n = float(len(xs)); a = 1.0
    leaves = [R(v, abs(D(v)-ex)) for v, ex in zip(xs, xexact)]
    x0 = leaves[0]
    delta = [log_ratio(v, x0) for v in leaves]
    unit = max(abs(d.v) for d in delta)
    held_shift = log_ratio(R(scale), x0)
    if unit == 0:
        unit = abs(held_shift.v)
        if unit == 0: unit = 1.0
    U = R(unit)
    y = [(d*1.0)/U for d in delta]
    hs = held_shift/U
    def score(z, y, hs, held_shift, mode_arith=None):
        k = exp(R.of(z)) if coord == 'b' else exp(R.of(z))*U     # coord 'kappa': the solved coordinate is log of the public power
        c = (k*held_shift)/U
        terms = [R(n)]
        for v in y:
            u = k*v-c
            coef = k*(v-hs)
            if gumbel: t = expm1(-u)
            else: t = 1-(a+1)*sigmoid(u)
            terms.append(coef*t)
        s, _ = dd_sum(terms)
        score.last_mag = sum(abs(t.v) for t in terms[1:])
        return s
    S = lambda z: score(z, y, hs, held_shift).v
    # root(): transformed_detail::root
    lo, hi = -1.0, 1.0; nit = 0
    while nit < maxit and S(lo) < 0: lo *= 2; nit += 1
    nit = 0
    while nit < maxit and S(hi) > 0: hi *= 2; nit += 1
    assert S(lo) >= 0 and S(hi) <= 0
    width = min(tol/16, 4*EPS)
    t = None
    for j in range(maxit):
        m = lo+(hi-lo)/2; g = S(m)
        if g == 0 or m == lo or m == hi: t = m; break
        if g > 0: lo = m
        else: hi = m
        if hi-lo <= width: t = lo+(hi-lo)/2; break
    b = exp(R(t)); power = b/U if coord == 'b' else b
    out = dict(z=t, shape=power.v, unit=unit, final_bracket=(lo, hi))
    # ---- error terms at z = t
    zz = R(t)
    sc = lambda yy, hh, ss: score(zz, yy, hh, ss)
    yd = y; hd = hs
    CTX.mode = 'none'; Edata = sc(yd, hd, held_shift).e      # leaf errors propagated, no new rounding
    yz = [R(v.v) for v in y]; hz = R(hs.v); sz = R(held_shift.v)
    CTX.mode = 'actual'; Earith = sc(yz, hz, sz).e
    CTX.mode = 'bound'; Ebound = sc(yz, hz, sz).e; mag = score.last_mag
    # the data error budget by the bound mode of the stage 1 is not separate: report stage-1 errors as is
    CTX.mode = 'actual'
    # ---- true problem
    W = [mp.log(ex/D(scale)) for ex in xexact]; nn = D(len(xs)); Dd = D(unit)
    def Strue(zv):
        kap = mp.exp(zv)/Dd
        if gumbel: return nn+sum(kap*w*mp.expm1(-kap*w) for w in W)
        return nn+sum(kap*w*(1-2/(1+mp.exp(-kap*w))) for w in W)
    tb = t if coord == 'b' else mp.mpf(t)+mp.log(Dd)
    zs, slope = true_root(Strue, tb)
    # the represented problem (y_i, held shift as stored), exact arithmetic: its root differs from zs by the data term
    Yh = [mpv(v.v) for v in y]; Hh = mpv(held_shift.v)/Dd
    def Srep(zv):
        kk_ = mp.exp(zv)
        if gumbel: return nn+sum(kk_*(v-Hh)*mp.expm1(-kk_*(v-Hh)) for v in Yh)
        return nn+sum(kk_*(v-Hh)*(1-2/(1+mp.exp(-kk_*(v-Hh)))) for v in Yh)
    zr, _ = true_root(Srep, tb)
    out['data_actual'] = abs(zr-zs)
    kstar = mp.exp(zs)/Dd
    off = mp.log(mpv(power.v)/kstar)
    conv_actual = abs(mp.log(mpv(power.v)/(mp.exp(mpv(t))/Dd))) if coord == 'b' else abs(mp.log(mpv(power.v)/mp.exp(mpv(t))))
    out.update(dict(mag=mag, offset=off, zstar=zs, slope=slope, E_data=Edata, E_arith=Earith, E_arith_bound=Ebound,
                    conv_actual=conv_actual, conv_bound=D(2)**-52+D(2)**-53, kappa_star=kstar,
                    half=max(2*EPS, ulp(t)), hi_lo=hi-lo, S_at_t=S(t)))
    return out


# ------------------------------------------------------------------ weibull free: log transform + gumbel scale bracket

def cast(r):
    return R(r.v, r.e)

def weibull_free(x, ext, tol=1e-10, maxit=2000, ratio_fix=False):
    """weibull/mle.hpp:48-76 -> gumbel/mle.hpp:57-122 (exponential_location = true). ext 'x' native (long double), 'd' double build."""
    CTX.typ = 'd'; CTX.mode = 'actual'
    n = len(x); dn = float(n)
    leaves = [R(v) for v in x]
    anchor = leaves[0]; log_anchor = log(anchor)
    log_x = []; sum_log_x = R(0.0)
    for v in leaves:
        rel = (v-anchor)/anchor
        if abs(rel.v) < .5: l = -log1p(rel)
        elif ratio_fix and 1e-300 < v.v/anchor.v < 1e300: l = -log(v/anchor)      # sketch: log of the rounded ratio instead of a difference of logs
        else: l = -(log(v)-log_anchor)
        log_x.append(l); sum_log_x = sum_log_x+l
    center = (-sum_log_x)/dn
    y = [v+center for v in log_x]           # double, with leaf errors e_y
    yv = [v.v for v in y]
    # ---- stage 2 in type ext
    CTX.typ = ext
    ys = [cast(v) for v in y]
    var0_in = None
    # two-pass variance for the starting hi (double arithmetic in the C++)
    xbar = sum(yv)/dn
    var0 = 0.0
    for v in yv: var0 += (v-xbar)*(v-xbar)
    var0 /= dn
    minimum = min(yv)
    def build(leaf):
        mn = min(v.v for v in leaf)
        draw = [(v-R(mn)) for v in leaf]
        sm = kahan_sum(draw)/dn
        def eq(scale, loc=False):
            sc = R(scale)
            w = R(0.0); first = R(0.0)
            for v in leaf:
                d = v-R(mn)
                e = exp((-d)/sc)
                w = w+e; first = first+d*e
            F = (sc-sm)+first/w
            if not loc: return F
            wd = float(w.v/dn)
            lc = mn-scale*math.log(wd)
            return F, lc
        return eq
    eq = build(ys)
    lo, hi = 0.0, math.sqrt(var0); it = 0
    while it < maxit and eq(hi).v <= 0: hi *= 2; it += 1
    acc = min(.01*tol, max(.0001*tol, 4*EPS))
    conv = False
    for it in range(it, maxit):
        mid = lo+(hi-lo)/2
        if eq(mid).v > 0: hi = mid
        else: lo = mid
        if lo > 0: ml = eq(lo, True)[1]; mh = eq(hi, True)[1]    # at lo = 0 the C++ evaluates 0/0 and ignores it
        if lo > 0 and hi-lo <= acc*lo and abs(mh-ml) <= acc*min(1.0, lo): conv = True; break
    assert conv
    sigma = lo+(hi-lo)/2
    shape = 1.0/sigma
    out = dict(sigma=sigma, shape=shape, final_rel_bracket=(hi-lo)/lo)
    # ---- error terms at sigma
    CTX.mode = 'none'; Edata = eq(sigma).e
    CTX.mode = 'actual'
    eq0 = build([R(v.v) for v in ys]); Earith = eq0(sigma).e
    CTX.mode = 'bound'; eq0b = build([R(v.v) for v in ys]); Ebound = eq0b(sigma).e
    # ---- true problem: d_i = log(xmax) - log(x_i), exact data
    CTX.typ = 'd'
    X = [D(v) for v in x]; L = [mp.log(v) for v in X]; ltop = max(L)
    d = [ltop-l for l in L]; dm = sum(d)/n
    def Ft(s):
        e = [mp.exp(-v/s) for v in d]
        return s-dm+sum(v*q for v, q in zip(d, e))/sum(e)
    sstar, Fp = true_root(Ft, sigma)
    Yh = [mpv(v) for v in yv]; mnh = min(Yh); dh = [v-mnh for v in Yh]; dmh = sum(dh)/n
    def Fr(s_):
        e = [mp.exp(-v/s_) for v in dh]
        return s_-dmh+sum(v*q for v, q in zip(dh, e))/sum(e)
    srep, _ = true_root(Fr, sigma)
    out['data_actual'] = abs(srep/sstar-1)
    off = (D(1)/mpv(shape))/sstar-1
    conv_actual = abs(D(1)/mpv(shape)/mpv(sigma)-1)
    out.update(dict(offset=off, sstar=sstar, slope_rel=Fp*sstar, E_data=Edata, E_arith=Earith, E_arith_bound=Ebound,
                    half=acc/2, conv_actual=conv_actual, conv_bound=D(2)**-53*2, y_err_max=max(v.e for v in y),
                    y_err_actual=max(abs(mpv(v.v)-(-L[i]+(-0)) ) for i, v in enumerate(y)) if False else None))
    # actual represented-data error: differences of the stored y against exact differences, vs. the largest y
    ymax = max(range(n), key=lambda i: yv[i]) 
    out['d_err_actual'] = max(abs((mpv(yv[i])-mpv(yv[ymax]))-(-(L[i]-L[ymax]))) for i in range(n))
    # data-only shift of the root (first-order from E_data)
    return out


# ------------------------------------------------------------------ gamma / naka: bracket on log k - psi(k) = s

PSI_REL = D(2)**-51      # ASSUMED relative error of boost::math::digamma (4 u), used where k < 100

def pairval(mpvalue):
    """a transformed_detail::Pair (error about 1e-32 relative) rounded by value(): the double nearest, with its rounding error"""
    v = float(mpvalue)
    if CTX.mode == 'none': return R(v, 0)
    e = CTX.u()*abs(mpvalue) if CTX.mode == 'bound' else abs(D(v)-mpvalue)
    return R(v, e)

def lmd(k):
    """gamma_detail::log_minus_digamma(k), k an R (exact leaf). Returns R and the truncation bound."""
    kv = k.v
    if kv >= 100.0:
        r = 1.0/k; r2 = r*r
        val = r*(0.5+r*(1.0/12.0-r2*(1.0/120.0-r2*(1.0/252.0-r2/240.0))))
        trunc = 1/(132*mpv(kv)**10)
        return val, trunc
    psi = mp.digamma(mpv(kv))
    ps = R(float(psi), (abs(psi)*PSI_REL) if CTX.mode != 'none' else 0)
    return log(k)-ps, D(0)

def phi_series(t, ratio):
    if abs(t.v) < 0.1:
        term = t*t; sm = R(0.0)
        for j in range(2, 60):
            add = term/float(j)
            sm = sm+add if j % 2 == 0 else sm-add
            if abs(add.v) <= 1e-17*abs(sm.v): break
            term = term*t
        return sm, D(abs(float(add.v)))      # truncation: the first omitted term is bounded by the last added
    return t-log(ratio), D(0)

def gamma_bracket(x, tol=1.220703e-4, maxit=2000):
    """gamma_mle_impl free/free, bracket path (gamma/mle.hpp:150-211). Returns None if the Newton path or a range path applies."""
    CTX.typ = 'd'; CTX.mode = 'actual'
    n = len(x); dn = float(n)
    sm = 0.0
    for v in x: sm += v
    mean_hat = sm/dn
    if not math.isfinite(mean_hat): return None      # overflowing sum: range path, not this claim
    slog = 0.0
    for v in x: slog += math.log(v)
    s_dir = math.log(mean_hat)-slog/dn
    if s_dir >= 1e-3 and math.isfinite(s_dir) and mean_hat >= 2.2250738585072014e-308: return None
    c = R(mean_hat)
    sum_phi = R(0.0); sum_d = R(0.0); trunc_total = D(0)
    for v in x:
        ratio = R(v)/c; t = (R(v)-c)/c
        sum_d = sum_d+t
        p, tr = phi_series(t, ratio); sum_phi = sum_phi+p; trunc_total += tr
    md = sum_d/dn
    p, tr = phi_series(md, 1.0+md)
    s = sum_phi/dn-p
    s_trunc = trunc_total/n+tr
    if not (s.v > 0): return None
    lo, hi = 0.25/s.v, 2.0/s.v; iters = 0
    while iters < maxit and hi/lo-1.0 > 4.0*EPS:
        mid = math.sqrt(lo)*math.sqrt(hi)
        g, _ = lmd(R(mid))
        if g.v > s.v: lo = mid
        else: hi = mid
        iters += 1
    shape = math.sqrt(lo)*math.sqrt(hi)
    out = dict(shape=shape, final_rel_bracket=hi/lo-1)
    # error terms at k = shape (the sign decisions were taken at the bracket ends, shape +- 2 eps)
    kk = R(shape)
    def errs(mode):
        CTX.mode = mode
        g, tr = lmd(kk)
        # s again under this mode
        sp = R(0.0); sd = R(0.0)
        for v in x:
            ratio = R(v)/c; t = (R(v)-c)/c
            sd = sd+t; pp, _ = phi_series(t, ratio); sp = sp+pp
        mdd = sd/dn; pp, _ = phi_series(mdd, 1.0+mdd); ss = sp/dn-pp
        return g.e, ss.e, tr
    eg_a, es_a, tr = errs('actual'); eg_b, es_b, _ = errs('bound'); CTX.mode = 'actual'
    X = [D(v) for v in x]
    s_true = mp.log(sum(X)/n)-sum(mp.log(v) for v in X)/n
    Gt = lambda z: (lambda k: mp.log(k)-mp.digamma(k)-s_true)(mp.exp(z))
    zs, slope = true_root(Gt, mp.log(D(shape)))
    kstar = mp.exp(zs)
    off = D(shape)/kstar-1
    ex = mp.sqrt(D(lo))*mp.sqrt(D(hi))
    out.update(dict(offset=off, kstar=kstar, slope=abs(slope), E_G_actual=eg_a, E_s_actual=es_a, E_G_bound=eg_b, E_s_bound=es_b,
                    E_trunc=tr+s_trunc, half=2*EPS, conv_actual=abs(D(shape)/ex-1), conv_bound=3*D(2)**-53, s=s.v, s_true=s_true))
    return out

def naka_square_held(x, omega, tol=1.220703e-4, maxit=2000):
    """gamma_raw, map Square, free shape, parameter held (gamma/transformed.hpp:21-90,59-61)."""
    CTX.typ = 'd'; CTX.mode = 'actual'
    n = len(x); dn = float(n)
    anchor = max(x); X = [D(v) for v in x]
    la = mp.log(D(anchor))
    def build(mode):
        CTX.mode = mode
        log_anchor_v = pairval(la)
        log_yref = log_anchor_v*2
        delta = []; logdelta = []
        for v in x:
            r = (R(v)-anchor)/anchor
            l = log_ratio(R(v), R(anchor))
            delta.append(r*(2+r)); logdelta.append(l*2)
        yref_m = D(anchor)**2
        offset = pairval((yref_m-D(omega))/D(omega))
        s = R(0.0); tr_tot = D(0)
        for i in range(n):
            u = offset+(1+offset)*delta[i]
            ratio = exp(log_yref-log(R(omega))+logdelta[i])
            p, tr = phi_series(u, ratio); tr_tot += tr
            s = s+p/dn
        return s, tr_tot/n
    s, _ = build('actual')
    s_v = s.v
    if not (s_v > 0): return None
    lo, hi = .25/s_v, 2/s_v
    mid = math.sqrt(lo)*math.sqrt(hi); iters = 0
    eps = 2.0**-52
    for iters in range(maxit):
        g, _ = lmd(R(mid)); f = g.v-s_v
        if f == 0: lo = hi = mid; break
        if f > 0: lo = mid
        else: hi = mid
        if abs(f) < 64*eps*s_v:
            radius = 8*eps*mid
            left = max(lo, mid-radius); right = min(hi, mid+radius)
            if lmd(R(left))[0].v >= s_v: lo = left
            if lmd(R(right))[0].v <= s_v: hi = right
        if hi/lo-1 <= min(tol/8, 4*eps): break
        r = 1/mid; r2 = r*r
        deriv = (-r*(.5+r*(1./6-r2*(1./30-r2*(1./42-r2/30))))) if mid >= 100 else float(1-mpv(mid)*mp.psi(1, mpv(mid)))
        prop = mid*math.exp(-f/deriv)
        mid = prop if (prop > lo and prop < hi and math.isfinite(prop)) else math.sqrt(lo)*math.sqrt(hi)
    shape = math.sqrt(lo)*math.sqrt(hi)
    out = dict(shape=shape, final_rel_bracket=hi/lo-1)
    kk = R(shape)
    res = {}
    for mode in ('actual', 'bound'):
        CTX.mode = mode
        g, tr = lmd(kk); ss, st = build(mode)
        res[mode] = (g.e, ss.e, tr, st)
    CTX.mode = 'actual'
    s_true = mp.log(D(omega))-sum(mp.log(v*v) for v in X)/n+sum(v*v for v in X)/n/D(omega)-1
    Gt = lambda z: (lambda k: mp.log(k)-mp.digamma(k)-s_true)(mp.exp(z))
    zs, slope = true_root(Gt, mp.log(D(shape)))
    kstar = mp.exp(zs); off = D(shape)/kstar-1
    ex = mp.sqrt(D(lo))*mp.sqrt(D(hi))
    out.update(dict(offset=off, kstar=kstar, slope=abs(slope), E_G_actual=res['actual'][0], E_s_actual=res['actual'][1],
                    E_G_bound=res['bound'][0], E_s_bound=res['bound'][1], E_trunc=res['actual'][2]+res['actual'][3],
                    half=2*EPS, conv_actual=abs(D(shape)/ex-1), conv_bound=3*D(2)**-53, s=s_v, s_true=s_true))
    return out
