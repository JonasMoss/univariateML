"""(1) leading-order loss of the power face, checked on 'two' and 'base6' at b = e^8 .. e^16 (y scale, b_y).
(2) two-point regime map in p = share at the lower value: P, I, crossover p_c, A(p), and the finite supremum from
    a (log b, T) grid with exact mu, n = 20 representatives."""
import mpmath as mp, sys, os, math
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import core, twopoint, profile
mp.mp.dps = 40
def power_loss(xs):
    y = [mp.log(mp.mpf(v)) for v in xs]; n = len(y)
    P, I, k, ls = core.faces(y)
    yp = max(y); m = sum(1 for v in y if v == yp); Dp = sum(yp-v for v in y); kap = n/Dp
    print('power-face loss, n=%d, m=%d ties at max, kappa=%s, P=%s' % (n, m, mp.nstr(kap, 8), mp.nstr(P, 12)))
    for e in (6, 8, 10, 12, 14, 16):
        b = mp.e**e
        # maximise over T near log(kappa/b)
        T0 = float(mp.log(kap/b)); tg = [T0+d for d in (-3, -2, -1.5, -1, -.5, 0, .5, 1, 1.5, 2, 3)]
        v, T = profile.Lb(y, b, tg)
        loss = P - v
        pred = (n*kap/b)*(1+mp.log(m*b/(2*n*kap)))
        print('  b=e^%d  loss=%s  predicted=%s  ratio=%s  T=%.3f (log kappa/b=%.3f)' % (e, mp.nstr(loss, 6), mp.nstr(pred, 6), mp.nstr(loss/pred, 6), T, T0), flush=True)
if __name__ == '__main__':
    power_loss(['1.8274133492616271', '3.487363739010494'])
    power_loss(['1.219444897201475','1.8274133492616271','2.698972628080099','2.941647829758515','3.487363739010494','4.474230175167811'])
    # p_c
    f = lambda p: twopoint.Pface(p) - twopoint.Iface(p)[0]
    lo, hi = mp.mpf('0.5'), mp.mpf('0.9')
    for _ in range(100):
        mid = (lo+hi)/2
        if f(mid) > 0: lo = mid
        else: hi = mid
    pc = (lo+hi)/2
    print('p_c (P = I) =', mp.nstr(pc, 15))
    # A(p): sign change
    lo, hi = mp.mpf('0.3'), mp.mpf('0.7')
    print('A(0.5) =', mp.nstr(twopoint.A_of(mp.mpf(1)/2)[0], 5), ' A(0.49)=', mp.nstr(twopoint.A_of(mp.mpf('0.49'))[0], 5), ' A(0.51)=', mp.nstr(twopoint.A_of(mp.mpf('0.51'))[0], 5))
    # supremum of finite likelihood for two-point samples (y in {0, 1}, rescaled): n=20, lower share p
    for m in (4, 6, 8, 10, 12, 13, 14, 16, 18):
        p = mp.mpf(m)/20; y = [mp.mpf(0)]*m + [mp.mpf(1)]*(20-m); n = 20
        P, I, k, ls = core.faces(y)
        sd = mp.sqrt(p*(1-p))
        best = (-mp.inf, None, None)
        for lb in [-8, -6, -4, -3, -2, -1, 0, 1, 2, 3, 4, 6, 8, 10, 12]:
            v, T = profile.Lb(y, mp.e**lb/sd, profile.TG)
            if v > best[0]: best = (v, lb, T)
        print('p=%.2f  P/n=%s I/n=%s  max over grid of L/n = %s (lb=%d T=%.1f)  rel to max(P,I) = %s' % (float(p), mp.nstr(P/n, 8), mp.nstr(I/n, 8), mp.nstr(best[0]/n, 8), best[1], best[2], mp.nstr((best[0]-max(P, I))/n, 4)), flush=True)
