"""Global search of the expweibull likelihood for the failing snapshot samples (mpmath, dps 40).
For each sample: face values P (power) and I (inverse Weibull), first-order coefficient A at the I optimum,
then a (log b, T=log c) grid with mu solved exactly (unique root), and Nelder-Mead refinement from the best cells.
b is measured in z units: b_z = b * sd(log x)."""
import mpmath as mp, sys, os, json, math
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import core
from scipy.optimize import minimize
mp.mp.dps = 40
ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), '../../../..'))
sys.path.insert(0, ROOT+'/dev/validation')
import strict_common as sc
SM = sc.snapshot_module()

def samples():
    out = {}
    for c in SM.cases('expweibull', {}):
        if c['source'] in ('class', 'resample') and c['case'] not in ('empty','negative','zeros','nan','inf','neginf','one','constant'):
            out[c['case']] = c['data']
    return out

def analyse(name, xs, bgrid=None, Tgrid=None):
    x = [mp.mpf(v) for v in xs]; y = [mp.log(v) for v in x]; n = len(y)
    ybar = sum(y)/n; sd = mp.sqrt(sum((v-ybar)**2 for v in y)/n)
    P, I, k, ls = core.faces(y)
    A = core.Acoef(y, k, ls)
    # float-safe wrapper: parameters (log b_z, T)
    def f(lb, T):
        if not (-8 <= lb <= 9 and -12 <= T <= 6000): return -mp.inf, None
        b = mp.e**mp.mpf(lb)/sd
        try: v, mu = core.lbc(y, b, mp.mpf(T))
        except Exception: return -mp.inf, None
        return v, mu
    bg = bgrid or [math.log(0.01)+i*(math.log(1000)-math.log(0.01))/15 for i in range(16)]
    Tg = Tgrid or ([-8, -4, -1, 0, 1, 2, 4, 6, 9, 13, 18, 25, 35, 50, 75, 110, 170, 300, 600, 1200, 2400])
    cells = []
    for lb in bg:
        for T in Tg:
            v, mu = f(lb, T)
            cells.append((float(v) if v != -mp.inf else -1e300, lb, T))
    cells.sort(reverse=True)
    best = cells[0]
    refined = []
    for v0, lb0, T0 in cells[:2]:
        res = minimize(lambda z: -float(f(z[0], z[1])[0]), [lb0, T0], method='Nelder-Mead', options=dict(xatol=1e-6, fatol=1e-13, maxiter=150))
        refined.append((-res.fun, float(res.x[0]), float(res.x[1])))
    refined.sort(reverse=True)
    rb = refined[0]
    return dict(name=name, n=n, P=float(P), I=float(I), k=float(k), A=float(A), faces_max=float(max(P, I)),
                grid_best=best, refined_best=rb,
                gap_to_faces=rb[0]-float(max(P, I)), edge_b=(bg[0], bg[-1]), edge_T=(Tg[0], Tg[-1]),
                sd=float(sd) if sd > 1e-300 else str(sd))

if __name__ == '__main__':
    want = sys.argv[1:]
    S = samples(); S['golden_like_two_p23'] = None
    res = []
    for name, d in S.items():
        if d is None or (want and name not in want): continue
        r = analyse(name, d); res.append(r)
        print(json.dumps(r), flush=True)
