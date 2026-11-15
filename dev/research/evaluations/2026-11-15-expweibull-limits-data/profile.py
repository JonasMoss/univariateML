"""Profile L(b) = max over (mu, c) of the log likelihood, on a log b grid (b in z units, b_z = b*sd(log x)),
for each failing snapshot sample; plus explicit power-face and inverse-Weibull-face path checks. mpmath dps 40."""
import mpmath as mp, sys, os, json, math
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import core, sup
mp.mp.dps = 40

def Lb(y, b, tgrid):
    vals = []
    for T in tgrid:
        try: vals.append((core.lbc(y, b, mp.mpf(T))[0], T))
        except Exception: vals.append((-mp.inf, T))
    v, T = max(vals, key=lambda p: p[0])
    i = [p[1] for p in vals].index(T)
    a = tgrid[max(i-1, 0)]; c = tgrid[min(i+1, len(tgrid)-1)]
    g = (math.sqrt(5)-1)/2
    f = lambda T: core.lbc(y, b, mp.mpf(T))[0]
    x1 = c-g*(c-a); x2 = a+g*(c-a); f1 = f(x1); f2 = f(x2)
    for _ in range(40):
        if f1 > f2: c = x2; x2 = x1; f2 = f1; x1 = c-g*(c-a); f1 = f(x1)
        else: a = x1; x1 = x2; f1 = f2; x2 = a+g*(c-a); f2 = f(x2)
    T = (a+c)/2
    return f(T), T

TG = [-14,-12,-10,-8,-6,-4,-2,0,1,2,3,4,6,8,10,13,17,22,30,40,55,75,100,140,200,300,450,700,1000,1500,2500,4000,6000]
if __name__ == '__main__':
    S = sup.samples(); out = {}
    for name in sys.argv[1:] or list(S):
        x = [mp.mpf(v) for v in S[name]]; y = [mp.log(v) for v in x]; n = len(y)
        ybar = sum(y)/n; sd = mp.sqrt(sum((v-ybar)**2 for v in y)/n)
        P, I, k, ls = core.faces(y)
        rows = []
        for lb in [-8, -6, -4, -3, -2, -1, 0, 1, 2, 3, 4, 6, 8, 10, 12]:
            b = mp.e**lb/sd
            v, T = Lb(y, b, TG)
            rows.append((lb, float(v-max(P, I)), float(T)))
        out[name] = dict(P=float(P), I=float(I), rows=rows)
        print(name, 'P=%.6f I=%.6f' % (P, I))
        print('   lb:rel-to-max(P,I)@T  ' + '  '.join('%d:%.2e@%.0f' % r for r in rows), flush=True)
    json.dump(out, open(os.path.join(os.path.dirname(os.path.abspath(__file__)), 'profile.json'), 'w'))
