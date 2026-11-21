"""Independent numerical references using exact binary64 inputs at 80 digits."""
from pathlib import Path
import gzip
import importlib.util
import json

import mpmath as m
import sympy as sym

m.mp.dps = 80
ROOT = Path(__file__).resolve().parents[4]
spec = importlib.util.spec_from_file_location("snapshot", ROOT / "dev/validation/fit-snapshot.py")
snapshot = importlib.util.module_from_spec(spec)
spec.loader.exec_module(snapshot)
with gzip.open(snapshot.ARCHIVE, "rt") as archive:
    cases = {row["index"]: row for row in json.load(archive)}


def inputs(index):
    row = cases[index]
    return list(map(m.mpf, row["data"])), {k: m.mpf(v) for k, v in row["held"].items()}


def show(label, *values):
    print(label, *(m.nstr(value, 30) for value in values), flush=True)


def normal_interval(lo, hi):
    if hi <= 0:
        return normal_interval(-hi, -lo)
    if lo >= 0:
        return (m.erfc(lo/m.sqrt(2)) - m.erfc(hi/m.sqrt(2)))/2
    return (m.erf(hi/m.sqrt(2)) - m.erf(lo/m.sqrt(2)))/2


for index in (364, 428):
    x, held = inputs(index)
    sigma = held["sigma"]

    def loglik(mu, width):
        return sum(m.log(normal_interval((v-mu-width)/sigma, (v-mu+width)/sigma)) for v in x) - len(x)*m.log(2*width)

    if index == 364:
        start = ((min(x)+max(x))/2, (max(x)-min(x))/2+5)
        mu, width = m.findroot(lambda u, a: (m.diff(lambda z: loglik(z,a),u),
                                             m.diff(lambda z: loglik(u,z),a)), start)
    else:
        width = held["a"]
        mu = m.findroot(lambda u: m.diff(lambda z: loglik(z,width),u), sum(x)/len(x))
    show(str(index), mu, sigma, width, loglik(mu,width))
    curvature = m.diff(lambda u: loglik(u,width),mu,2)
    if index == 364:
        mixed = m.diff(loglik,(mu,width),(1,1))
        width_curvature = m.diff(lambda a: loglik(mu,a),width,2)
        show("364 Hessian location and determinant", curvature, curvature*width_curvature-mixed*mixed)
    else:
        show("428 location curvature", curvature)


def gamma_loglik(x, shape, rate):
    # Integrate over finite cells. Around shape 10^6 the incomplete gamma
    # routine can require a long series even though the cell is smooth.
    def cell(v):
        if not v:
            return m.log(m.gammainc(shape, 0, rate, regularized=True))
        midpoint = (v+m.mpf(".5"))*rate
        mass = m.quadgl(lambda t: m.exp((shape-1)*m.log1p(t/midpoint)-t), [-rate/2, rate/2])
        return (shape-1)*m.log(midpoint)-midpoint-m.loggamma(shape)+m.log(mass)

    return sum(map(cell, x))


x, held = inputs(1025)
rate = held["rate"]
shape = m.findroot(lambda a: m.diff(lambda z: gamma_loglik(x,z,rate),a), 972057)
show("1025", shape, rate, gamma_loglik(x,shape,rate))

x = [0, 1, 1000, 1000000]
shape, rate = m.findroot(lambda a,b: (m.diff(lambda z: gamma_loglik(x,z,b),a),
                                      m.diff(lambda z: gamma_loglik(x,a,z),b)), (.081, 3.25e-7))
show("dgamma wide", shape, rate, gamma_loglik(x,shape,rate))

M = m.sqrt(2/m.pi)
H = 1-M*M


def snorm_faces(x, held):
    n = len(x)
    for side in (-1, 1):
        y = sorted(side*v for v in x)
        if "sd" in held:
            scale = held["sd"]/m.sqrt(H)
            location = y[0]
        elif "mean" in held:
            mean = side*held["mean"]
            data = [v-mean for v in y]
            b = M*sum(data)
            scale = max(-data[0]/M, (b+m.sqrt(b*b+4*n*sum(v*v for v in data)))/(2*n))
            location = mean-M*scale
        else:
            location = y[0]
            scale = m.sqrt(sum((v-location)**2 for v in y)/n)
        ll = n*m.log(M/scale)-sum(((v-location)/scale)**2 for v in y)/2
        yield side, side*location, scale, ll


def snorm_profile(x, held, ratio):
    n = len(x)
    side_sd = m.sqrt(H*(1+ratio*ratio)+(2*M*M-1)*ratio)
    a, b = ratio/side_sd, 1/side_sd
    if "mean" in held:
        x = sorted(v-held["mean"] for v in x)
        w = M*(b-a)
        choices = []
        for k in range(n+1):
            weights = [1/a**2]*k+[1/b**2]*(n-k)
            C = sum(v*v*t for v,t in zip(x,weights))
            E = w*sum(v*t for v,t in zip(x,weights))
            disc = m.sqrt(E*E+4*n*C)
            sd = (disc+E)/(2*n) if E>=0 else 2*C/(disc-E)
            mode = -w*sd
            if (k==0 or x[k-1]<=mode) and (k==n or mode<=x[k]):
                choices.append((mode, sd))
    elif "sd" in held:
        x = sorted(x)
        choices = []
        sd = held["sd"]
        for k in range(n+1):
            weights = [1/a**2]*k+[1/b**2]*(n-k)
            mode = sum(v*t for v,t in zip(x,weights))/sum(weights)
            if (k==0 or x[k-1]<=mode) and (k==n or mode<=x[k]):
                choices.append((mode, sd))
    else:
        raise ValueError("conditional profile requires a held moment")
    return max(n*m.log(M/(sd*(a+b)))-
               sum(((v-mode)/(sd*(a if v<mode else b)))**2 for v in x)/2
               for mode,sd in choices)


for index in (6576, 6587, 6590, "huge"):
    if index == "huge":
        meta, golden = snapshot.read("snorm")
        x, held = list(map(m.mpf,snapshot.classes(meta,golden)["huge"])), {}
    else:
        x, held = inputs(index)
    faces = list(snorm_faces(x,held))
    for face in faces:
        show(f"snorm {index} face", *face)
    show(f"snorm {index} right minus left", faces[1][-1]-faces[0][-1])
    if held:
        likelihood, power = max((snorm_profile(x,held,m.exp(m.mpf(k)/8)), k)
                                for k in range(-256, 257))
        show(f"snorm {index} grid maximum (log ratio, logLik)", m.mpf(power)/8, likelihood)
    else:
        unit = max(x)-min(x)
        y = [(v-min(x))/unit for v in x]
        # Integer-polynomial root isolation in rational coordinates, followed
        # by independently evaluating the mode profile at 80 digits.
        def profile(c):
            a = sum((v-c)**2 for v in y if v<c)**(m.mpf(1)/3)
            b = sum((v-c)**2 for v in y if v>=c)**(m.mpf(1)/3)
            return len(y)*(m.log(M/unit)-m.mpf(3)/2*m.log(a+b)+m.log(len(y))/2-m.mpf(".5"))

        exact = [sym.Rational(float(v)) for v in sorted(x)]
        rational = [(v-exact[0])/(exact[-1]-exact[0]) for v in exact]
        c = sym.Symbol("c")
        points = set(rational)
        for k in range(1, len(rational)):
            a = sum((c-v)**2 for v in rational[:k])
            b = sum((v-c)**2 for v in rational[k:])
            polynomial = sym.Poly(sym.diff(a,c)**3*b**2+sym.diff(b,c)**3*a**2,c)
            for (lo,hi),_ in polynomial.intervals(eps=sym.Rational(1,10**65),
                    inf=rational[k-1],sup=rational[k]):
                points.add((lo+hi)/2)
        modes = [m.mpf(int(v.p))/int(v.q) for v in points]
        likelihood, mode = max((profile(v), v) for v in modes)
        show("snorm huge isolated candidate count", len(modes))
        show("snorm huge maximum (mode, logLik)", mode, likelihood)


x, held = inputs(7040)
omega, alpha = held["omega"], held["alpha"]


def student_t_cdf(x, nu):
    small = m.betainc(nu/2, m.mpf(".5"), 0, nu/(nu+x*x), regularized=True)/2
    return 1-small if x>=0 else small


def st_loglik(location, nu):
    z = [(v-location)/omega for v in x]
    return len(x)*(m.loggamma((nu+1)/2)-m.loggamma(nu/2)-m.log(omega*m.sqrt(m.pi*nu))) + sum(
        -(nu+1)/2*m.log1p(v*v/nu)
        + m.log(2*student_t_cdf(alpha*v*m.sqrt((nu+1)/(nu+v*v)), nu+1))
        for v in z)


xi = m.findroot(lambda t: m.diff(lambda u: st_loglik(u,1),t), (.04,.06))
show("st 7040", xi, omega, alpha, 1, st_loglik(xi,1),
     m.diff(lambda nu: st_loglik(xi,nu),1))
show("st skew-normal upper", len(x)*m.log(2/(omega*m.sqrt(2*m.pi)))-(max(x)-min(x))**2/(4*omega**2))
for nu in (1, 1.001, 1.01, 1.1, 1.5, 2, 4, 16, 100, 10000):
    location = m.findroot(lambda t: m.diff(lambda u: st_loglik(u,nu),t), (.04,.15))
    show("st 7040 profile", nu, location, st_loglik(location,nu))
