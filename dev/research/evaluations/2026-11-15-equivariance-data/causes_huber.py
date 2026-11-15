"""huber (epsilon held 1.345): huber_nuisance_upper at the public (rounded) coefficients,
mle.hpp lines 570-614, for the near-constant sample."""
import sys, math, json
sys.path.insert(0, 'dev/validation')
import strict_common as sc
D = 'dev/research/evaluations/2026-11-15-fit-snapshot-data/'
rows = sc.load(D + 'native.jsonl.gz')
r3 = rows[('huber', 'class', 'tie3')]; rn = rows[('huber', 'class', 'near-constant')]
x = rn['data']; x3 = r3['data']; eps = 1.345; n = 3
mu3 = sc.number(r3['result']['estimates']['mu']); s3 = sc.number(r3['result']['estimates']['sigma'])
# huber_fixed_mle: center = median, unit = max |y - center|, standardised solve, then restoration
center = sorted(x)[1]; unit = max(abs(x[0]-center), abs(x[2]-center))
# standardised optimum equals the tie3 one in units of (x[2]-x[0]):
m = (mu3-x3[0])/(x3[2]-x3[0]); s = s3/(x3[2]-x3[0])      # in units of the range (unit)
mu_pub = center+unit*m; sigma_pub = unit*s
psi = lambda z: max(-eps, min(eps, z))
def upper(mu, sigma):
    g = h = 0.0; lo, hi = min(x), max(x)
    for v in x:
        z = (v-mu)/sigma; g += z*psi(z); h += psi(z)
    move = h*((hi if h > 0 else lo)-mu)/sigma
    k = g-move
    return dict(g=g, h=h, move=move, k=k, upper='inf (k <= 0)' if not k > 0 else None)
res = dict(center=center, unit=unit, std_mu_in_units=m, std_sigma_in_units=s, mu_public=mu_pub,
           mu_public_minus_center_in_ulps=(mu_pub-center)/unit, sigma_public=sigma_pub,
           z_at_public=[(v-mu_pub)/sigma_pub for v in x], nuisance_upper_terms=upper(mu_pub, sigma_pub))
print(json.dumps(res, indent=1))
json.dump(res, open('dev/research/evaluations/2026-11-15-equivariance-data/causes_huber.json', 'w'), indent=1)
