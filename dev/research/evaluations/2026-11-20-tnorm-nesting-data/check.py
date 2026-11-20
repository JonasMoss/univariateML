# TASK-214.46: 60-digit suprema for tnorm archive 7657 (free; escapes to the truncated exponential)
# and 7689 (sd held). Data: archive cases.json.gz index 7657 = 7689, sample constant--0.327424-1e-12.
# Run: python3 check.py
import mpmath as mp
mp.mp.dps = 60
xs = [mp.mpf(float.fromhex(h)) for h in ['-0x1.4f4831e30a680p-2', '-0x1.4f4831e308f75p-2',
      '-0x1.4f4831e30786bp-2', '-0x1.4f4831e306161p-2', '-0x1.4f4831e30334cp-2']]
a, b = min(xs), max(xs); w = b - a; u = [x - a for x in xs]

def ll_exp(lam):   # truncated exponential on [a, b]: density lam exp(-lam u) / (1 - exp(-lam w))
    return sum(mp.log(lam) - lam*ui - mp.log(-mp.expm1(-lam*w)) for ui in u)
lam = mp.findroot(lambda L: mp.diff(ll_exp, L), mp.mpf('4.4e11'))
print('truncated-exponential supremum: rate %s, loglik %s' % (mp.nstr(lam, 20), mp.nstr(ll_exp(lam), 25)))

s = mp.mpf(1.0043282104167566)   # held sd of case 7689
def ll_tn(mu):     # truncated normal on [a, b], written relative to a (finite for mu far below a)
    g = lambda v: -v*(v + 2*(a - mu))/(2*s*s)            # log phi((a+v-mu)/s) - log phi((a-mu)/s)
    Z = mp.quad(lambda v: mp.exp(g(v)), [0, w])
    return sum(g(ui) - mp.log(Z) for ui in u)
mu = mp.findroot(lambda m: mp.diff(ll_tn, m), mp.mpf(-447462709488.34937))
print('sd held: mu %s, loglik %s' % (mp.nstr(mu, 20), mp.nstr(ll_tn(mu), 25)))
print('sd held, loglik at the returned mu -447462709488.34937: %s' % mp.nstr(ll_tn(mp.mpf(-447462709488.34937)), 25))
print('snapshot dda89f62: free limit_loglik 135.79877396527803 (rate -4.43512e11), held logLik 135.79882321069263')
