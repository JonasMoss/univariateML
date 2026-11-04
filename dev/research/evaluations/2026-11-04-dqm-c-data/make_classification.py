"""Build this job's table from reviewed arguments and canonical coefficient metadata."""
import csv
from pathlib import Path
import yaml

HERE=Path(__file__).resolve().parent
ROOT=HERE.parents[3]
IDS='bbinom binom bnbinom cmb cmp dgamma dlaplace dnorm dunif dweibull geom hyper lgser nbinom nhyper pig pois skellam tbinom tpois yulesimon zeta zib zinb zip zipf zmbinom zmgeom zmlogarithmic zmnbinom zmpois ztbinom ztgeom ztnbinom ztpois'.split()
COLUMNS='id free held_or_integer dqm_everywhere dqm_fail_set info_nonsingular_everywhere info_singular_set k_every_x k_ae support_moves cramer observed_hessian_ok argument notes'.split()
ARG={
'bbinom':'Finite positive analytic pmf; first two factorial moments identify both shapes for size >= 2.',
'binom':'Full one-parameter exponential family conditional on known size; information size/[prob*(1-prob)].',
'bnbinom':'Polynomial-tail log envelopes give DQM and Cramer; adjacent-mass ratio proves rank except size=beta (proof B).',
'cmb':'Finite exponential family with statistics x and log choose(size,x); minimal exactly when size >= 2.',
'cmp':'Full minimal exponential family in log(lambda) and -nu; factorial tails dominate all derivatives locally for nu > 0.',
'dgamma':'Fixed binning preserves gamma DQM; conditional moment envelopes and tail score independence prove Cramer and rank (proof C).',
'dlaplace':'With integer location known this is a minimal exponential family in log(scale), statistic abs(x-location).',
'dnorm':'Fixed binning preserves normal DQM; conditional polynomial envelopes and quadratic tail coefficients prove rank (proof C).',
'dunif':'All coefficients are known integers; there is no continuous estimation problem.',
'dweibull':'Survival shape1**(k**shape2) identifies both tangent directions; stretched exponential envelopes prove Cramer (proof D).',
'geom':'Minimal geometric exponential family; exponentially decaying tail supplies all local derivative moments.',
'hyper':'All coefficients are known integer population/design counts; no continuous estimation problem.',
'lgser':'Minimal exponential family in log(theta), statistic x; exponential tail.',
'nbinom':'Exponential-tail gamma-ratio envelopes; adjacent ratios (x+size)*(1-prob)/(x+1) identify both tangents.',
'nhyper':'All coefficients are known integer population/design counts; no continuous estimation problem.',
'pig':'IG-mixture DQM; GIG conditional moment bounds give Cramer; mean and variance derivatives prove rank (proof E).',
'pois':'Minimal Poisson exponential family; information 1/lambda.',
'skellam':'Independent-Poisson difference preserves DQM; conditional count bounds give Cramer; mean/variance identify both tangents (proof F).',
'tbinom':'Finite conditional exponential family in logit(prob), statistic x; rank one exactly when retained support has >= 2 atoms.',
'tpois':'Finite conditional exponential family in log(lambda), statistic x; rank one exactly when retained support has >= 2 atoms.',
'yulesimon':'Gamma-ratio tail x**(-rho-1) controls logarithmic score envelopes; p(1)=rho/(rho+1) has nonzero derivative.',
'zeta':'Minimal exponential family in -s, statistic log(x); log moments of every finite order exist for s > 1.',
'zib':'Positive-count ratio identifies prob for size >= 2; total positive mass then identifies pi; finite analytic model.',
'zinb':'Positive adjacent ratios identify size and prob; positive total mass identifies pi; exponential envelopes.',
'zip':'Positive adjacent ratios identify lambda; total positive mass identifies pi; factorial envelopes.',
'zipf':'Finite exponential family in -s, statistic log(x); minimal exactly for known N >= 2.',
'zmbinom':'Independent Bernoulli zero indicator plus conditional positive binomial; finite analytic hurdle model.',
'zmgeom':'Independent Bernoulli zero indicator plus shifted geometric; exponential envelopes.',
'zmlogarithmic':'Independent Bernoulli zero indicator plus logarithmic-series exponential family; exponential envelopes.',
'zmnbinom':'Independent Bernoulli zero indicator plus positive NB; adjacent positive ratios prove rank; exponential envelopes.',
'zmpois':'Independent Bernoulli zero indicator plus positive Poisson exponential family; factorial envelopes.',
'ztbinom':'Conditional finite binomial exponential family; positive support has >= 2 atoms exactly for size >= 2.',
'ztgeom':'Fixed shift of geometric; minimal exponential family and exponential envelopes.',
'ztnbinom':'Positive NB adjacent ratios identify size and prob; conditioning preserves local exponential derivative envelopes.',
'ztpois':'Minimal positive Poisson exponential family in log(lambda); factorial envelopes.'}
EXCEPT={
'bbinom':'size=0: rank 0; size=1: rank 1 (only alpha/(alpha+beta) identified)',
'binom':'size=0: rank 0 (prob unidentified)',
'bnbinom':'size=beta: rank 2 of 3; null direction (1,0,-1) in (size,alpha,beta)',
'cmb':'size=0: rank 0; size=1: rank 1 (nu unidentified)',
'zib':'size=0: rank 0; size=1: rank 1 (only (1-pi)*prob identified)',
'zipf':'N=1: rank 0 (s unidentified)',
'zmbinom':'size=1: rank 1 of 2 (prob unidentified)',
'ztbinom':'size=1: rank 0 (prob unidentified)',
 'tbinom':'L=U: rank 0; L=max(0,floor(a)+1), U=min(size,floor(b))',
 'tpois':'L=U: rank 0; L=max(0,floor(a)+1), U=floor(b)'}
rows=[]
for id in IDS:
    d=yaml.safe_load((ROOT/'univariateML-cpp/include/cppunivariate/distributions'/id/'README.md').read_text().split('---')[1])
    held=[p for p in d['params'] if p.get('integer') or p.get('kind') in ('design','threshold')]
    free=[p for p in d['params'] if p not in held]
    note='Interior: '+', '.join(p['name']+' in '+str(p['domain'])+' (endpoints excluded)' for p in free)+'.' if free else 'Zero-dimensional conditional model; no Wald/LR/AIC coefficient dimension for the held integer choices.'
    if id=='bnbinom': note+=' Global size/beta label exchange persists off the diagonal; local rank is full there. Earlier unrestricted rank eligibility needs correction.'
    if id in ('tbinom','tpois'): note+=' Require nonempty retained support and positive conditioning probability; a and b held even though real-valued.'
    if id=='cmp': note+=' nu=0 geometric face is outside the requested Euclidean interior; positive lambda and nu only.'
    if id=='zipf': note+=' s=0 uniform member is a boundary; no integer-cutoff inference is asserted.'
    rows.append(dict(id=id,free='; '.join(p['name'] for p in free) or 'none',held_or_integer='; '.join(p['name']+(' (integer known)' if p.get('integer') else ' (design held)') for p in held) or 'none',dqm_everywhere='yes' if free else 'vacuous',dqm_fail_set='none',info_nonsingular_everywhere=('no' if id in EXCEPT else 'yes') if free else 'not_applicable',info_singular_set=EXCEPT.get(id,'none') if free else 'not_applicable',k_every_x='infinity' if free else 'vacuous',k_ae='infinity' if free else 'vacuous',support_moves='no (conditional on holds)',cramer='yes (smoothness and domination; rank exceptions separate)' if free else 'vacuous',observed_hessian_ok='yes (estimates finite I; may be singular)' if free else 'not_applicable',argument=ARG[id],notes=note))
with (HERE/'classification.csv').open('w',newline='') as f:
    w=csv.DictWriter(f,fieldnames=COLUMNS);w.writeheader();w.writerows(rows)
print('Wrote',len(rows),'rows')
