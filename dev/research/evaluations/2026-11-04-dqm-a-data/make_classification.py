"""Rebuild the research table; metadata selects coordinates, not regularity."""
import csv
from pathlib import Path
import yaml
ROOT = Path(__file__).resolve().parents[4]
OUT = Path(__file__).resolve().parent
IDS = 'beta betapr bhatt burr cauchy emg exp fatigue foldnorm fpareto frechet gamma gb2 ged genbeta gev gompertz gpd gumbel hcauchy hnorm ht huber invburr invexp invgamma invgauss invparalogis invpareto invtrgamma invweibull'.split()
FIELDS = 'id free held_or_integer dqm_everywhere dqm_fail_set info_nonsingular_everywhere info_singular_set k_every_x k_ae support_moves cramer observed_hessian_ok argument notes'.split()
ARGUMENTS = {
'beta':'Full exponential family with independent log(x), log(1-x) statistics.',
'betapr':'Fixed log transform gives the a=1 generalized logistic (GB2) submodel; score independence inherited.',
'bhatt':'Normal plus a*Uniform(-1,1); Gaussian mixture derivative envelopes; mean/variance/fourth cumulant have full-rank Jacobian for a>0.',
'burr':'Log-data generalized logistic with one beta shape fixed; GB2 rank proof and logarithmic tail envelopes.',
'cauchy':'Bounded rational location/scale scores; odd/even independence gives I=diag(1,1)/(2*scale^2).',
'emg':'Normal plus independent exponential; latent scores imply DQM; first three cumulants have nonsingular Jacobian.',
'exp':'One-parameter full exponential family; I=1/rate^2.',
'fatigue':'Held shift: direct exp(-c*y-c/y) envelopes; alpha/beta scores have independent upper/lower-tail leading terms.',
'foldnorm':'Fixed folding of normal preserves DQM; positive-mu score grows linearly and sigma score quadratically in the upper tail.',
'fpareto':'Fixed translation of GB2; same information rank and parameter envelopes.',
'frechet':'Held shift and fixed log transform give Gumbel in smooth invertible coordinates.',
'gamma':'Full exponential family with independent log(x), x statistics.',
'gb2':'Log-data generalized logistic; opposite tail slopes and t*exp(-a*abs(t)) corrections prove full score rank.',
'ged':'Square-root cusp derivative is L2 iff nu>1/2; odd location score and independent even scale/shape scores.',
'genbeta':'Y=-log(X/scale) has density a*exp(-a*p*y)*(1-exp(-a*y))^(q-1)/B(p,q); tail correction identifies a except q=1.',
'gev':'Moving endpoint power alpha=-1/xi for xi<0; square-root derivative and support loss give threshold xi>-1/2; quantile derivatives prove rank.',
'gompertz':'Superexponential tail dominates all local derivatives; x*exp(b*x) term separates b from a score.',
'gpd':'Known lower endpoint; moving upper endpoint power alpha=-1/xi; exact information determinant positive for xi>-1/2.',
'gumbel':'Exponential variable exp(-(X-mu)/sigma) supplies finite log/power envelopes; location/scale scores independent.',
'hcauchy':'Known threshold and fixed absolute-value transform of Cauchy; nonconstant scale score.',
'hnorm':'Known threshold: square of standardized half-normal is chi-square(1); positive scale information 2/sigma^2.',
'ht':'Known threshold: fixed folding of location-zero t; nu score has logarithmic tail while scale score tends to nonzero constant.',
'huber':'Held epsilon: continuous matching first derivatives; exponential envelopes, odd/even independent scores and piecewise Hessian uniform LLN.',
'invburr':'Fixed reciprocal transformation of Burr, with invertible reciprocal scale map.',
'invexp':'Fixed reciprocal of exponential in rate=scale coordinates.',
'invgamma':'Fixed reciprocal of gamma with shape=alpha and gamma scale=1/beta.',
'invgauss':'Full exponential family with independent x and 1/x statistics; nonsingular map from mean/shape.',
'invparalogis':'Smooth full-rank two-coordinate embedding in inverse Burr (both shapes equal); inherits positive information.',
'invpareto':'Smooth full-rank embedding in inverse Burr with second shape fixed to one.',
'invtrgamma':'Log reciprocal data give log-gamma location/scale/shape; independent t, exp(a*t), t*exp(a*t) terms prove rank.',
'invweibull':'Fixed reciprocal Weibull, equivalently a held-threshold Frechet; invertible shape/scale map.'}
rows=[]
for id in IDS:
    d=yaml.safe_load((ROOT/'univariateML-cpp/include/cppunivariate/distributions'/id/'README.md').read_text().split('---')[1])
    free=[p['name'] for p in d['params'] if p.get('kind','regular') not in ('threshold','design')]
    held=[p['name']+' (known '+p['kind']+')' for p in d['params'] if p.get('kind','regular') in ('threshold','design')]
    r=dict(zip(FIELDS,['']*len(FIELDS)))
    r.update(id=id,free='; '.join(free),held_or_integer='; '.join(held) or 'none; no integer coefficients',dqm_everywhere='yes',dqm_fail_set='none',info_nonsingular_everywhere='yes',info_singular_set='none',k_every_x='infinity',k_ae='infinity',support_moves='no (under stated holds)',cramer='yes (common null endpoints omitted)',observed_hessian_ok='yes at truth and consistent fitted parameters',argument=ARGUMENTS[id],notes='Interior of README domain; fit restrictions do not redefine this domain. All information is per observation in listed free coordinates.')
    if id in ('fatigue','frechet'):
        r.update(k_every_x='not C0 as a real log density at x=mu; infinity for x>mu')
    if id=='fpareto':r.update(k_every_x='not C0 at x=min; infinity for x>min',notes=r['notes']+' At min, varying shape2*shape3 changes log endpoint density between +infinity and -infinity, including at product=1.')
    if id=='genbeta':
        r.update(info_nonsingular_everywhere='no',info_singular_set='shape2=1: rank 2 of 3; only shape1*shape3 identified along this surface',k_every_x='not C0 at x=scale; infinity for 0<x<scale',notes=r['notes']+' DQM and Hessian identity hold even on the rank-deficient surface; no ordinary joint inverse or three-parameter AIC there.')
    if id in ('bhatt','foldnorm'):
        r['notes']+=' '+('a=0 is a boundary, excluded here; its a-score vanishes.' if id=='bhatt' else 'mu=0 is a boundary, excluded here; its mu-score vanishes. Signed-mu global symmetry is outside the README domain.')
    if id=='huber':
        r.update(k_every_x='1',k_ae='infinity (at each truth away from x=mu +/- sigma*epsilon)',cramer='no: moving second-derivative joins',observed_hessian_ok='yes: ordinary a.e. piecewise Hessian at truth and consistent fits',notes=r['notes']+' epsilon is held because README kind=design; free-epsilon regularity also follows from prior moving-feature report but is not this row.')
    if id=='ged':
        r.update(dqm_everywhere='no',dqm_fail_set='0<nu<=1/2 (free mean cusp)',info_nonsingular_everywhere='no',info_singular_set='no finite singular points; location information infinite for 0<nu<=1/2',k_every_x='ceil(nu)-1 (joint shape free, including even integer nu)',k_ae='infinity (x != mean)',cramer='yes iff nu>3',observed_hessian_ok='truth: yes iff nu>1; joint fitted: yes nu>2, open 1<nu<=2; no nu<=1',notes=r['notes']+' Shape is free on the density domain nu>0; implemented free-shape fit restricts nu>=1. Held-shape normal nu=2 has infinity smoothness but is a different submodel. Open joint fitted-Hessian gap is documented in report.')
    if id in ('gev','gpd'):
        r.update(dqm_everywhere='no',dqm_fail_set='xi<=-1/2',info_nonsingular_everywhere='no',info_singular_set='DQM Fisher full rank at xi>-1/2; outside: raw E[ssT] divergent except xi=-1 (finite full rank); centered covariance singular at xi=-1',k_every_x=('infinity at xi=0; not C0 at finite closed endpoint for xi!=0' if id=='gev' else 'infinity at xi>=0; not C0 at upper closed endpoint for xi<0'),k_ae='infinity (positive-density observations, including xi=0)',support_moves=('yes locally at every xi, including zero' if id=='gev' else 'yes locally iff xi<=0; fixed lower threshold'),cramer=('no at every xi: no common-support neighborhood with a finite local log-derivative envelope' if id=='gev' else 'yes iff xi>0; no at xi<=0 (moving support in each neighborhood)'),observed_hessian_ok='yes xi>-1/2 at truth and root-n consistent fits; no finite joint information estimate at xi<=-1/2',notes=r['notes']+' xi=-1 finite raw scores have nonzero endpoint-direction mean; their OPG is not LAN information. Fit bounds xi>=-1 and (GEV) xi<=1 concern estimator constraints, not this model classification.')
    rows.append(r)
if __name__ == '__main__':
    with (OUT/'classification.csv').open('w',newline='') as f:
        w=csv.DictWriter(f,fieldnames=FIELDS);w.writeheader();w.writerows(rows)
    print(f'Wrote {len(rows)} rows with {len(FIELDS)} columns')
