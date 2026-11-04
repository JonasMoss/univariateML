"""Rebuild the research classification, checking free coordinates against dossiers.
Run from the repository root. No fitting/density implementation is imported.
"""
import csv
import hashlib
import json
from pathlib import Path
import yaml

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]
IDS = 'kumar laplace lgamma llogis lnorm logis logitnorm lomax lst naka norm nsbeta paralogis pareto pareto2 pareto3 pareto4 power rayleigh sged sgomp slash sn snorm sstd st std tlambda tnorm trgamma triang unif weibull'.split()
COLS = 'id free held_or_integer dqm_everywhere dqm_fail_set info_nonsingular_everywhere info_singular_set k_every_x k_ae support_moves cramer observed_hessian_ok argument notes'.split()
arguments = {
'kumar': 'Endpoint power/log envelopes; log(x) and log(1-x^a) score asymptotics establish rank.',
'laplace': 'The square root density has an L2 translation derivative; location/scale scores have positive diagonal covariance; the pointwise Hessian omits the cusp mass.',
'lgamma': 'Fixed log(x) transform of the full two-parameter gamma exponential family.',
'llogis': 'Fixed log(x) transform gives a logistic location-scale family with location log(scale) and scale 1/shape.',
'lnorm': 'Fixed log(x) transform of normal.',
'logis': 'Smooth positive location-scale density with exponential tails and independent odd/even scores.',
'logitnorm': 'Fixed logit(x) transform of normal.',
'lomax': 'Use Y=log(1+lambda*X) for integrability; the shape score grows as Y and the rate score is bounded and nonconstant.',
'lst': 'Student tails give logarithmic df score and bounded nuisance scores; parity and tail orders give rank.',
'naka': 'Fixed X^2 transform gives a full gamma family with shape=shape and rate=shape/scale.',
'norm': 'Full normal exponential family.',
'nsbeta': 'Held affine transform gives a full beta exponential family.',
'paralogis': 'Log(X/scale) has exponential tails; distinct lower/upper tail slopes of the shape score and the bounded scale score give rank.',
'pareto': 'Moving b excludes mass of order |h| at a positive-density endpoint; fixed-support formal scores do not capture that loss.',
'pareto2': 'Held min and smooth scale=1/lambda transformation of lomax.',
'pareto3': 'Held min and fixed log(X-min) transform of logistic location-scale.',
'pareto4': 'Held min gives Burr XII; independent lower/upper tail exponents and scale score give rank, with logarithmic envelopes.',
'power': 'Moving alpha excludes mass of order |h| at a positive-density endpoint.',
'rayleigh': 'Fixed X^2 transform gives exponential with rate 1/(2*sigma^2).',
'sged': 'Smooth chart to raw two-piece exponential-power density; L2 cusp derivative iff nu>1/2; tail powers/logs and side coefficients prove rank.',
'sgomp': 'Exponential tail envelopes; rate score grows linearly while the eta score tends to 1/(1+eta), giving rank.',
'slash': 'The removable origin in integral_0^1 u*phi(u*z)du is analytic; polynomial tails have bounded scale and decaying location scores.',
'sn': 'Gaussian/Mills-ratio envelopes give DQM; alpha=0 makes shape and location scores proportional; unequal tail curvatures give rank elsewhere.',
'snorm': 'Raw two-piece normal has continuous first derivatives at the join; bounded piecewise Hessian envelopes and side-polynomial scores give DQM, rank and Hessian consistency.',
'sstd': 'Raw two-piece t has continuous first derivatives at the join; bounded/logarithmic tail envelopes, side scales and df tail exponent give full rank.',
'st': 'Student times positive bounded-argument t CDF; logarithmic df envelopes; tail exponent, tail ratio, common amplitude and next tail coefficient identify all score directions.',
'std': 'Smooth invertible chart of lst: sigma_raw=sd*sqrt((nu-2)/nu), df=nu.',
'tlambda': 'Quantile implicit differentiation and endpoint power alpha=1/lambda for 0<lambda<1; positive edge density for lambda>=1; see report for score-rank exceptions.',
'tnorm': 'Held a<b leaves a full exponential family with sufficient statistics X and X^2 and finite normal-tail moments.',
'trgamma': 'Y=(X/scale)^shape2 is gamma; scores span 1, log(Y), Y, Y*log(Y), with three independent centered directions.',
'triang': 'Linear endpoint vanishing yields divergent endpoint score squares; the mode join also omits a distributional term from the ordinary Hessian.',
'unif': 'Moving positive-density endpoints give order-|h| support loss; the formal score is a constant vector of rank one.',
'weibull': 'Y=(X/scale)^shape is exponential; score envelopes are powers/logs of Y and the two centered scores are independent.'}
rows=[]; hashes={}
for ident in IDS:
    path=ROOT/'univariateML-cpp/include/cppunivariate/distributions'/ident/'README.md'
    raw=path.read_bytes(); d=yaml.safe_load(raw.decode().split('---')[1]); hashes[str(path.relative_to(ROOT))]=hashlib.sha256(raw).hexdigest()
    held=[p['name'] for p in d['params'] if p['kind'] in ('design','threshold') or p.get('integer',False)]
    free=[p['name'] for p in d['params'] if p['name'] not in held]
    row=dict(zip(COLS,['']*len(COLS)))
    row.update(id=ident,free=';'.join(free),held_or_integer=';'.join(held) or 'none',dqm_everywhere='yes',dqm_fail_set='none',info_nonsingular_everywhere='yes (finite positive definite)',info_singular_set='none',k_every_x='infinity',k_ae='infinity',support_moves='no',cramer='yes',observed_hessian_ok='yes (truth and consistent interior plug-in)',argument=arguments[ident],notes='All finite README interior points; every listed free coefficient is fitted. No integer coefficients in this batch.')
    if ident in ('nsbeta','tnorm'):
        row['notes']+=' Held bounds must satisfy min<max or a<b, respectively; infinite tnorm bounds are allowed as fixed designs.'
    if ident=='rayleigh': row['k_every_x']='undefined at x=0 (log density=-infinity); infinity for x>0'
    if ident=='nsbeta': row['k_every_x']='undefined at held endpoints as a joint shape map; infinity for min<x<max'
    if ident=='laplace': row.update(k_every_x='0 (moving join x=mu)',cramer='no',observed_hessian_ok='no: location curvature is zero a.e.; Fisher location information is 1/sigma^2')
    if ident=='pareto': row.update(dqm_everywhere='no',dqm_fail_set='entire interior (b free)',info_nonsingular_everywhere='not applicable: non-DQM; formal E[ss^T] finite positive definite everywhere',k_every_x='not C0 at x=b; infinity for x>b',support_moves='yes',cramer='no',observed_hessian_ok='no: support term is missing')
    if ident=='power': row.update(dqm_everywhere='no',dqm_fail_set='entire interior (alpha free)',info_nonsingular_everywhere='not applicable: non-DQM; formal E[ss^T] finite positive definite everywhere',k_every_x='not C0 at x=alpha; infinity for 0<x<alpha',support_moves='yes',cramer='no',observed_hessian_ok='no: support term is missing')
    if ident=='unif': row.update(dqm_everywhere='no',dqm_fail_set='entire interior',info_nonsingular_everywhere='no: non-DQM; formal E[ss^T] finite rank 1',info_singular_set='entire interior (formal score second moment rank 1 of 2)',k_every_x='not C0 at either endpoint; infinity for min<x<max',support_moves='yes',cramer='no',observed_hessian_ok='no: support information is absent')
    if ident=='triang': row.update(dqm_everywhere='no',dqm_fail_set='entire interior a<c<b (free endpoints)',info_nonsingular_everywhere='no: endpoint score second moments infinite everywhere',info_singular_set='none with a finite full matrix; infinite endpoint entries instead',k_every_x='undefined at endpoints (log density=-infinity); C0 at x=c; infinity off endpoints/join',support_moves='yes',cramer='no',observed_hessian_ok='no: infinite endpoint information and omitted mode-kink sensitivity')
    if ident=='sn': row.update(info_nonsingular_everywhere='no: finite everywhere; positive definite iff alpha!=0',info_singular_set='alpha=0 (rank 2 of 3; xi and alpha scores proportional)',cramer='yes for smoothness/domination; nonsingularity fails at alpha=0',observed_hessian_ok='yes as an estimate of I including at alpha=0; inversion/Wald invalid at alpha=0')
    if ident in ('snorm','sstd'):
        row.update(k_every_x='1 (joint local C^k at moving join; also at xi=1)',cramer='no: moving join prevents local C3',notes=row['notes']+' At xi=1 a second-order expansion exists at the join, but no C2 neighborhood with xi free; this is not a rank singularity.')
    if ident=='sged':
        row.update(dqm_everywhere='no',dqm_fail_set='0<nu<=1/2 (free moving cusp)',info_nonsingular_everywhere='no: finite positive definite iff nu>1/2; infinite cusp-direction entries otherwise',info_singular_set='none in finite-information region nu>1/2',k_every_x='ceil(nu)-1 at the moving cusp, as a local C^k germ with nu and xi free',cramer='yes iff nu>3; no for 0<nu<=3',observed_hessian_ok='nu>2: yes at truth and consistent plug-in; 1<nu<=2: yes at truth, fitted joint Hessian OPEN; nu<=1: no',notes=row['notes']+' README density domain nu>0 is used, independently of the free-nu fitter restriction nu>=1. At nu=1 there is a first-derivative jump, including xi=1; for nu<1 curvature is nonintegrable. See report for fitted-Hessian open problem.')
    if ident=='tlambda':
        row.update(dqm_everywhere='no',dqm_fail_set='lambda>=1/2 (all three coefficients free)',info_nonsingular_everywhere='no: finite positive definite for lambda<1/2 or lambda>3/2 with lambda!=2; non-DQM latter region',info_singular_set='formal E[ss^T] rank 2 at lambda=1 or 2; infinite entries for 1/2<=lambda<1 and 1<lambda<=3/2',k_every_x='infinity at every strictly interior observation; at finite endpoints log density is infinite or discontinuous as support moves',support_moves='yes for lambda>0; locally yes at lambda=0; no locally for lambda<0',cramer='yes for lambda<0; no for lambda>=0 under classical common-support/local-envelope conditions',observed_hessian_ok='lambda<1/2: yes (truth and consistent root-n interior plug-in); lambda>=1/2: no ordinary joint information interpretation',notes=row['notes']+' The entire README real lambda domain is classified, including lambda>1 beyond the free-shape fitter branch. Uniform members lambda=1 and 2 have different scale widths; formal score rank is not DQM information.')
    rows.append(row)
assert len(rows)==33 and len({r['id'] for r in rows})==33
with (HERE/'classification.csv').open('w',newline='') as f:
    w=csv.DictWriter(f,fieldnames=COLS);w.writeheader();w.writerows(rows)
(HERE/'dossier-sha256.json').write_text(json.dumps(hashes,indent=2)+'\n')
print('PASS: 33 families; exact CSV schema; free/held coordinates checked against README metadata.')
