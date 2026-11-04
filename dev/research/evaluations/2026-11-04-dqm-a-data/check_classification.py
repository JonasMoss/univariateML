"""Metadata/CSV checks and independent analytic-score quadrature, not DQM proofs.
Run from the worktree through /tmp/umvl-cpu/run; no package code is called.
"""
import csv, hashlib, json, platform, warnings
from pathlib import Path
import numpy as np
import scipy
from scipy.integrate import quad_vec, IntegrationWarning
from scipy.special import betaln, digamma, polygamma, expit
import sympy as sp
import yaml
from make_classification import ROOT, OUT, IDS, FIELDS
warnings.simplefilter('error', IntegrationWarning)
rows=list(csv.DictReader((OUT/'classification.csv').open()))
assert len(rows)==31 and [r['id'] for r in rows]==IDS
assert list(rows[0])==FIELDS
sources={}
for r in rows:
    path=ROOT/'univariateML-cpp/include/cppunivariate/distributions'/r['id']/'README.md'
    text=path.read_text(); d=yaml.safe_load(text.split('---')[1])
    expected=[p['name'] for p in d['params'] if p.get('kind','regular') not in ('design','threshold')]
    assert r['free'].split('; ')==expected
    assert all(r[k] for k in FIELDS)
    sources[str(path.relative_to(ROOT))]=hashlib.sha256(text.encode()).hexdigest()
    for p in d['params']:
        if p.get('kind','regular') in ('design','threshold'):assert p['name']+' (known '+p['kind']+')' in r['held_or_integer']
for relative in ['CLAUDE.md', 'univariateML-cpp/include/cppunivariate/distributions/README_SCHEMA.md', 'dev/roadmap/2026-10-29-parameter-rules.md', 'dev/roadmap/2026-10-30-inference-and-dqm.md', 'dev/research/evaluations/2026-10-31-inference-1-report.md', 'dev/research/evaluations/2026-11-04-infer-moving-report.md', 'dev/research/evaluations/2026-11-04-dqm-a-data/make_classification.py', 'dev/research/evaluations/2026-11-04-dqm-a-data/check_classification.py']:
    sources[relative]=hashlib.sha256((ROOT/relative).read_bytes()).hexdigest()
results=[]
# Log-X GB2: p=shape3, q=shape1, a=shape2, c=log(scale).
# Independent tail chart and closed-form scores; matrix entries by quadrature.
for p,q,a in [(1,1,1),(.7,1.3,1.1),(2,2,.8),(1,3,2)]:
    def integrand(t):
        z=a*t; L=np.logaddexp(0,z); v=expit(z)
        f=np.exp(np.log(a)-betaln(p,q)+p*z-(p+q)*L)
        s=np.array([digamma(p+q)-digamma(p)+z-L,digamma(p+q)-digamma(q)-L,1+p*z-(p+q)*z*v,-a*p+(p+q)*a*v])
        return np.r_[1,s,np.outer(s,s).ravel()]*f
    ans,err=quad_vec(integrand,-100/a,100/a,epsabs=1e-10,epsrel=1e-10)
    I=ans[5:].reshape(4,4);e=np.linalg.eigvalsh(I)
    assert abs(ans[0]-1)<1e-8 and np.max(np.abs(ans[1:5]))<1e-8 and e[0]>1e-5
    results.append(dict(model='gb2',p=p,q=q,a=a,mass=float(ans[0]),max_abs_mean_score=float(max(abs(ans[1:5]))),eigenvalues=e.tolist(),quadrature_error=float(err),bounds=[-100/a,100/a]))
for p,q in [(1,1),(2,1),(.8,.7),(1.2,2),(2.5,4)]:
    def integrand(z):
        v=1/np.expm1(z);L=np.log(-np.expm1(-z))
        f=np.exp(-p*z+(q-1)*L-betaln(p,q))
        s=np.array([digamma(p+q)-digamma(p)-z,digamma(p+q)-digamma(q)+L,1-p*z+(q-1)*z*v])
        H=np.array([[polygamma(1,p)-polygamma(1,p+q),-polygamma(1,p+q),z],[-polygamma(1,p+q),polygamma(1,q)-polygamma(1,p+q),-z*v],[z,-z*v,p*z-(q-1)*(z*v-z*z*(v+v*v))]])
        return np.r_[1,s,np.outer(s,s).ravel(),H.ravel()]*f
    ans,err=quad_vec(integrand,0,100/p,epsabs=1e-9,epsrel=1e-9)
    I=ans[4:13].reshape(3,3);H=ans[13:].reshape(3,3);e=np.linalg.eigvalsh(I)
    assert abs(ans[0]-1)<1e-7 and max(abs(ans[1:4]))<1e-7
    assert np.max(np.abs(I-H))<1e-7
    if q==1:assert abs(e[0])<1e-9 and np.linalg.norm(I@np.array([-p,0,1]))<1e-8 and e[1]>1e-4
    else:assert e[0]>1e-5
    results.append(dict(model='genbeta',p=p,q=q,mass=float(ans[0]),max_abs_mean_score=float(max(abs(ans[1:4]))),eigenvalues=e.tolist(),max_abs_OPG_minus_H=float(np.max(np.abs(I-H))),quadrature_error=float(err),bounds=[0,100/p]))
# Non-DQM xi=-1: formal pointwise scores are finite but not centered.
for family in ('gpd', 'gev'):
    dim=2 if family=='gpd' else 3
    def integrand(t):
        if family=='gpd':
            score=np.array([-1.,np.log(t)]);weight=1.
        else:
            score=np.array([-1.,t-2,(1-t)*np.log(t)+t-1]);weight=np.exp(-t)
        return np.r_[1.,score,np.outer(score,score).ravel()]*weight
    ans,err=quad_vec(integrand,0,1 if family=='gpd' else 100,epsabs=1e-9,epsrel=1e-9)
    mean=ans[1:1+dim];raw=ans[1+dim:].reshape(dim,dim)
    cov=raw-np.outer(mean,mean)
    re=np.linalg.eigvalsh(raw);ce=np.linalg.eigvalsh(cov)
    assert np.max(np.abs(mean+1))<1e-7 and re[0]>1e-5 and abs(ce[0])<1e-7 and ce[1]>1e-4
    results.append(dict(model=family,xi=-1,coordinates='log(sigma),xi' if family=='gpd' else 'mu,log(sigma),xi',sigma=1,mean_score=mean.tolist(),raw_OPG_eigenvalues=re.tolist(),centered_covariance_eigenvalues=ce.tolist(),quadrature_error=float(err)))
# Cumulant maps prove local rank once score integrability has been established.
mu,sigma,a,lam=sp.symbols('mu sigma a lam',positive=True)
B=sp.Matrix([mu,sigma**2+a**2/3,-2*a**4/15]).jacobian([mu,sigma,a]).det().factor()
E=sp.Matrix([mu+1/lam,sigma**2+1/lam**2,2/lam**3]).jacobian([mu,sigma,lam]).det().factor()
assert B==-16*a**3*sigma/15 and E==-12*sigma/lam**4
xi,s=sp.symbols('xi s',real=True)
G=sp.Matrix([[1/(s*s*(1+2*xi)),1/(s*(1+xi)*(1+2*xi))],[1/(s*(1+xi)*(1+2*xi)),2/((1+xi)*(1+2*xi))]])
assert sp.simplify(G.det()-1/(s*s*(1+xi)**2*(1+2*xi)))==0
out=dict(platform=platform.platform(),versions=dict(numpy=np.__version__,scipy=scipy.__version__,sympy=sp.__version__,PyYAML=yaml.__version__),metadata_rows=len(rows),columns=FIELDS,source_sha256=sources,quadratures=results,symbolic=dict(bhatt_cumulant_jacobian=str(B),emg_cumulant_jacobian=str(E),gpd_information_determinant=str(sp.factor(G.det()))),limits='Finite deterministic quadrature, no seeds; no fitted estimators or coverage checks; tail truncation is numerical and is not an interval certificate.')
(OUT/'checks.json').write_text(json.dumps(out,indent=2)+'\n')
print(f'PASS: {len(rows)} metadata rows; {len(results)} score quadratures; 3 symbolic identities. Evidence: checks.json')
