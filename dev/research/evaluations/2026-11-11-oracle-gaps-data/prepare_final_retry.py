"""Retry residual reference failures after the second foreground pass ends."""
import difflib,json,os,shutil,subprocess,sys
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4]
HERE=Path(__file__).resolve().parent

def main():
    planned={}
    p=ROOT/'dev/validation/quality/truth_reference.py';old=p.read_text()
    needle='''    if z>mp.mpf('1e50')*(1+abs(a-1)):
'''
    replacement='''    if z<mp.mpf('1e-50'):
        # P(a,z)=z**a/Gamma(a+1) E[exp(-z*U)], U~Beta(a,1).
        # 1-z <= E[exp(-z*U)] <= 1; omitted log width < 2e-50.
        # Direct upper gammainc tries an enormous 2F0 argument 1/z here.
        return pair(a*mp.log(z)-mp.loggamma(a+1))
    if z>mp.mpf('1e50')*(1+abs(a-1)):
'''
    assert old.count(needle)==1
    new=old.replace(needle,replacement)
    new=new.replace('import mpmath as mp\n','import mpmath as mp\nfrom math import isqrt\n',1)
    needle='''                if hi >= limit: return mp.inf
                hi = min(limit,'''
    replacement='''                if hi >= limit: return mp.inf
                lo = hi  # retain the last proved lower bracket endpoint
                hi = min(limit,'''
    assert new.count(needle)==1
    new=new.replace(needle,replacement)
    needle='''                mid = (lo+hi)//2
                if below(mid): lo = mid'''
    replacement='''                # Counts can span hundreds of decades. Bisect their log
                # scale until the bracket has a moderate ratio, then integers.
                mid = isqrt(lo*hi) if lo>0 and hi>4*lo else (lo+hi)//2
                if below(mid): lo = mid'''
    assert new.count(needle)==1
    new=new.replace(needle,replacement)
    needle='def logarithmic_tail(p, k):'
    helper='''def logarithmic_stieltjes_tail(p,k):
    """Euler--Maclaurin with a signed next-term bound from coth's poles.

    1/(1-exp(-t))=1/t+1/2+2*t*sum_n 1/(t*t+4*pi*pi*n*n).
    The finite geometric expansion of each positive denominator has a
    signed remainder bounded by its next term, for every t>0.
    Integrate against exp(-k*t) from -log(p) to infinity.
    """
    x=-k*mp.log(p)
    total=mp.exp(x)*mp.e1(x)+mp.mpf('.5')/k
    term=mp.mpf(1);poly=term
    for n in range(1,129):
        term*=x/n;poly+=term
        if n%2:
            j=(n+1)//2
            correction=mp.bernoulli(2*j)/(2*j)*poly/k**(2*j)
            if abs(correction)<mp.mpf('1e-50')*abs(total):
                return k*mp.log(p)+mp.log(total)-mp.log(-mp.log1p(-p))
            total+=correction
    raise ArithmeticError('logarithmic Stieltjes next-term bound')


def logarithmic_tail(p, k):'''
    assert new.count(needle)==1
    new=new.replace(needle,helper)
    needle="    if k>mp.mpf('1e40'):\n"
    replacement='''    if p>mp.mpf('.5') and k<=mp.mpf('1e40'):
        if k<200:
            prefix=mp.fsum(p**j/j for j in range(1,int(k)))
            return complement(mp.log(prefix)-mp.log(-mp.log1p(-p)))
        return logarithmic_stieltjes_tail(p,k)
    if k>mp.mpf('1e40'):
'''
    assert new.count(needle)==1
    planned[p]=(old,new.replace(needle,replacement))
    p=ROOT/'dev/validation/quality/truth.py';old=p.read_text()
    needle='                value = tail if logp else mp.exp(tail)'
    replacement="                value = tail if logp else (mp.mpf(0) if tail < -10000 else mp.exp(tail))"
    assert old.count(needle)==1
    planned[p]=(old,old.replace(needle,replacement))
    p=HERE/'generate.py';old=p.read_text()
    needle="                reserve=None;e.__traceback__=None;e.__context__=None;gc.collect()"
    replacement="                reserve=None;gc.collect()\n                memory_trace=traceback.format_exc(limit=8)\n                e.__traceback__=None;e.__context__=None;gc.collect()"
    assert old.count(needle)==1
    new=old.replace(needle,replacement)
    new=new.replace('traceback=traceback.format_exc(limit=8),seconds=',
                    'traceback=(memory_trace if isinstance(e,MemoryError) else traceback.format_exc(limit=8)),seconds=')
    new=new.replace("pattern='test_references.py'","pattern='test_*.py'")
    planned[p]=(old,new)
    pig=json.loads((HERE/'generation/pig.json').read_text())
    if any(r['status']=='oracle gap' for r in pig.get('records',[])):
        p=ROOT/'univariateML-cpp/include/cppunivariate/distributions/pig/truth.py'
        old=p.read_text()
        needle='            value,error=mp.quad(scaled,cuts,error=True)'
        replacement='''            value,error=mp.quad(scaled,cuts,method='gauss-legendre',maxdegree=6,error=True)
            if error>abs(value)*mp.mpf('1e-45'):
                value,error=mp.quad(scaled,cuts,method='gauss-legendre',maxdegree=8,error=True)'''
        assert old.count(needle)==1
        planned[p]=(old,old.replace(needle,replacement))
    skellam=json.loads((HERE/'generation/skellam.json').read_text())
    if any(r['status']=='oracle gap' for r in skellam.get('records',[])):
        p=ROOT/'univariateML-cpp/include/cppunivariate/distributions/skellam/truth.py'
        old=p.read_text()
        needle='''        previous=poisson(k)[index]
        for j in range(10000):'''
        replacement='''        previous=poisson(k)[index]
        poisson_mass=-A+k*mp.log(A)-mp.loggamma(k+1)
        for j in range(10000):'''
        assert old.count(needle)==1
        new=old.replace(needle,replacement)
        needle='''            current=poisson(k+j+1)[index]
            term*=B/(j+1)*mp.exp(current-previous);total+=term;previous=current'''
        replacement='''            # The adjacent Poisson cut differs by one positive PMF.
            # Avoid recomputing a concentrated gamma integral at every cut.
            poisson_mass+=mp.log(A)-mp.log(k+j+1)
            ratio=mp.exp(poisson_mass-previous)
            if lower:
                current=previous+mp.log1p(ratio)
            elif ratio<1-mp.mpf('1e-100'):
                current=previous+mp.log1p(-ratio)
            else:
                current=poisson(k+j+1)[index]
            term*=B/(j+1)*mp.exp(current-previous);total+=term;previous=current'''
        assert new.count(needle)==1
        planned[p]=(old,new.replace(needle,replacement))
    archive=HERE/'generation-pass2'
    if archive.exists():raise FileExistsError(archive)
    shutil.copytree(HERE/'generation',archive)
    shutil.copyfile(ROOT/'build/scratch/generation-retry.log',HERE/'generation-pass2.log')
    patch=[]
    for p,(old,new) in planned.items():
        rel=str(p.relative_to(ROOT))
        patch.extend(difflib.unified_diff(old.splitlines(keepends=True),new.splitlines(keepends=True),fromfile='a/'+rel,tofile='b/'+rel))
    (HERE/'final-retry.patch').write_text(''.join(patch))
    for p,(_,new) in planned.items():p.write_text(new)
    ids=[]
    for p in sorted((HERE/'generation').glob('*.json')):
        r=json.loads(p.read_text())
        if 'records' not in r or any(x['status']=='oracle gap' for x in r['records']):ids.append(p.stem)
    (HERE/'final-retry-ids.json').write_text(json.dumps(ids,indent=2)+'\n')
    env=dict(os.environ,TMPDIR=str(ROOT/'build/tmp'),JUST_TEMPDIR=str(ROOT/'build/tmp'),
             XDG_RUNTIME_DIR=str(ROOT/'build/tmp'),TRUTH_EXTREME_TIMEOUT='600',
             TRUTH_EXTREME_WORKERS='3',TRUTH_FORCE_GAPS='1')
    print('Retry residual gaps with bounded small-z gamma and guarded natural tails:',ids,flush=True)
    with (ROOT/'build/scratch/generation-final-retry.log').open('w') as log:
        return subprocess.run(['/tmp/umvl-cpu/run4',sys.executable,str(HERE/'generate.py'),*ids],
                              cwd=ROOT,env=env,stdout=log,stderr=subprocess.STDOUT).returncode

if __name__=='__main__':sys.exit(main())
