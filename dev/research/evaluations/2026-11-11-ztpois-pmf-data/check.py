"""Independent 80/120-digit binomial log-gamma checks and frozen case replay.
Run from repository root after build.sh, under /tmp/umvl-cpu/run.
All floating point parameters are converted to mpmath exactly from binary64.
"""
import collections,json,math,pathlib,subprocess,sys
import mpmath as mp
HERE=pathlib.Path(__file__).resolve().parent
EVAL=HERE.parent
mp.mp.dps=80

def binlog(x,n,p):
    n=mp.mpf(n);p=mp.mpf(p)
    if p==0:return mp.mpf(0) if x==0 else mp.ninf
    if p==1:return mp.mpf(0) if x==n else mp.ninf
    return mp.loggamma(n+1)-mp.loggamma(x+1)-mp.loggamma(n-x+1)+x*mp.log(p)+(n-x)*mp.log1p(-p)

def grid():
    points=set()
    for n in [10**j for j in range(3,10)]:
        for p in [1e-12,1/n,.001,.1,.5,.9,1-1/n,1-1e-12]:
            sd=math.sqrt(n*p*(1-p))
            for x in [0,1,2,15,16,n-16,n-15,n-2,n-1,n,*[round(n*p+k*sd) for k in [-3,-1,0,1,3]]]:
                if 0<=x<=n:points.add((x,n,p))
    points.update((x,n+.25,p) for x,n,p in list(points) if x<n)
    points=sorted(points)
    inp=''.join(f'{x} {n} {p!r}\n' for x,n,p in points)
    rows=[]
    for mode in ['native','double']:
        vals=list(map(float,subprocess.run([f'build/scratch/kernel-{mode}'],input=inp,text=True,capture_output=True,check=True).stdout.splitlines()))
        assert len(vals)==len(points)
        for (x,n,p),v in zip(points,vals):
            ref=binlog(x,n,p)
            with mp.workdps(120):high=binlog(x,n,p)
            assert abs(high-ref)<mp.mpf('1e-60')*max(1,abs(high))
            error=float(abs(v-ref)/max(1,abs(ref)))
            rows.append(dict(mode=mode,x=x,size=n,prob=p,value=v,reference=str(ref),normalized_error=error,reference_drift=float(abs(high-ref))))
    (HERE/'grid.json').write_text(json.dumps(rows,indent=2)+'\n')
    assert max(r['normalized_error'] for r in rows)<1e-10
    print('grid',len(points),'points per mode; worst normalized error',max(r['normalized_error'] for r in rows),flush=True)

KEYS=['size','alpha','beta','m','N','k','r','prob','pi','p0','a','b','mu1','mu2']
def replay():
    cases=[c for c in json.loads((EVAL/'2026-11-11-acc-fb-count-data/cases.json').read_text()) if c['family'] in ['ztbinom','zib','zmbinom']]
    prior=[c for c in json.loads((EVAL/'2026-11-10-fallback-count-data/cases.json').read_text()) if c['family'] in ['ztbinom','zib','zmbinom']]
    assert all(any(c['study_index']==q['study_index'] and c['data']==q['data'] and c['held']==q['held'] for c in cases) for q in prior)
    (HERE/'cases.json').write_text(json.dumps(cases,indent=2)+'\n')
    inp=''.join(' '.join(map(str,[c['study_index'],c['family'],len(c['data']),*c['data'],*[c['held'].get(k,'NA') for k in KEYS],c.get('maxit',200),c.get('tol',1e-10)]))+'\n' for c in cases)
    for mode in ['native','double']:
        for phase in ['before','after']:
            dest=HERE/f'{phase}-{mode}.tsv'
            with dest.open('w') as out:
                subprocess.run([f'build/scratch/replay-{phase}-{mode}'],input=inp,text=True,stdout=out,check=True)
            print(mode,phase,'finished',flush=True)

def parse(path):
    rows={}
    for l in path.read_text().splitlines():
        a=l.split('\t');rows[int(a[0])]=dict(outcome=a[2],logLik=float(a[3]),coef={k:float(v) for k,v in (f.split('=',1) for f in a[4:] if '=' in f)})
    return rows

def likelihood(c,t):
    n=t['size'];p=mp.mpf(t['prob']);xs=collections.Counter(c['data']);total=mp.mpf(0)
    for x,w in xs.items():
        if c['family']=='zib':
            pi=mp.mpf(t['pi'])
            term=mp.log(pi+(1-pi)*mp.exp(binlog(0,n,p))) if x==0 else mp.log1p(-pi)+binlog(x,n,p)
        elif x==0:term=mp.log(mp.mpf(t['p0']))
        else:
            if p==0 and x==1:term=mp.mpf(0)
            elif p==1:term=binlog(x,n,p)
            else:term=binlog(x,n,p)-mp.log(-mp.expm1(n*mp.log1p(-p)))
            if c['family']=='zmbinom':term+=mp.log1p(-mp.mpf(t['p0']))
        total+=w*term
    return total

def assess():
    cases=json.loads((HERE/'cases.json').read_text());rows=[];summary={}
    for mode in ['native','double']:
        before=parse(HERE/f'before-{mode}.tsv');after=parse(HERE/f'after-{mode}.tsv')
        for c in cases:
            i=c['study_index'];a=before[i];b=after[i];r=dict(index=i,family=c['family'],mode=mode,before=a,after=b)
            if b['outcome'] in ['certified','uncertified']:
                ref=likelihood(c,b['coef']);error=float(abs(b['logLik']-ref)/max(1,abs(ref)))
                with mp.workdps(120):high=likelihood(c,b['coef'])
                assert abs(high-ref)<mp.mpf('1e-60')*max(1,abs(high))
                r.update(reference=str(ref),after_error=error,reference_drift=float(abs(high-ref)))
                if a['outcome'] in ['certified','uncertified']:
                    oldref=likelihood(c,a['coef']);r.update(before_reference=str(oldref),before_error=float(abs(a['logLik']-oldref)/max(1,abs(oldref))))
                r['contract_pass']=error<1e-10
            rows.append(r)
        subset=[r for r in rows if r['mode']==mode]
        certified=[r for r in subset if r['after']['outcome']=='certified']
        assert all(r.get('contract_pass',True) for r in subset)
        assert all(r['before']['outcome']==r['after']['outcome'] for r in subset)
        assert all(r['before']['coef']==r['after']['coef'] for r in certified)
        assert all(r['after_error']<=r['before_error']+64*sys.float_info.epsilon for r in certified)
        summary[mode]=dict(certified_coefficients_unchanged=True,certified_likelihoods_unchanged=sum(r['before']['logLik']==r['after']['logLik'] for r in certified),certified_worst_error_increase=max(r['after_error']-r['before_error'] for r in certified),cases=len(cases),outcomes=dict(collections.Counter(r['after']['outcome'] for r in subset)),outcome_changes=[r['index'] for r in subset if r['before']['outcome']!=r['after']['outcome']],coefficient_changes=[r['index'] for r in subset if r['before']['coef']!=r['after']['coef']],worst_error=max(r.get('after_error',0) for r in subset),case629=next(r for r in subset if r['index']==629))
    (HERE/'replay-checks.json').write_text(json.dumps(rows,indent=2)+'\n')
    (HERE/'summary.json').write_text(json.dumps(summary,indent=2)+'\n')
    print(json.dumps(summary,indent=2),flush=True)

if __name__=='__main__':
    for arg in sys.argv[1:] or ['grid','replay','assess']:globals()[arg]()
