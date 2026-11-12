"""Correct newly added rows without changing the committed historical payload."""
import difflib,json,os,shutil,subprocess,sys
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4]
HERE=Path(__file__).resolve().parent
DISTS=ROOT/'univariateML-cpp/include/cppunivariate/distributions'

def main():
    archive=HERE/'generation-pass4'
    if archive.exists():raise FileExistsError(archive)
    shutil.copytree(HERE/'generation',archive)
    shutil.copyfile(ROOT/'build/scratch/generation-closure.log',HERE/'generation-pass4.log')
    planned={}
    p=ROOT/'dev/validation/quality/truth_reference.py';old=p.read_text()
    needle='''        if x==m:
            if inverse:return -mp.inf
            if b==1:return mp.log(a/s)
'''
    replacement='''        if x==m:
            if inverse:
                power=a*b-1
                return mp.inf if power<0 else -mp.inf if power>0 else mp.log(a*b/s)
            if b==1:return mp.log(a/s)
'''
    assert old.count(needle)==1
    new=old.replace(needle,replacement)
    new+='''\n\ndef conditioned_positive_tails(tails, zero):
    """Stable tails of a count law conditioned on X>0."""
    ll,lu=tails;l0,u0=zero
    if ll<-mp.log(2):
        return pair(ll+complement(l0-ll)-u0)
    return pair(lu-u0,False)
'''
    planned[p]=(old,new)
    for name in ('ztpois','ztnbinom'):
        p=DISTS/name/'truth.py';old=p.read_text()
        new=old.replace('import mpmath as mp','import mpmath as mp\nfrom quality.truth_reference import conditioned_positive_tails',1)
        needle='''    survival0=base(0)[1]
    def tails(x):
        small=base(x)[1]+mp.log(weight)-survival0
        return pair(small,False)
'''
        replacement='''    zero=base(0)
    def tails(x):
        return conditioned_positive_tails(base(x),zero)
'''
        assert new.count(needle)==1,name
        planned[p]=(old,new.replace(needle,replacement))
    p=DISTS/'tnorm/truth.py';old=p.read_text()
    needle='''    z=interval((a-m)/s,(b-m)/s)
    def tails(x):
        l=interval((a-m)/s,(x-m)/s)-z
        h=interval((x-m)/s,(b-m)/s)-z
'''
    replacement='''    def physical_interval(l,h):
        if mp.isfinite(l) and mp.isfinite(h):
            width=(h-l)/s
            start=(l-m)/s
            if width*(1+abs(start)+abs(width))<mp.mpf('1e-35'):
                # Keep the physical width before subtracting a huge mean.
                # Midpoint relative error is below 1e-70 in this branch.
                return mp.log(width)-mp.log(2*mp.pi)/2-start*start/2-start*width/2-width*width/8
        return interval((l-m)/s,(h-m)/s)
    z=physical_interval(a,b)
    def tails(x):
        l=physical_interval(a,x)-z
        h=physical_interval(x,b)-z
'''
    assert old.count(needle)==1
    planned[p]=(old,old.replace(needle,replacement))
    patch=[]
    for p,(old,new) in planned.items():
        rel=str(p.relative_to(ROOT))
        patch.extend(difflib.unified_diff(old.splitlines(keepends=True),new.splitlines(keepends=True),fromfile='a/'+rel,tofile='b/'+rel))
    (HERE/'reference-corrections.patch').write_text(''.join(patch))
    for p,(_,new) in planned.items():p.write_text(new)
    ids=['invburr','invparalogis','invpareto','tnorm','ztpois','ztnbinom']
    removed={}
    for name in ids:
        p=DISTS/name/'truth.json';current=json.loads(p.read_text())
        old=json.loads(subprocess.check_output(['git','show','7825e912:'+str(p.relative_to(ROOT))],cwd=ROOT,text=True))
        for key in ('parameters','values','quantiles','normalization','excluded_values','excluded_quantiles','excluded_parameter_points'):
            assert current.get(key,[])[:len(old.get(key,[]))]==old.get(key,[]),(name,key)
        removed[name]={key:current.get(key,[])[len(old.get(key,[])):] for key in ('parameters','values','quantiles','normalization','excluded_values','excluded_quantiles','excluded_parameter_points')}
        p.write_text(json.dumps(old,indent=2,allow_nan=False)+'\n')
    (HERE/'reference-corrections-before.json').write_text(json.dumps(removed,indent=2,allow_nan=False)+'\n')
    env=dict(os.environ,TMPDIR=str(ROOT/'build/tmp'),JUST_TEMPDIR=str(ROOT/'build/tmp'),
             XDG_RUNTIME_DIR=str(ROOT/'build/tmp'),TRUTH_EXTREME_TIMEOUT='600',
             TRUTH_EXTREME_WORKERS='2',TRUTH_FORCE_GAPS='1')
    print('Regenerating only this job\'s rows after reference corrections:',','.join(ids),flush=True)
    with (ROOT/'build/scratch/generation-corrections.log').open('w') as log:
        return subprocess.run(['/tmp/umvl-cpu/run4',sys.executable,str(HERE/'generate.py'),*ids],cwd=ROOT,env=env,stdout=log,stderr=subprocess.STDOUT).returncode

if __name__=='__main__':sys.exit(main())
