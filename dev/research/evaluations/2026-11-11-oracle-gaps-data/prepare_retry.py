"""After the first foreground run ends, preserve receipts and retry its gaps.

This launcher edits reference code before acquiring CPU slots. It never holds
slots while preparing files, and the retry itself has at most four workers.
"""
import difflib,json,os,shutil,subprocess,sys
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4]
HERE=Path(__file__).resolve().parent
DISTS=ROOT/'univariateML-cpp/include/cppunivariate/distributions'


def main():
    ids=[]
    for path in sorted((HERE/'generation').glob('*.json')):
        receipt=json.loads(path.read_text())
        if 'records' not in receipt or any(r['status']=='oracle gap' for r in receipt['records']):ids.append(path.stem)
    if not ids:
        print('No first-pass oracle gaps require retry.',flush=True)
        return 0
    planned={}
    for name in ('beta','betapr','gb2','fpareto'):
        path=DISTS/name/'truth.py';old=path.read_text()
        assert old.count('def build(t):')==1,name
        assert old.count('    return beta_normalization(ref,')==1,name
        new=old.replace('def build(t):','def build(t, logd=None):',1)
        new=new.replace('    return beta_normalization(ref,','    ref.log_density=logd\n    return beta_normalization(ref,',1)
        planned[path]=(old,new)
    for name in ('ht','sstd'):
        path=DISTS/name/'truth.py';old=path.read_text()
        assert old.count('def build(t):')==1 and old.count('    return out')==1,name
        new=old.replace('def build(t):','def build(t, logd=None):',1)
        new=new.replace('    return out','    out.log_density=logd\n    return out',1)
        planned[path]=(old,new)
    for name,degrees in (('lst','df'),('std','nu')):
        path=DISTS/name/'truth.py';old=path.read_text()
        lines=old.splitlines(keepends=True)
        candidates=[i for i,line in enumerate(lines) if line.startswith('    return Reference(')]
        assert len(candidates)==1,name
        i=candidates[0]
        lines[i]=lines[i].replace('return Reference(','out=Reference(',1)
        lines.insert(i+1,"    out.log_density=logd\n    degrees=t[%r]\n    C=mp.exp(mp.loggamma((degrees+1)/2)-mp.loggamma(degrees/2)-mp.log(degrees*mp.pi)/2)\n    # Complete Student beta integral after the location-scale map.\n    out.normalization_identity=C*mp.sqrt(degrees)*mp.beta(mp.mpf('.5'),degrees/2)\n    return out\n"%degrees)
        new=''.join(lines).replace('def build(t):','def build(t, logd=None):',1)
        planned[path]=(old,new)
    path=ROOT/'dev/validation/quality/truth_reference.py';old=path.read_text()
    needle='value,error=mp.quad(lambda v:mp.exp(logkernel(v)),cuts,error=True)'
    assert old.count(needle)==1
    new=old.replace(needle,"value,error=mp.quad(lambda v:mp.exp(logkernel(v)),cuts,method='gauss-legendre',maxdegree=8,error=True)")
    planned[path]=(old,new)
    # These three listed points already have committed references from TASK-187.
    # Remove only this job's unnecessary inverse override, retaining that work.
    for name in ('frechet','invweibull'):
        path=DISTS/name/'truth.py';old=path.read_text()
        baseline=subprocess.check_output(['git','show','7825e912:'+str(path.relative_to(ROOT))],cwd=ROOT,text=True)
        restored=''.join(line for line in old.splitlines(keepends=True) if not line.startswith('    out.quantile = lambda lp, lower:'))
        assert restored==baseline,name
        planned[path]=(old,baseline)
    archive=HERE/'generation-pass1'
    if archive.exists():raise FileExistsError('First-pass receipt archive already exists')
    shutil.copytree(HERE/'generation',archive)
    shutil.copyfile(ROOT/'build/scratch/generation.log',HERE/'generation-pass1.log')
    patch=[]
    for path,(old,new) in planned.items():
        relative=str(path.relative_to(ROOT))
        patch.extend(difflib.unified_diff(old.splitlines(keepends=True),new.splitlines(keepends=True),fromfile='a/'+relative,tofile='b/'+relative))
    (HERE/'retry-optimization.patch').write_text(''.join(patch))
    for path,(_,new) in planned.items():path.write_text(new)
    (HERE/'retry-ids.json').write_text(json.dumps(ids,indent=2)+'\n')
    env=dict(os.environ)
    temp=str(ROOT/'build/tmp')
    env.update(TMPDIR=temp,JUST_TEMPDIR=temp,XDG_RUNTIME_DIR=temp,
               TRUTH_EXTREME_TIMEOUT='600',TRUTH_EXTREME_WORKERS='4',TRUTH_FORCE_GAPS='1')
    print('Retrying at 160 digits, 600 seconds/point:',','.join(ids),flush=True)
    with (ROOT/'build/scratch/generation-retry.log').open('w') as log:
        result=subprocess.run(['/tmp/umvl-cpu/run4',sys.executable,str(HERE/'generate.py'),*ids],cwd=ROOT,env=env,stdout=log,stderr=subprocess.STDOUT)
    return result.returncode


if __name__=='__main__':sys.exit(main())
