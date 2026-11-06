"""Run after inference-numeric-registry.py: exact-null and known-nuisance checks.

This tests production dispatch, not covariance calibration. No fitting is done.
"""
import itertools,json,subprocess
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4]
probe=ROOT/'build/scratch/inference-numeric-registry/probe'
cases=[]
for family,p,pair,x in [('sn',[0,1,0],5,.2),('bnbinom',[2,3,2],5,1),('genbeta',[2,1,3,1],5,.4)]:
    n=3
    for mask in range(1,1<<n):
        expected=6 if mask & pair == pair else 0
        cases.append((family,p,mask,x,expected))
for family,p,masks in [('laplace',[0,1],[1,3]),('ged',[0,1,1],[1,3]),('ged',[0,1,.75],[1,3]),('sged',[0,1,1,1],[1,3,9,11]),('sged',[0,1,.75,1],[1,3,9,11])]:
    for mask in masks:cases.append((family,p,mask,.2,11))
request=''.join(f'{f} 1 {mask} '+ ' '.join(map(str,p+[0]*(8-len(p))))+f' {x}\n' for f,p,mask,x,_ in cases)
lines=subprocess.run([str(probe)],input=request,text=True,capture_output=True,check=True).stdout.splitlines()
assert len(lines)==len(cases)
results=[]
for case,line in zip(cases,lines):
    f,p,mask,x,want=case;got=int(line.split()[0]);assert got==want,(case,line)
    results.append(dict(family=f,parameters=p,free_mask=mask,observation=x,expected=want,actual=got))
(Path(__file__).with_name('regime-results.json')).write_text(json.dumps(results,indent=2)+'\n')
print(len(results),'production regime checks passed')
