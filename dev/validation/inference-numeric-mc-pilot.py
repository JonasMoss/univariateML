#!/usr/bin/env python3
"""Resumable model-only Monte Carlo for the current 11-family dispatch pilot.

Run in a single CPU slot, n=500 and 200 independently seeded replications per
family. Each line is flushed into a source-hash-specific scratch directory.
Comparisons are paired: MC uncertainty for mean(SE)-empirical SD uses the
sample SD of SE minus the empirical SD's influence value, divided by sqrt(R).
This pointwise sanity check does not establish coverage or asymptotic validity.
"""
import hashlib
import json
import math
import os
from pathlib import Path
import statistics
import subprocess
import shutil
import sys

ROOT=Path(__file__).resolve().parents[2]
os.environ.setdefault('CCACHE_DIR',str(Path.home()/'.cache/ccache/univariateML'))
os.environ.setdefault('CCACHE_BASEDIR',str(ROOT))
os.environ.setdefault('CCACHE_NOHASHDIR','1')
COMPILER=([shutil.which('ccache')] if shutil.which('ccache') else [])+[os.environ.get('CXX','c++')]

SRC=ROOT/'dev/validation/inference-numeric-mc-pilot.cpp'
INC=ROOT/'univariateML-cpp/include/cppunivariate'
FAMILIES={'norm':['mean','sd'],'gamma':['shape','scale'],'beta':['shape1','shape2'],
          'pois':['lambda'],'exp':['rate'],'geom':['prob'],'lnorm':['meanlog','sdlog'],
          'logitnorm':['mu','sigma'],'lgamma':['shapelog','ratelog'],
          'rayleigh':['sigma'],'binom':['prob']}
h=hashlib.sha256(SRC.read_bytes())
for path in sorted(INC.rglob('*.hpp')):
    h.update(str(path.relative_to(ROOT)).encode());h.update(path.read_bytes())
identity=h.hexdigest()
OUT=ROOT/'build/scratch/inference-numeric-mc-pilot'/identity[:16]
OUT.mkdir(parents=True,exist_ok=True)
boost=os.environ.get('BOOST_INCLUDE_DIR','')
cache=ROOT/'univariateML-cpp/build/CMakeCache.txt'
if not boost and cache.exists():
    boost=next((line.split('=',1)[1] for line in cache.read_text().splitlines() if line.startswith('Boost_INCLUDE_DIR:')),'')
binary=OUT/'probe'
subprocess.run(COMPILER+['-std=c++17','-O2','-I'+boost,
                '-I'+str(INC.parent),str(SRC),'-o',str(binary)],check=True)
selected=sys.argv[1:] or list(FAMILIES)
if any(f not in FAMILIES for f in selected): raise ValueError('unknown family')
results={}
for tag,family in enumerate(FAMILIES):
    if family not in selected: continue
    path=OUT/(family+'.txt')
    lines=path.read_text().splitlines() if path.exists() else []
    assert all(int(line.split()[0])==i for i,line in enumerate(lines)),(family,'noncontiguous checkpoint')
    if len(lines)<200:
        process=subprocess.Popen([str(binary),family,str(len(lines)),str(tag)],stdout=subprocess.PIPE,text=True,bufsize=1)
        with path.open('a') as dest:
            for line in process.stdout:
                assert int(line.split()[0])==len(lines),(family,line)
                lines.append(line.strip());dest.write(line);dest.flush()
        if process.wait()!=0: raise RuntimeError(f'probe failed for {family}')
    assert len(lines)==200,(family,len(lines))
    failures=[line for line in lines if line.split()[1]!='ok']
    good=[[float(x) for x in line.split()[2:]] for line in lines if line.split()[1]=='ok']
    n=len(good)
    entry={'replications':200,'successes':n,'failures':failures,'coordinates':{}}
    if n>=2:
        for j,name in enumerate(FAMILIES[family]):
            estimates=[row[3*j] for row in good];model=[row[3*j+1] for row in good];sandwich=[row[3*j+2] for row in good]
            assert all(math.isfinite(v) and v>=0 for v in model+sandwich)
            center=statistics.mean(estimates);sd=statistics.stdev(estimates)
            if sd==0: raise ValueError((family,name,'zero empirical SD'))
            influence=[((v-center)**2-sd**2)/(2*sd) for v in estimates]
            m4=statistics.mean((v-center)**4 for v in estimates)
            sd_mc=math.sqrt(max(0,m4-((n-3)/(n-1))*sd**4)/n)/(2*sd)
            coord={'empirical_sd':sd,'empirical_sd_mcse':sd_mc}
            for label,values in [('model',model),('sandwich',sandwich)]:
                mean=statistics.mean(values);mcse=statistics.stdev(values)/math.sqrt(n)
                diff_mcse=statistics.stdev(a-b for a,b in zip(values,influence))/math.sqrt(n)
                coord[label]={'mean_se':mean,'mean_se_mcse':mcse,'difference':mean-sd,
                              'difference_mcse_paired':diff_mcse,'flag_2mcse':abs(mean-sd)>2*diff_mcse}
            entry['coordinates'][name]=coord
    results[family]=entry
    print(f'{family}: {n}/200 successful',flush=True)
result={'revision':subprocess.check_output(['git','rev-parse','HEAD'],cwd=ROOT,text=True).strip(),
        'source_hash':identity,'n':500,'replications':200,'seed_rule':'20261101 + 1000*family_index + replication',
        'family_order':list(FAMILIES),'results':results}
(OUT/'summary.json').write_text(json.dumps(result,indent=2)+'\n')
print('results:',OUT/'summary.json',flush=True)
if any(r['failures'] for r in results.values()): raise SystemExit('replication failures recorded')
