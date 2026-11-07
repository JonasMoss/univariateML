#!/usr/bin/env python3
"""Resumable seeded model-only calibration; failure accounting is unconditional."""
import os,json,subprocess,hashlib,math,statistics,sys
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4];HERE=Path(__file__).parent
os.environ.setdefault('CCACHE_DIR',str(Path.home()/'.cache/ccache/univariateML'))
paths=[HERE/'calibrate.cpp']+sorted((ROOT/'univariateML-cpp/include').rglob('*.hpp'))
h=hashlib.sha256()
for p in paths:h.update(p.read_bytes())
identity=h.hexdigest();OUT=ROOT/'build/scratch/infer-joins-mc'/identity[:16];OUT.mkdir(parents=True,exist_ok=True)
boost=os.environ.get('BOOST_INCLUDE_DIR','')
if not boost:boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip()
subprocess.run(['ccache','c++','-std=c++17','-O2','-I'+boost,'-I'+str(ROOT/'univariateML-cpp/include'),str(HERE/'calibrate.cpp'),'-o',str(OUT/'calibrate')],check=True)
R=int(os.environ.get('JOIN_REPS','300'));sizes=[200,800,3200]
truths=[('snorm',[0,1,1.5]),('snorm',[0,1,1]),('sstd',[0,1,5,1.5]),('sstd',[0,1,5,1]),('huber',[0,1,1]),('pig',[2,3]),('gpd',[1,-.25]),('gpd',[1,0]),('gpd',[1,.25]),('gev',[0,1,-.25]),('gev',[0,1,0]),('gev',[0,1,.25])]
results={}
for tag,(f,truth) in enumerate(truths):
 if len(sys.argv)>1 and f not in sys.argv[1:]:continue
 for n in sizes:
  label=f'{f}-{tag}-n{n}';path=OUT/(label+'.txt');lines=path.read_text().splitlines() if path.exists() else []
  assert all(int(l.split()[0])==i for i,l in enumerate(lines))
  if len(lines)<R:
   process=subprocess.Popen([str(OUT/'calibrate'),f,str(n),str(R),str(len(lines)),str(tag)],stdout=subprocess.PIPE,text=True,bufsize=1)
   with path.open('a') as dest:
    for line in process.stdout:dest.write(line);dest.flush();lines.append(line.strip())
   assert process.wait()==0
  assert len(lines)==R
  good=[[float(v) for v in l.split()[2:]] for l in lines if l.split()[1]=='ok'];fail=[l for l in lines if l.split()[1]!='ok']
  names={'snorm':['mean','sd','xi'],'sstd':['mean','sd','nu','xi'],'huber':['mu','sigma','epsilon'],'pig':['mean','shape'],'gpd':['sigma','xi'],'gev':['mu','sigma','xi']}[f]
  item=dict(family=f,n=n,truth=truth,held={'mu':0} if f=='gpd' else {},free=names,status='pointwise model calibration',replications=R,successes=len(good),failures=fail,coordinates=[])
  for j,t in enumerate(truth):
   estimates=[r[3*j] for r in good];record=dict(truth=t,mean=statistics.mean(estimates),empirical_sd=statistics.stdev(estimates))
   for k,name in [(1,'observed'),(2,'sandwich')]:
    covered=sum(abs(r[3*j]-t)<=1.959963984540054*r[3*j+k] for r in good)
    # Wilson interval on unconditional coverage: failures count as noncoverage.
    z=1.959963984540054;phat=covered/R;den=1+z*z/R;center=(phat+z*z/(2*R))/den
    half=z*math.sqrt(phat*(1-phat)/R+z*z/(4*R*R))/den
    record[name]=dict(covered=covered,coverage_all=phat,coverage_success=covered/len(good),wilson95=[center-half,center+half],mean_se=statistics.mean(r[3*j+k] for r in good))
   item['coordinates'].append(record)
  results[label]=item
  print(label,'successes',len(good),'/',R,'coverage',[(round(c['observed']['coverage_all'],3),round(c['sandwich']['coverage_all'],3)) for c in item['coordinates']],flush=True)
  (HERE/'calibration.json').write_text(json.dumps(dict(seed=20261107,source_identity=identity,replications=R,results=results),indent=2)+'\n')
