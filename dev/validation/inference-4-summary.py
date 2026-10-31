#!/usr/bin/env python3
"""Post-run diagnostics and optional figures; run under the CPU slot lock."""
import json, math, os, subprocess
from pathlib import Path
import numpy as np
ROOT=Path(__file__).resolve().parents[2]
OUT=ROOT/'dev/research/evaluations/inference-4'
r=json.loads((OUT/'results.json').read_text())
# Replay the only interior cells with unavailable covariance, preserving seed.
# This records statuses and estimates rather than assuming failure causes.
details=[]
for index in [3,5]:
 rng=np.random.default_rng(np.random.SeedSequence([r['seed'],index,10,0]))
 samples=[rng.gamma(5.,.4,10) for _ in range(r['reps'])]
 request=''.join('gamma fit 1 1 10 '+' '.join(format(v,'.17g') for v in x)+'\n' for x in samples)
 response=subprocess.run([str(ROOT/'build/scratch/inference-4-probe')],input=request,text=True,capture_output=True,check=True).stdout.splitlines()
 target_action='targetmean' if index==3 else 'targetshape'
 truth=2. if index==3 else 5.
 target_request=''.join(f'gamma {target_action} {truth} 1 10 '+' '.join(format(v,'.17g') for v in x)+'\n' for x in samples)
 targets=subprocess.run([str(ROOT/'build/scratch/inference-4-probe')],input=target_request,text=True,capture_output=True,check=True).stdout.splitlines()
 for rep,(line,target_line,x) in enumerate(zip(response,targets,samples)):
  row=list(map(float,line.split()));statuses=[int(row[i]) for i in [0,4,5,10,15]]
  target_row=list(map(float,target_line.split()));target_statuses=[int(target_row[k]) for k in [2,4,6,8]]
  if any(statuses+target_statuses):details.append(dict(cell=f'{index}-10-0',replicate=rep,statuses=dict(zip(['fit','derivatives','F','H','sandwich'],statuses)),target_statuses=dict(zip(['delta','Wald','Rao','LR'],target_statuses)),shape=row[1],scale=row[2],observations=x.tolist()))
(OUT/'diagnostics.json').write_text(json.dumps(details,indent=2)+'\n')
# Independent analytic evaluation demonstrates gamma mean score topology even
# where the production natural-coordinate matrix is rightly rejected.
from scipy import optimize,special
x=np.tile(np.array([.2,.5,1.,2.,4.]),20);n=len(x);topology=[]
for log_mean in [math.log(x.mean()),3.,10.,100.]:
 q=1+np.log(x).mean()-log_mean-x.mean()*math.exp(-log_mean)
 fn=lambda z:math.log(math.exp(z))-special.digamma(math.exp(z))+q
 a=math.exp(optimize.brentq(fn,-15,10,xtol=1e-13))
 statistic=n*a*(1-x.mean()*math.exp(-log_mean))**2
 topology.append(dict(log_null_mean=log_mean,constrained_shape=a,rao=statistic,accepted=bool(statistic<=3.841458820694124)))
(OUT/'score-topology.json').write_text(json.dumps(dict(n=n,data='20 repetitions of [.2,.5,1,2,4]',points=topology,scope='independent analytic calculation; no coordinate-conditioning override of production API'),indent=2)+'\n')
print('Replayed interior covariance failures:',len(details))
print('Gamma score topology:',topology)
os.environ.setdefault('MPLCONFIGDIR',str(ROOT/'build/tmp/matplotlib'))
try:
 import matplotlib
 matplotlib.use('Agg')
 import matplotlib.pyplot as plt
except ImportError:
 print('Matplotlib unavailable; retained tables are the complete calibration artifact.')
else:
 fig,axes=plt.subplots(5,2,figsize=(10,15),layout='constrained')
 targets=[('norm','mean'),('gamma','mean'),('gamma','shape'),('beta','mean'),('pois','rate')]
 labels={'wald_F':'Model Wald','wald_S':'Sandwich Wald','score':'Rao set','LR':'Profile LR','exact':'Exact reference'}
 colors={'wald_F':'#0072b2','wald_S':'#d55e00','score':'#009e73','LR':'#cc79a7','exact':'#222222'}
 for i,(f,t) in enumerate(targets):
  for j,miss in enumerate([False,True]):
   ax=axes[i,j]
   cells=[c for c in r['cells'] if c['family']==f and c['target']==t and c['misspecified']==miss]
   first=cells[0]['parameters'];cells=[c for c in cells if c['parameters']==first]
   for name in labels:
    if name not in cells[0]['procedures']:continue
    ps=np.array([c['procedures'][name]['covered']/c['reps'] for c in cells]);ns=[c['n'] for c in cells]
    se=np.sqrt(ps*(1-ps)/r['reps'])
    ax.errorbar(ns,ps,yerr=se,label=labels[name],color=colors[name],marker='o',markersize=3,linewidth=1,capsize=2)
   ax.axhline(.95,color='#777777',linestyle='--',linewidth=1)
   ax.set_xscale('log');ax.set_xticks([10,30,100,1000],['10','30','100','1000']);ax.grid(alpha=.15)
   ax.set_ylim((.55,1) if miss and f=='gamma' else (.8,1))
   ax.set_title(f"{f} {t}: {'misspecified' if miss else 'model'}, {first}",fontsize=10)
   ax.set_xlabel('Sample size');ax.set_ylabel('Coverage (all replications)')
   if i==0 and j==0:ax.legend(fontsize=8,loc='lower right')
   if i==0 and j==1:ax.legend(fontsize=8,loc='lower right')
 fig.suptitle('Nominal 95% confidence coverage; bars are one Monte Carlo SE\nFirst parameter point per target; full grid and failures in the tables',fontsize=12)
 fig.savefig(OUT/'coverage.png',dpi=150)
 plt.close(fig)
