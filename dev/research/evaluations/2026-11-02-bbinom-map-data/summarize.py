#!/usr/bin/env python3
import csv, math, collections, sys
from pathlib import Path
p=Path(__file__).resolve().parent
rows=list(csv.DictReader((p/'samples.csv').open()))
faces=list(csv.DictReader((p/'faces.csv').open()))
cert=list(csv.DictReader((p/'certified.csv').open()))
assert len(rows)==len(cert)
extfile=p/'extended.csv'
ext={} if '--base' in sys.argv or not extfile.exists() else {int(v['index']):v for v in csv.DictReader(extfile.open())}
summary=[]
for idx,r in enumerate(rows):
 x=[int(z) for z in r['data'].split()]; h=len(x); S=sum(x)
 c=cert[idx].copy(); f=faces[idx]; nmax=int(r['Nmax'])
 if idx in ext:
  z=ext[idx]; nmax=int(z['Nmax'])
  if float(z['loglik'])>float(c['loglik']):
   for k in ('N','status','loglik','alpha','beta'): c[k]=z[k]
  for k in ('tail_half','tail_end','tail_half_status','tail_end_status'): c[k]=z[k]
  c['failed']=str(int(c['failed'])+int(z['failed']))
 disp=h*sum(z*z for z in x)-S*S-h*S
 region='under' if disp<0 else 'over' if disp>0 else 'equal'
 lp=float(f['poisson']); bn=float(f['binom']); nb=float(f['nb']); grid=float(c['loglik']); tol=1e-5
 face=max(lp,bn,nb)
 if len(set(x))==1: winner='point'
 elif grid>face+tol and c['status']=='0': winner='interior'
 elif grid>face+tol and c['status']=='4': winner='endpoint'
 elif bn>max(nb,lp)+tol: winner='binomial'
 elif nb>max(bn,lp)+tol: winner='NB'
 else: winner='Poisson/tie'
 if winner=='interior' and int(c['N'])==nmax: winner='size unresolved'
 if winner=='NB' and f['nb_slope'] and float(f['nb_slope'])>1e-5: winner='tail increase'
 n=nmax; half=max(int(r['M']),n//2)
 delta=float(c['tail_end'])-nb; delta_half=float(c['tail_half'])-nb
 tail_ok=int(c['tail_end_status']) in (0,3,4,5,6) and int(c['tail_half_status']) in (0,3,4,5,6) and winner!='size unresolved' and half<n and abs(delta)<.001*h and abs(delta_half-(n/half)*delta)<.001*h
 summary.append(dict(index=idx,effective_Nmax=nmax,region=region,screen_winner=winner,grid_minus_face=grid-face,tail_check='pass' if tail_ok else 'unsettled',profile_failures=c['failed'],screen_minus_profile=float(r['grid'])-grid,optimizer_failed=r.get('optimizer_failed','0'),optimizer_boundary=r.get('optimizer_boundary','0'),nb_slope=f['nb_slope'] if f['nb_slope'] else 'nan'))
with (p/'classification.csv').open('w',newline='') as f:
 w=csv.DictWriter(f,fieldnames=summary[0]);w.writeheader();w.writerows(summary)
for region in ['under','equal','over']:
 q=[r for r in summary if r['region']==region]
 print(region,'n',len(q),'winners',dict(collections.Counter(r['screen_winner'] for r in q)),'tail_unsettled',sum(r['tail_check']=='unsettled' for r in q),'profile_failure',sum(int(r['profile_failures'])>0 for r in q),'optimizer_failure',sum(int(r['optimizer_failed'])>0 for r in q),'boundary',sum(int(r['optimizer_boundary'])>0 for r in q))
 if region=='over':
  for sign in ['positive','negative','near']:
   s=[r for r in q if math.isfinite(float(r['nb_slope'])) and ((float(r['nb_slope'])>1e-5) if sign=='positive' else (float(r['nb_slope'])< -1e-5) if sign=='negative' else abs(float(r['nb_slope']))<=1e-5)]
   print('slope',sign,len(s),dict(collections.Counter(r['screen_winner'] for r in s)))
print('sources',dict(collections.Counter(r['source'] for r in rows)))
print('total',len(rows))
