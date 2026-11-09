"""Independent 70-digit integrand derivatives, at corners and interiors."""
import json,pathlib,mpmath as mp
mp.mp.dps=70
folder=pathlib.Path(__file__).resolve().parent
rows=json.loads((folder/'corner-bounds.json').read_text())
data=list(map(mp.mpf,[0,1e-12,1e-6,.03,.2,.7,1]));checks=0
for row in rows:
 lo,hi=map(mp.mpf,row['xi']);a,b=map(mp.mpf,row['v'])
 for k in [lo,(lo+hi)/2,hi]:
  for v in [a,(a+b)/2,b]:
   L=[];X=[];V=[];XX=[];XV=[];VV=[]
   for d in data:
    z=d/v;t=1+k*z
    l=mp.log1p(k*z)/k if k else z
    lx=(z/t-l)/k if k else -z*z/2
    lxx=(-(z/t)**2-2*lx)/k if k else 2*z**3/3
    L.append(l);X.append(lx);XX.append(lxx)
    V.append(-z/(v*t));XV.append(z*z/(v*t*t));VV.append(z*(2+k*z)/(v*v*t*t))
   weights=[mp.exp(-l) for l in L];total=mp.fsum(weights)
   E=lambda q:mp.fsum(w*t for w,t in zip(weights,q))/total
   M=lambda q:mp.fsum(q)/len(q)
   square=lambda q:[t*t for t in q]
   val=-mp.log(v)-mp.log(total/len(data))-(1+k)*M(L)-1
   sx=E(X)-M(L)-(1+k)*M(X);sv=E(V)-1/v-(1+k)*M(V)
   sxx=E(XX)-E(square(X))+E(X)**2-2*M(X)-(1+k)*M(XX)
   sxv=E(XV)-E([x*y for x,y in zip(X,V)])+E(X)*E(V)-M(V)-(1+k)*M(XV)
   svv=E(VV)-E(square(V))+E(V)**2+1/v**2-(1+k)*M(VV)
   for value,(l,h) in zip([val,sx,sv,sxx,sxv,svv,sv,svv],row['jet']):
    assert mp.mpf(l)<=value<=mp.mpf(h),(row,k,v,value,l,h)
    checks+=1
print(json.dumps({'digits':70,'rectangles':len(rows),'comparisons':checks,'passed':True},indent=2))
