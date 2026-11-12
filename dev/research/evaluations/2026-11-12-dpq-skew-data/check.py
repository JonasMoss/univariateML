"""Replay immutable truth tables, exclusions, archived parameter members, 307 witnesses."""
import study,json,gzip,math,sys,collections
H=study.H;D=study.D

def jobs():
 out=[]
 for f in study.F:
  d=json.load(open(D/f/'truth.json'));ps=d['parameters'][:]
  vr=d['values']+[r['row'] for r in d.get('excluded_values',[])];qr=d['quantiles']+[r['row'] for r in d.get('excluded_quantiles',[])]
  for pt in d.get('excluded_parameter_points',[]):
   idx=len(ps);ps.append(pt['parameters']);vr += [[idx,*r[1:]] for r in pt['values']];qr += [[idx,*r[1:]] for r in pt['quantiles']]
  for r in vr:
   for op,lo,lg,b in [(0,1,1,r[2]),(1,1,1,r[3]),(1,0,1,r[4])]:out.append(dict(job=[f,op,r[1],lo,lg,ps[r[0]]],expected=float(b),source='truth',row=r))
   for lo,b in [(1,r[3]),(0,r[4])]:
    b=math.exp(float(b))
    if b>1e-300:out.append(dict(job=[f,1,r[1],lo,0,ps[r[0]]],expected=b,source='truth',row=r))
  for r in qr:out.append(dict(job=[f,2,r[3],r[1],r[2],ps[r[0]]],expected=float(r[4]),source='truth',row=r))
 old=json.load(gzip.open(study.ROOT/'dev/research/evaluations/2026-11-11-dpqr-extreme-data/skew-smoke.json.gz','rt'))
 for r in old['rows']:out.append(dict(job=r['job'],expected=r['expected'],source='skew',was_nan=math.isnan(r['actual'])))
 return out

def error(a,b,rel):
 if a==b:return 0
 if not math.isfinite(a) or not math.isfinite(b):return math.inf
 return abs(a-b)/(abs(b) if rel and b else max(1,abs(b)))
def main(label):
 rows=jobs()
 for mode in ['native','double']:
  vals=study.evaluate([r['job'] for r in rows],mode);fails=[];recovered=collections.Counter();nan=collections.Counter();worst=0
  for r,v in zip(rows,vals):
   a=float(v);j=r['job'];e=error(a,r['expected'],j[1]==2 or (j[1]==1 and not j[4]));tol=1e-9 if j[1]==2 else 1e-10
   if e>tol:
    fails.append(dict(r,actual=a,error=e))
    if math.isnan(a):nan[(r['source'],j[0],j[5][2] if j[0]!='st' else j[5][3],j[5][-1])]+=1
   else:
    worst=max(worst,e)
    if r.get('was_nan'):recovered[j[0]]+=1
  result=dict(checks=len(rows),failures=fails,nan_regions=[dict(region=list(k),count=v) for k,v in nan.items()],recovered_witnesses=dict(recovered),worst_pass=worst)
  (H/(label+'-'+mode+'.json')).write_text(json.dumps(result,indent=2)+'\n');print(label,mode,len(rows),'checks',len(fails),'failures','NaN',sum(nan.values()),'recovered',dict(recovered),flush=True)
if __name__=='__main__':main(sys.argv[1])
