"""Independent represented-point likelihood/support checks (100 decimal digits)."""
import csv,json,pathlib,sys
from fractions import Fraction
import mpmath as mp
mp.mp.dps=100
HERE=pathlib.Path(__file__).resolve().parent
cases={c['index']:c for c in json.loads((HERE/'cases.json').read_text())}
def density(f,v,p):
 if f=='triang':
  a,b,c=p
  if not a<=v<=b:return mp.ninf
  ll=mp.log(2)-mp.log(b-a)
  if v<c:ll+=mp.log(v-a)-mp.log(c-a)
  if v>c:ll+=mp.log(b-v)-mp.log(b-c)
  return ll
 m,s,k=p;z=(v-m)/s
 if f=='gev':
  t=1+k*z
  if t<0 or (t==0 and k!=-1):return mp.ninf
  if k==-1:return -mp.log(s)+z-1
  L=mp.log(t)/k if k else z
  return -mp.log(s)-(1+k)*L-mp.exp(-L)
 a=s*k;t=a-z
 if t>50:
  term=total=mp.mpf(1)
  for j in range(1,81):term*=-(2*j-1)/(t*t);total+=term
  return mp.log(k)-z*z/2-mp.log(2*mp.pi)/2-mp.log(t)+mp.log(total)
 return mp.log(k)+a*(a/2-z)+mp.log(mp.erfc(t/mp.sqrt(2))/2)
def run(name):
 rows=[]
 for row in csv.reader((HERE/(name+'.csv')).open()):
  id=int(row[0]);f=row[1];status=int(row[2]);c=cases[id]
  if status!=0:rows.append(dict(index=id,family=f,status=status));continue
  public=[float(s) for s in row[4:7]];p=list(map(mp.mpf,public));values=[density(f,mp.mpf(v),p) for v in c['data']];ll=mp.fsum(values)
  finite=mp.isfinite(ll);reported=mp.mpf(float(row[3]));error=abs(ll-reported)/max(1,abs(ll)) if finite else mp.inf
  support=True
  if f=='gev':
   m,s,k=map(Fraction,public)
   support=all((t:=s+k*(Fraction(v)-m))>0 or (k==-1 and t==0) for v in c['data'])
  if f=='triang':support=all(public[0]<=v<=public[1] for v in c['data'])
  rows.append(dict(index=id,family=f,status=status,support=support,raw_loglik=str(ll),reported_loglik=row[3],relative_loglik_error=str(error),valid=bool(support and finite and error<mp.mpf('1e-10'))))
 (HERE/(name+'-validation.json')).write_text(json.dumps(rows,indent=2)+'\n')
 print(name,'rows',len(rows),'Ok',sum(r['status']==0 for r in rows),'invalid',[(r['index'],r.get('relative_loglik_error')) for r in rows if r['status']==0 and not r['valid']])
 if name.startswith('after'):
  assert all(r['status']!=0 or r['valid'] for r in rows), ('invalid restored Ok fit',name)
for name in sys.argv[1:] or ['before','after']:run(name)
