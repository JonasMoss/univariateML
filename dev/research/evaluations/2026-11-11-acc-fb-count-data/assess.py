"""Join independent stationary points to baseline/refined native/double rows."""
import collections,json,pathlib,mpmath as mp
mp.mp.dps=80
P=pathlib.Path(__file__).resolve().parent
cs=json.loads((P/'cases.json').read_text())
def read(path):return {int(a[0]):a for a in (l.split('\t') for l in path.read_text().splitlines())}
def theta(a):return {k:mp.mpf(float(v)) for k,v in (s.split('=',1) for s in a[4:] if '=' in s)}
def route(c):
 f=c['family'];h=c['held']
 if f in ['hyper','nhyper']:return f+'/integer'
 if f=='bbinom':return f+('/held-both' if 'alpha' in h and 'beta' in h else '/held-alpha' if 'alpha' in h else '/held-beta' if 'beta' in h else '/two-shapes')
 if f=='bnbinom':return f+('/scalar-alpha-rescue' if c['sample']=='scalar-rescue' else '/all-held' if len(h)==3 else '/all-free' if not h else '/held-'+','.join(sorted(h)))
 if f=='skellam':return f+('/joint' if not h else '/held-rate')
 if f=='tbinom':return f+('/held-prob' if 'prob' in h else '/free-prob')
 if f=='ztbinom':return f+('/held-prob' if 'prob' in h else '/free-prob')
 if f=='zmbinom':return f+('/held-prob' if 'prob' in h else '/free-prob')
 return f+('/Bernoulli-curve' if c.get('sample')=='nonidentified-Bernoulli-curve' else '/exact-flat-held-pi' if c.get('sample')=='exact-flat-root' else '/flat-held-pi' if c['study_index']==1068 else '/held-prob' if 'prob' in h else '/held-pi' if 'pi' in h else '/free-pi')
result={}
for mode in ['native','double']:
 before=read(P/f'before-{mode}.tsv');after=read(P/f'after-{mode}.tsv')
 refs={r['index']:r for r in json.loads((P/f'before-{mode}-errors.json').read_text())['rows']}
 rows=[]
 for i,r in refs.items():
  c=cs[i];a=after.get(i)
  if not a:continue
  if 'error' in r:raise RuntimeError(r)
  ref={k:mp.mpf(v) for k,v in r['reference'].items()};free=[k for k in ref if k not in c['held']]
  row=dict(index=i,route=route(c),tol=c.get('tol',1e-10),before=r['max_error'],after_outcome=a[2])
  nearest={k:float(abs(mp.mpf(float(ref[k]))-ref[k])/max(1,abs(ref[k]))) for k in free}
  row['nearest_double']=nearest
  if a[2] in ['certified','uncertified','unflagged']:
   t=theta(a)
   for key,v in c['held'].items():assert t[key]==mp.mpf(float(v)),(mode,i,'hold changed',key)
   if c.get('sample')=='nonidentified-Bernoulli-curve':
    mean=mp.mpf(sum(c['data']))/len(c['data']);ref['prob']=max(mean,t['prob']);ref['pi']=1-mean/ref['prob']
    row['after_reference']={key:str(v) for key,v in ref.items()}
   # Integer selections must agree with those used by the oracle. The BNB
   # size is continuous and is intentionally included in the error check.
   if c['family']!='bnbinom' and 'size' in t:assert t['size']==ref['size'],(i,mode,'size changed')
   error={k:float(abs(t[k]-ref[k])/max(1,abs(ref[k]))) for k in free}
   row.update(after=max(error.values(),default=0),errors=error)
  rows.append(row)
 summary=[]
 for name in sorted({r['route'] for r in rows}):
  z=[r for r in rows if r['route']==name and r['tol']>=8*2.220446049250313e-16];ok=[r for r in z if 'after' in r]
  summary.append(dict(route=name,n=len(z),max_before=max((r['before'] for r in z),default=0),max_after=max((r['after'] for r in ok),default=0),stops=[r['index'] for r in z if 'after' not in r]))
 failures=[r for r in rows if 'after' in r and r['after']>r['tol']]
 changes=[dict(index=i,before=a[2],after=after[i][2]) for i,a in before.items() if i in after and a[2]!=after[i][2] and cs[i]['family']!='skellam']
 result[mode]=dict(rows=rows,summary=summary,contract_failures=failures,outcome_changes=changes)
old=read(P.parent/'2026-11-10-fallback-count-data/after-final.tsv');an=read(P/'after-native.tsv');ad=read(P/'after-double.tsv')
cert=[i for i,a in old.items() if a[2]=='certified']
result['certified_preservation']=dict(rows=len(cert),native_text_identical=sum(old[i]==an.get(i) for i in cert),native_changes=[i for i in cert if old[i]!=an.get(i)])
bd=read(P/'before-double.tsv')
result['certified_preservation']['double_rows']=sum(bd[i][2]=='certified' for i in bd)
result['certified_preservation']['double_text_identical']=sum(bd[i]==ad.get(i) for i in bd if bd[i][2]=='certified')
bn=read(P/'before-native.tsv')
extra_cert=[i for i in bn if bn[i][2]=='certified']
result['certified_preservation']['native_extra_rows']=len(extra_cert)
result['certified_preservation']['native_extra_text_identical']=sum(bn[i]==an.get(i) for i in extra_cert)
# Class labels for Skellam are harness annotations. The library has no flag.
for mode in ['native','double']:
 b=read(P/f'before-{mode}.tsv');a=read(P/f'after-{mode}.tsv')
 for f in ['bnbinom','skellam']:
  ids=[i for i in b if cs[i]['family']==f and i in a]
  result.setdefault('unchanged',{}).setdefault(mode,{})[f]=dict(rows=len(ids),all_payloads_identical=all(b[i][3:]==a[i][3:] for i in ids))
(P/'assessment.json').write_text(json.dumps(result,indent=2)+'\n')
print(json.dumps({k:{s:v for s,v in a.items() if s!='rows'} if isinstance(a,dict) else a for k,a in result.items()},indent=2))
assert all(not result[m]['contract_failures'] for m in ['native','double'])
assert result['certified_preservation']['native_text_identical']==len(cert)
assert result['certified_preservation']['native_extra_text_identical']==len(extra_cert)
assert result['certified_preservation']['double_text_identical']==result['certified_preservation']['double_rows']
assert all(not z['stops'] for m in ['native','double'] for z in result[m]['summary'])
for mode in ['native','double']:
 assert {r['index'] for r in result[mode]['outcome_changes']}=={1059,1060,1061,1069,1070,1071,1072,1262}
 assert all(cs[r['index']].get('tol',1e-10)==1e-18 for r in result[mode]['outcome_changes'])
