#!/usr/bin/env python3
"""Verify frozen inputs and measure stored/native/wasm default fit errors.

Read-only git show HEAD:path is the baseline. Captures are JSON-lines;
R drift is CSV. Tolerances use componentwise relative error (absolute at
zero), rounded up on the 1,2,3,5,10 decimal lattice, at most 3x headroom,
including both the stored binary64 comparison and the 25-digit reference.
There is no blanket rounding floor; exact matches can use zero. A pure power-of-ten lattice cannot meet a
3x bound for errors just above a power of ten; 2 and 3 multipliers close
that unavoidable gap.
"""
import json,subprocess,math,csv,sys
from decimal import Decimal, localcontext
from pathlib import Path
from policy import explanation
root=Path(__file__).resolve().parents[4]
out=Path(__file__).resolve().parent
dists=root/'univariateML-cpp/include/cppunivariate/distributions'
def error(a,b):
 if isinstance(a,str):a=float(a)
 if isinstance(b,str):b=float(b)
 if a==b:return 0.
 if a is None or b is None:return math.inf
 return abs(a-b)/(abs(b) if b else 1)
def allowance(e):
 if e==0:return 0.
 base=10**math.floor(math.log10(e))
 return next(base*k for k in (1,2,3,5,10) if base*k>=e)
def raw_fields(text):
 decoder=json.JSONDecoder();pos=text.index('{')+1;fields={}
 while True:
  while text[pos].isspace() or text[pos]==',':pos+=1
  if text[pos]=='}':return fields
  key,pos=decoder.raw_decode(text,pos)
  pos=text.index(':',pos)+1
  while text[pos].isspace():pos+=1
  start=pos;_,pos=decoder.raw_decode(text,pos)
  if key in fields:raise ValueError(('duplicate top-level key',key))
  fields[key]=text[start:pos]

def main():
 captures_only='--captures-only' in sys.argv
 previous={r['id']:r for r in json.loads((out/'audit.json').read_text())} if captures_only else {}
 captures={}
 for mode in ('native','double','wasm','simbox-double','r-native'):
  path=out/f'{mode}.jsonl'
  if path.exists():captures[mode]={r['id']:r for r in map(json.loads,path.read_text().splitlines())}
 expected={p.parent.name for p in dists.glob('*/golden.json')}
 for mode in ('native','double','wasm','simbox-double','r-native'):
  assert mode in captures, ('missing capture',mode)
  assert set(captures[mode]) >= expected, ('incomplete capture',mode,expected-set(captures[mode]))
  for id,c in captures[mode].items():
   if id in expected:assert not c.get('error'), (mode,id,c)
 rpath=out/'r-drift.csv'
 if rpath.exists():captures['R']={r['family']:r for r in csv.DictReader(rpath.open())}
 changed = [] if captures_only else subprocess.check_output(['git','diff','--name-only','--','univariateML-cpp/include/cppunivariate/distributions/*/golden.json'],cwd=root,text=True).splitlines()
 previous_measurements=json.loads((out/'previous-measurements.json').read_text())
 rows=[]
 for path in sorted(dists.glob('*/golden.json')):
  g=json.loads(path.read_text());id=g['id'];rel=str(path.relative_to(root))
  if id=='bhatt':continue
  old_text=None if captures_only else subprocess.check_output(['git','show',f'HEAD:{rel}'],cwd=root,text=True)
  old=None if captures_only else json.loads(old_text)
  unexpected=[] if captures_only else [k for k in set(g)|set(old) if k not in ('estimates','logLik','reference') and (k not in g or k not in old or g[k]!=old[k])]
  if unexpected:raise ValueError((id,'frozen fields changed',unexpected))
  if not captures_only:
   before,after=raw_fields(old_text),raw_fields(path.read_text())
   changed_bytes=[k for k in before if k not in ('estimates','logLik','reference') and before[k]!=after[k]]
   if changed_bytes:raise ValueError((id,'frozen field text changed',changed_bytes))
  ref=g.get('reference');assert ref,id
  def high_error(a, b):
   if a is None:return math.inf
   if not math.isfinite(float(b)):return error(a,b)
   with localcontext() as ctx:
    ctx.prec=60
    actual=Decimal.from_float(float(a)); reference=Decimal(str(b))
    return float(abs(actual-reference)/(abs(reference) if reference else Decimal(1)))
  def fit_error(c):
   return max([max(error(c['estimates'][k],v),high_error(c['estimates'][k],ref['estimates'][k]))
      for k,v in g['estimates'].items()]+[max(error(c['logLik'],g['logLik']),high_error(c['logLik'],ref['logLik']))])
  errors={mode:float(c[id]['max_relative_difference']) if mode=='R' else fit_error(c[id])
    for mode,c in captures.items() if id in c}
  components={}
  for mode,c in captures.items():
   if mode=='R' or id not in c:continue
   r=c[id]
   components[mode]={k:max(error(r['estimates'][k],v),high_error(r['estimates'][k],ref['estimates'][k])) for k,v in g['estimates'].items()}
   components[mode]['logLik']=max(error(r['logLik'],g['logLik']),high_error(r['logLik'],ref['logLik']))
  old_error=previous[id]['old_error'] if captures_only else fit_error(old)
  measured=max(errors.values())
  assert math.isfinite(measured),(id,errors)
  tolerance=allowance(measured)
  assert tolerance <= 3*measured if measured else tolerance==0
  rows.append({'id':id,'previous':previous_measurements[id],'reason':explanation(id,tolerance),'method':ref['method'],'free':ref['free'],'not_refined':ref['not_refined'],
    'global':ref['global'],'old_error':old_error,'measured':errors,'components':components,'tolerance':tolerance})
 (out/'audit.json').write_text(json.dumps(rows,indent=2)+'\n')
 if captures_only:
  print(f'{len(rows)} captures recalibrated; frozen inputs were not rechecked')
  return
 print(f'{len(rows)} frozen samples verified ({len(changed)} changed goldens from git diff); '+str({m:len(c) for m,c in captures.items()}))
if __name__=='__main__':main()
