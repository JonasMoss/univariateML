"""Exercise finite truth-table inputs with measure.py's standalone evaluator.
Run measure.py first and invoke this script under one reserved CPU slot.
"""
import json,math,pathlib,subprocess
ROOT=pathlib.Path(__file__).resolve().parents[4]
OUT=pathlib.Path(__file__).resolve().parent
rows=[];expected=[];kinds=[]
def add(family,mode,x,params,lower,log,ref):
 x=float(x)
 if not math.isfinite(x):return
 a,m,s=params if family==0 else (params[0],0,params[1])
 rows.append(' '.join(map(str,(family,mode,x,a,m,s,int(lower),int(log)))))
 expected.append(float(ref));kinds.append((family,mode,lower,log))
for family,name in enumerate(('frechet','invweibull')):
 t=json.loads((ROOT/f'univariateML-cpp/include/cppunivariate/distributions/{name}/truth.json').read_text())
 for i,x,ld,ll,lu in t['values']:
  add(family,0,x,t['parameters'][i],True,True,ld)
  for lower,ref in ((True,ll),(False,lu)):
   add(family,1,x,t['parameters'][i],lower,True,ref)
 for i,lower,log,p,ref,*rest in t['quantiles']:
  add(family,2,p,t['parameters'][i],lower,log,ref)
actual=subprocess.check_output([str(ROOT/'build/scratch/frechet-measure/current')],input='\n'.join(rows)+'\n',text=True).splitlines()
assert len(actual)==len(rows)
failures=[];worst={}
for row,ref,val,kind in zip(rows,expected,actual,kinds):
 val=float(val)
 if ref==val:err=0.
 elif not math.isfinite(ref) or not math.isfinite(val):err=math.inf
 else:err=abs(val-ref)/(max(1,abs(ref)) if kind[1]!=2 else max(1e-300,abs(ref)))
 key=str(kind)
 worst[key]=max(worst.get(key,0),err)
 if err>1e-10:failures.append({'input':row,'reference':ref,'actual':val,'error':err})
result={'rows':len(rows),'failures':failures,'worst_scaled_error':worst}
(OUT/'table-check.json').write_text(json.dumps(result,indent=2)+'\n')
print(json.dumps(result,indent=2))
assert not failures
