"""Audit all fourteen stats routes against active independent truth rows.

This is measurement only: it never changes R routing or stats functions.
"""
import json,math,subprocess,sys
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4];HERE=Path(__file__).resolve().parent
sys.path.insert(0,str(HERE));import audit
from quality.truth import DISTS,read

def rn(v):
 v=float(v)
 return 'Inf' if v==math.inf else '-Inf' if v==-math.inf else repr(v)
def main():
 calls=[];records=[]
 for folder in sorted(DISTS.iterdir()):
  if not (folder/'truth.json').exists():continue
  meta,g=read(folder);density=meta.get('r',{}).get('density','')
  if not density.startswith('stats::'):continue
  table=json.loads((folder/'truth.json').read_text())
  for category in ('values','quantiles'):
   # Include explicitly archived new rows as stats findings as well.
   rows=table[category]+[r['row'] for r in table.get('excluded_'+category,[])]
   for row in rows:
    theta=dict(zip(table['parameter_names'],table['parameters'][row[0]]))
    args=meta.get('r',{}).get('args',{n:n for n in theta})
    params={n:theta[str(expr)] if str(expr) in theta else eval(str(expr),{'__builtins__':{}},theta) for n,expr in args.items()}
    spec=[('d',row[1],True,True,float(row[2]),False,None)] if category=='values' else [('q',row[3],bool(row[1]),bool(row[2]),float(row[4]),True,row[5:9])]
    if category=='values':
     for lo,b in [(True,float(row[3])),(False,float(row[4]))]:
      spec.append(('p',row[1],lo,True,b,False,None))
      if math.exp(b)>1e-300:spec.append(('p',row[1],lo,False,math.exp(b),True,None))
    for kind,x,lo,lg,expected,rel,bracket in spec:
     fn=density.replace('::d','::'+kind,1)
     actualargs=[rn(x)]+[n+'='+rn(v) for n,v in params.items()]
     actualargs+=['log='+('TRUE' if lg else 'FALSE')] if kind=='d' else ['lower.tail='+('TRUE' if lo else 'FALSE'),'log.p='+('TRUE' if lg else 'FALSE')]
     calls.append('cat(suppressWarnings(tryCatch('+fn+'('+','.join(actualargs)+'),error=function(e) NaN)),"\\n")')
     records.append(dict(family=folder.name,function=fn,input=x,parameters=theta,lower=lo,log=lg,expected=expected,relative=rel,bracket=bracket))
 path=ROOT/'build/scratch/dpqr-extreme/stats-probe.R';path.write_text('options(digits=17)\n'+'\n'.join(calls)+'\n')
 out=subprocess.run(['Rscript',str(path)],capture_output=True,text=True,check=True)
 actual=list(map(float,out.stdout.split()));assert len(actual)==len(records)
 failed=[]
 for a,r in zip(actual,records):
  e=audit.error(a,r['expected'],r['relative']);ok=e<=(1e-9 if r['bracket'] else 1e-10)
  if r['bracket']:
   l,h,amb,floor=map(float,r['bracket']);ok|=(a==l or a==h if amb else l<=a<=h) or (math.isfinite(a) and math.isfinite(r['expected']) and abs(a-r['expected'])<=floor)
  if not ok:failed.append(dict(r,actual=a,error=e))
 summary=dict(checks=len(records),families=sorted({r['family'] for r in records}),failures=failed)
 (HERE/'stats-findings.json').write_text(json.dumps(summary,indent=2)+'\n')
 print('stats',len(records),'checks',len(failed),'failures')
if __name__=='__main__':main()
