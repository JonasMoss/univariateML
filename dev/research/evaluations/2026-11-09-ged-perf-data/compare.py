import json,math,pathlib
out=pathlib.Path(__file__).resolve().parent
before=[v.split() for v in (out/'before-compare.txt').read_text().splitlines()]
after=[v.split() for v in (out/'after-compare.txt').read_text().splitlines()]
assert len(before)==len(after)==480
changes=[];max_error=0.;errors=[]
for b,a in zip(before,after):
 assert b[:4]==a[:4]
 if b[4:9]!=a[4:9]:changes.append(dict(case=b[:4],before=b[4:9],after=a[4:9]))
 if b[4]==a[4]=='0':
  for i in range(9,12):
   x,y=float(b[i]),float(a[i]); error=abs(x-y)/max(1.,abs(x));max_error=max(max_error,error)
   if error>1e-10:errors.append(dict(case=b[:4],coefficient=i-9,before=x,after=y,error=error))
record=dict(rows=len(before),stop_changes=changes,coefficient_errors=errors,max_scaled_coefficient_error=max_error)
(out/'comparison.json').write_text(json.dumps(record,indent=2)+'\n')
print(json.dumps(record,indent=2))
assert not changes and not errors
