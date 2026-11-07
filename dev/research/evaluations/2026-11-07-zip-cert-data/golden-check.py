import json,subprocess,sys
from pathlib import Path
g=json.loads(Path('univariateML-cpp/include/cppunivariate/distributions/zip/golden.json').read_text())
p=g['estimates'];want=[p['lambda'],p['pi'],g['logLik']]
request=' '.join(map(str,[len(g['data']),*map(int,g['data']),*want[:2]]))+'\n'
rows=[]
for row in subprocess.check_output([sys.argv[1]],input=request,text=True).splitlines():
    mask,status,*values=row.split();values=list(map(float,values));errors=[abs(a-b)/(1+abs(b)) for a,b in zip(values,want)]
    assert int(status)==0 and max(errors)<1e-10,(mask,status,errors)
    rows.append(dict(held_mask=int(mask),lambda_=values[0],pi=values[1],loglik=values[2],scaled_errors=errors))
print(json.dumps(dict(tolerance=1e-10,rows=rows),indent=2))
