"""Case 31101: retained 100-digit reference, native/double core and R/WASM."""
from pathlib import Path
import json,subprocess,mpmath as m
root=Path.cwd();p=root/'dev/research/evaluations/2026-11-11-acc-fb-cont-data';work=root/'build/scratch/review-round';m.mp.dps=100
r=json.loads((p/'review-ged-31101-reference.json').read_text());x=r['data'];sd=r['held']['sd'];truth={k:m.mpf(v) for k,v in r['reference'].items()};rows=[]
for mode in ['native','double']:
 line=' '.join(['ged','review-31101','3','1','1000','1e-10',*map(repr,x),'sd',repr(sd)])+'\n'
 out=subprocess.check_output([str(root/'build/scratch/acc-parallel'/('fit-'+mode))],input=line,text=True).strip().split('\t');assert out[2] in ['certified','uncertified']
 rows.append(dict(backend=mode,family='ged',estimates={a:float(b) for a,b in (z.split('=',1) for z in out[4:])}))
rcode='options(pkg.build_extra_flags=FALSE);pkgload::load_all("univariateML-r",quiet=TRUE);x<-c('+','.join(map(repr,x))+');for(id in c("ged","sged")){args<-list(x=x,sd='+repr(sd)+');if(id=="sged")args$xi<-1;f<-do.call(getExportedValue("univariateML",paste0("ml",id)),args);cat(jsonlite::toJSON(list(backend="R",family=id,estimates=lapply(as.list(f),function(v)sprintf("%.17g",v))),auto_unbox=TRUE,digits=NA),intToUtf8(10),sep="")} '
out=subprocess.check_output(['Rscript','-e',rcode],text=True)
rows += [json.loads(line) for line in out.splitlines() if line.startswith('{')]
js="import create from './bindings/js/index.mjs';const api=await create();const x="+json.dumps(x)+";for(const id of ['ged','sged']){const held={sd:"+repr(sd)+"};if(id==='sged')held.xi=1;const f=api[id].fit(x,held);console.log(JSON.stringify({backend:'WASM',family:id,estimates:f.estimates}));}"
out=subprocess.check_output(['node','--input-type=module','-e',js],text=True);rows += [json.loads(line) for line in out.splitlines() if line.startswith('{')]
assert len(rows)==6,len(rows)
for row in rows:
 got={k:float(v) for k,v in row['estimates'].items()};row['estimates']=got;assert got['sd']==sd,(row,sd)
 if row['family']=='sged':assert got['xi']==1
 row['errors']={k:float(abs(m.mpf(got[k])-v)/max(abs(v),truth['sd'] if k=='mean' else 0)) for k,v in truth.items() if k!='sd'};assert max(row['errors'].values())<=1e-10,row
print(json.dumps(rows,indent=2));(p/'review-ged-31101-results.json').write_text(json.dumps(rows,indent=2)+'\n')
