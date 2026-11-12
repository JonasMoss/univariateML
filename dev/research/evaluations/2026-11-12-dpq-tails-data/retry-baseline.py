"""Retry baseline replay guards using the preserved, pre-edit binary."""
import gzip,importlib.util,io,json,subprocess,sys,tarfile
from pathlib import Path
here=Path(__file__).resolve().parent;root=here.parents[3]
rows=json.loads(gzip.decompress((root/'dev/research/evaluations/2026-11-07-fit-audit-data/cases.json.gz').read_bytes()))
exe=root/'build/scratch/seam-check/probe-53f7912622be-da874a8d5291';out=[]
if not exe.exists():
 sys.path.insert(0,str(root/'dev/validation'))
 spec=importlib.util.spec_from_file_location('seam',root/'dev/validation/seam-check.py');seam=importlib.util.module_from_spec(spec);spec.loader.exec_module(seam)
 generated=seam.build(['bbinom','beta','betapr','dgamma','fpareto','gb2','lgamma','lnorm','pareto4'],source_id='baseline-reproduction')
 checkout=root/'build/scratch/dpq-tails-baseline';checkout.mkdir(parents=True,exist_ok=True)
 raw=subprocess.check_output(['git','archive','217ab8b5','univariateML-cpp/include'],cwd=root)
 with tarfile.open(fileobj=io.BytesIO(raw)) as archive:archive.extractall(checkout,filter='data')
 boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip()
 subprocess.run(['ccache','c++','-std=c++17','-O1','-I'+str(root),'-I'+str(checkout/'univariateML-cpp/include'),'-I'+boost,str(generated.with_suffix('.cpp')),'-o',str(exe)],check=True)
for index in [1750,10757]:
 r=next(c for c in rows if c['index']==index);names=['shape1','shape2','shape3','scale','min'];holds=[r['held'].get(n,0) for n in names];mask=sum(1<<i for i,n in enumerate(names) if n in r['held'])
 line=' '.join(map(str,['fpareto',mask,5,len(r['data']),*holds,*r['data']]))+'\n'
 try:
  result=subprocess.run([str(exe)],input=line,text=True,capture_output=True,timeout=300,check=True)
  out.append(dict(index=index,stdout=result.stdout))
 except subprocess.TimeoutExpired:out.append(dict(index=index,open='no terminal baseline outcome within 300 seconds'))
 (here/'baseline-retries.json').write_text(json.dumps(out,indent=2)+'\n');print(out[-1],flush=True)
