"""Both archived families, original / pre-review / final, in either arithmetic mode.
Invoke under run. Each fit runs in the foreground with a 120-second external guard.
The pre-review state is reconstructed from the original Git commit plus its delta.
"""
import argparse,difflib,gzip,hashlib,json,os,re,subprocess,time
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4];HERE=Path(__file__).resolve().parent
BASE='bb46078757dad3538bfb263086d4e72cde163330'
os.environ.update(TMPDIR=str(ROOT/'build/tmp'),JUST_TEMPDIR=str(ROOT/'build/tmp'),XDG_RUNTIME_DIR=str(ROOT/'build/tmp'))
ap=argparse.ArgumentParser();ap.add_argument('--version',choices=['original','pre-review','final'],required=True);ap.add_argument('--double',action='store_true');ap.add_argument('--timeout',type=float,default=120);ap.add_argument('--case',type=int,action='append');args=ap.parse_args()
p=ROOT/'build/scratch/fpareto-review';p.mkdir(exist_ok=True)
flags=[];hashes={}
if args.version!='final':
 for family in ['gb2','fpareto']:
  rel=Path('univariateML-cpp/include/cppunivariate/distributions')/family/'mle.hpp'
  source=subprocess.check_output(['git','show',BASE+':'+str(rel)],text=True,cwd=ROOT)
  if args.version=='pre-review':
   # Before-review header copies are scratch only; the retained delta permits reconstruction.
   copy=p/(family+'-before.hpp')
   if copy.exists():source=copy.read_text()
   else:
    staging=p/'reconstruct';staging.mkdir(exist_ok=True)
    for f in ['gb2','fpareto']:
     r=Path('univariateML-cpp/include/cppunivariate/distributions')/f/'mle.hpp';dest=staging/r;dest.parent.mkdir(parents=True,exist_ok=True);dest.write_bytes(subprocess.check_output(['git','show',BASE+':'+str(r)],cwd=ROOT))
    subprocess.run(['patch','-p1','-i',str(HERE/'review-before.patch')],cwd=staging,check=True,stdout=subprocess.DEVNULL)
    source=(staging/rel).read_text()
  hashes[str(rel)]=hashlib.sha256(source.encode()).hexdigest()
  def inc(m):
   target=(ROOT/rel.parent/m[1]).resolve()
   if m[1]=='../gb2/mle.hpp':target=p/(args.version+'-gb2.hpp')
   return '#include "'+str(target)+'"'
  (p/(args.version+'-'+family+'.hpp')).write_text(re.sub(r'#include "([^"]+)"',inc,source))
 flags+=['-DFPAR_HEADER="'+str(p/(args.version+'-fpareto.hpp'))+'"']
 if args.version=='original':flags+=['-DUMVL_BASELINE']
else:
 for f in ['gb2','fpareto']:
  rel=Path('univariateML-cpp/include/cppunivariate/distributions')/f/'mle.hpp';hashes[str(rel)]=hashlib.sha256((ROOT/rel).read_bytes()).hexdigest()
if args.double:flags+=['-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS']
mode='double' if args.double else 'native';tag=args.version+'-'+mode
exe=p/('replay-'+tag)
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True)
subprocess.run(['ccache','g++','-std=c++17','-O2',*flags,'-I'+str(ROOT/'univariateML-cpp/include'),'-I'+boost,str(HERE/'review_driver.cpp'),'-o',str(exe)],check=True)
output=HERE/('review-'+tag+('.spot' if args.case else '')+'.jsonl')
meta=dict(baseline=BASE,version=args.version,mode=mode,source_hashes=hashes,timeout=args.timeout,controls=dict(fpareto=[1e-7,1000],gb2=[1e-10,300]))
output.with_suffix('.meta.json').write_text(json.dumps(meta,indent=2)+'\n')
done={r['index'] for r in map(json.loads,output.read_text().splitlines())} if output.exists() else set()
cases=[c for c in json.load(gzip.open(ROOT/'dev/research/evaluations/2026-11-07-fit-audit-data/cases.json.gz')) if c['family'] in ['gb2','fpareto'] and (not args.case or c['index'] in args.case)]
with output.open('a') as out:
 for c in cases:
  if c['index'] in done:continue
  h=c['held'];line=' '.join(map(str,[c['family'],c['index'],len(c['data']),*[h.get(k,'nan') for k in ['min','shape1','shape2','shape3','scale']],*c['data']]))
  t=time.monotonic()
  try:
   r=subprocess.run([str(exe)],input=line,text=True,capture_output=True,timeout=args.timeout,check=True)
   record=dict(index=c['index'],family=c['family'],sample=c['sample'],seconds=time.monotonic()-t,result=r.stdout.strip())
  except subprocess.TimeoutExpired:record=dict(index=c['index'],family=c['family'],sample=c['sample'],seconds=time.monotonic()-t,timeout=True)
  out.write(json.dumps(record)+'\n');out.flush()
print(tag,'complete',flush=True)
