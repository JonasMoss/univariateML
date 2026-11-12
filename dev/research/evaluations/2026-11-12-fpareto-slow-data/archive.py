"""Direct archive replay at binding-default controls (optional stricter tol).
Compile/run through one slot wrapper; output is flushed per case and resumable.
"""
import argparse,json,os,re,subprocess,time
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4];HERE=Path(__file__).resolve().parent
os.environ.update(TMPDIR=str(ROOT/'build/tmp'),JUST_TEMPDIR=str(ROOT/'build/tmp'),XDG_RUNTIME_DIR=str(ROOT/'build/tmp'))
BASE='bb46078757dad3538bfb263086d4e72cde163330'
a=argparse.ArgumentParser();a.add_argument('--baseline',action='store_true');a.add_argument('--double',action='store_true');a.add_argument('--tol',default='1e-7');a.add_argument('--timeout',type=float,default=120);a.add_argument('--output',required=True);args=a.parse_args()
scratch=ROOT/'build/scratch/fpareto-archive';scratch.mkdir(exist_ok=True)
flags=[]
if args.baseline:
 for family in ['gb2','fpareto']:
  path=Path('univariateML-cpp/include/cppunivariate/distributions')/family/'mle.hpp'
  source=subprocess.check_output(['git','show',f'{BASE}:{path}'],text=True,cwd=ROOT)
  def inc(m):
   target=(ROOT/path.parent/m[1]).resolve()
   if m[1]=='../gb2/mle.hpp':target=scratch/'baseline-gb2.hpp'
   return '#include "'+str(target)+'"'
  (scratch/f'baseline-{family}.hpp').write_text(re.sub(r'#include "([^"]+)"',inc,source))
 flags+=['-DUMVL_BASELINE','-DFPAR_HEADER="'+str(scratch/'baseline-fpareto.hpp')+'"']
if args.double:flags+=['-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS']
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True)
exe=scratch/('baseline' if args.baseline else 'current');exe=exe.with_name(exe.name+('-double' if args.double else '-native'))
subprocess.run(['ccache','g++','-std=c++17','-O2',*flags,'-I'+str(ROOT/'univariateML-cpp/include'),'-I'+boost,str(HERE/'driver.cpp'),'-o',str(exe)],check=True)
out=Path(args.output);done={r['index'] for r in map(json.loads,out.read_text().splitlines())} if out.exists() else set()
with out.open('a') as f:
 for c in json.loads((HERE/'cases.json').read_text()):
  if c['index'] in done:continue
  h=c['held'];line=' '.join(map(str,[c['index'],len(c['data']),*[h.get(k,'nan') for k in ['min','shape1','shape2','shape3','scale']],*c['data']]))
  start=time.monotonic()
  try:
   p=subprocess.run([str(exe),args.tol],input=line,text=True,capture_output=True,timeout=args.timeout,check=True)
   r=dict(index=c['index'],seconds=time.monotonic()-start,result=p.stdout.strip())
  except subprocess.TimeoutExpired:r=dict(index=c['index'],seconds=time.monotonic()-start,timeout=True)
  f.write(json.dumps(r)+'\n');f.flush()
