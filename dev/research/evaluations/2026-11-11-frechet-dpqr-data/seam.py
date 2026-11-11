"""Re-run TASK-174's stored seam cases and compare to exact-input mpmath.
Run measure.py first (reconstructs baseline headers), then run this under a CPU slot.
"""
import csv, io, json, pathlib, re, subprocess
import mpmath as mp
mp.mp.dps=100
ROOT=pathlib.Path(__file__).resolve().parents[4]
OUT=pathlib.Path(__file__).resolve().parent
SCRATCH=ROOT/'build/scratch/frechet-measure'
source=(ROOT/'dev/research/evaluations/2026-11-10-gev-held2-data/combination-density-check.cpp').read_text()
inc=ROOT/'univariateML-cpp/include'
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip()
results={}
for version in ('baseline','current'):
 s=source if version=='current' else source.replace('"cppunivariate/distributions/frechet/dpqr.hpp"',f'"{SCRATCH}/frechet.hpp"')
 path=SCRATCH/f'seam-{version}.cpp';path.write_text(s)
 subprocess.run(['g++','-O2','-std=c++17','-I'+str(inc),'-I'+boost,str(path),'-o',str(SCRATCH/f'seam-{version}')],check=True)
 result=subprocess.check_output([str(SCRATCH/f'seam-{version}')],text=True)
 results[version]=list(csv.DictReader(io.StringIO(result)))
references=[]
for xs,args in re.findall(r'std::vector<double> x\{([^}]+)\}.*?dfrechet\(v,([^;]+),true\)',source):
 x=[mp.mpf(float(v)) for v in xs.split(',')]
 a,m,s=[mp.mpf(float(v)) for v in args.split(',')]
 terms=[mp.log(a/s)-(a+1)*mp.log((v-m)/s)-((v-m)/s)**(-a) for v in x]
 references.append(mp.fsum(terms))
assert len(references)==4
records=[]
for i,ref in enumerate(references):
 r={'case':i+4,'reference_loglik':mp.nstr(ref,40)}
 for version,rows in results.items():
  for key in ('density_loglik','fitter_loglik'):
   v=float(rows[i][key]);r[f'{version}_{key}']=v;r[f'{version}_{key}_absolute_error']=float(abs(mp.mpf(v)-ref))
 records.append(r)
(OUT/'seam.json').write_text(json.dumps(records,indent=2)+'\n')
print(json.dumps(records,indent=2))
