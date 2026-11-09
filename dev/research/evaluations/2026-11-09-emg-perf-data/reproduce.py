"""Run under /tmp/umvl-cpu/run; builds and computations are sequential.

Baseline is the task's clean base revision; all scratch stays in build/.
Three alternating pairs per tolerance (wall and process CPU clocks): the frozen 500-point sample and a
seed-162 independent 10000-point sample generated at its golden coefficients.
"""
from pathlib import Path
import subprocess, shutil, sys, json, csv, statistics
p=Path(__file__).resolve().parent
root=Path.cwd(); scratch=root/'build/scratch/emg-perf'
scratch.mkdir(parents=True,exist_ok=True)
source=root/'univariateML-cpp/include/cppunivariate/distributions/emg'
baseline=scratch/'baseline-reproduce'
shutil.copytree(source,baseline,dirs_exist_ok=True)
for name in ('certificate.hpp','certificate_math.hpp','shape_bounds.hpp'):
    (baseline/name).write_bytes(subprocess.check_output(['git','show',f'2ed5514b:univariateML-cpp/include/cppunivariate/distributions/emg/{name}']))
for version,folder in [('before',baseline),('after',source)]:
    overlay=scratch/f'{version}-instrumented'
    subprocess.run([sys.executable,str(p/'instrument.py'),str(folder),str(overlay)],check=True)
    subprocess.run(['c++','-std=c++17','-O2','-DEMG_PERF_PROFILE',f'-I{overlay}','-IunivariateML-cpp/tests',str(p/'benchmark.cpp'),'-o',str(scratch/f'{version}-instrumented-bin')],check=True)
for tol in ['1e-8']:
    for rep in range(3):
        versions=['before','after'] if rep%2==0 else ['after','before']
        for version in versions:
            print(f'{version}, tolerance={tol}, repetition={rep}',flush=True)
            with (p/f'{version}-{tol}-{rep}.csv').open('w') as out, (p/f'{version}-{tol}-{rep}-profile.csv').open('w') as err:
                err.write('n,mask,point_evaluations,hessian_evaluations,newton_calls,newton_steps,fallbacks,max_subdivision_depth\n');err.flush()
                subprocess.run([str(scratch/f'{version}-instrumented-bin'),tol],stdout=out,stderr=err,check=True)
rows=[]
for tol in ['1e-8']:
    for n in [500,10000]:
        for mask in range(7):
            r=dict(n=n,mask=mask,tol=tol)
            for version in ['before','after']:
                samples=[]
                for rep in range(3):
                    table=list(csv.DictReader((p/f'{version}-{tol}-{rep}.csv').open()))
                    row=next(q for q in table if int(q['n'])==n and int(q['mask'])==mask)
                    assert row['status']=='0',row
                    samples.append(float(row['seconds']))
                    for key in ['certificate_evaluations','profiles','winners']:
                        r[f'{version}_{key}']=int(row[key])
                r[f'{version}_seconds']=statistics.median(samples)
            r['speedup']=r['before_seconds']/r['after_seconds'];rows.append(r)
(p/'timing-summary.json').write_text(json.dumps(rows,indent=2)+'\n')
print('All alternating runs returned Ok; timing-summary.json written.',flush=True)
