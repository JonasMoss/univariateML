"""Resolve all-free timing variability on one fixed CPU; run under one slot.
Requires reproduce.py's instrumented overlays. Does not change fit headers.
"""
from pathlib import Path
import os,subprocess,csv,json,statistics
p=Path(__file__).resolve().parent;scratch=Path('build/scratch/emg-perf')
core=min(os.sched_getaffinity(0));os.sched_setaffinity(0,{core})
print('Pinned CPU',core,flush=True)
for v in ['before','after']:
    subprocess.run(['c++','-std=c++17','-O2','-DEMG_PERF_PROFILE',f'-I{scratch}/{v}-instrumented','-IunivariateML-cpp/tests',str(p/'benchmark.cpp'),'-o',str(scratch/f'focused-{v}')],check=True)
results=[]
for n in [500,10000]:
    for rep in range(3):
        for v in (['before','after'] if rep%2==0 else ['after','before']):
            print(n,rep,v,flush=True)
            out=subprocess.run([str(scratch/f'focused-{v}'),'1e-8','0',str(n)],text=True,capture_output=True,check=True)
            row=list(csv.DictReader(out.stdout.splitlines()))[0]
            row.update(version=v,repetition=rep,core=core);results.append(row)
            assert row['status']=='0',row
            (p/'focused-runs.json').write_text(json.dumps(results,indent=2)+'\n')
summary=[]
for n in [500,10000]:
    item=dict(n=n,core=core)
    for v in ['before','after']:
        rows=[r for r in results if int(r['n'])==n and r['version']==v]
        item[v+'_cpu_seconds']=statistics.median(float(r['cpu_seconds']) for r in rows)
        item[v+'_wall_seconds']=statistics.median(float(r['seconds']) for r in rows)
    item['cpu_speedup']=item['before_cpu_seconds']/item['after_cpu_seconds'];summary.append(item)
(p/'focused-summary.json').write_text(json.dumps(summary,indent=2)+'\n')
print(json.dumps(summary,indent=2),flush=True)
