#!/usr/bin/env python3
"""Run via simbox-run (or locally under run4), from repository root.
All-free pairs use two single-thread workers, three repetitions at n=500 and
one at n=10000. Held subsets run sequentially in alternating variant order,
three repetitions at each n, preventing overlap with a costly all-free fit.
CSV adds variant and repeat columns to benchmark.py's documented columns.
Compilation/data preparation are outside timed fits. Completed fits are
checkpointed in build/scratch/slash-perf and re-emitted on resume. No installs.
"""
import concurrent.futures
import hashlib
import pathlib
import subprocess
import sys

source=pathlib.Path(__file__).parent
scratch=pathlib.Path('build/scratch/slash-perf')
print(subprocess.check_output(['g++','--version'],text=True).splitlines()[0],file=sys.stderr,flush=True)
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True)
for variant in ('baseline','after'):
    subprocess.run([sys.executable,str(source/'benchmark.py'),variant,'--depth'],check=True,stdout=subprocess.DEVNULL)
    subprocess.run(['g++','-std=c++17','-O2','-IunivariateML-cpp/include','-I'+boost,str(scratch/(variant+'-depth.cpp')),'-o',str(scratch/(variant+'-depth'))],check=True)

def run(variant,sample,repeat,mask):
    header=(scratch/(variant+'-depth.hpp')).read_bytes()
    data=(scratch/(sample+'.txt')).read_bytes()
    protocol=b'' if mask==0 else b'held-serial-v1'
    key=hashlib.sha256(header+data+(scratch/(variant+'-depth.cpp')).read_bytes()+protocol).hexdigest()[:16]
    checkpoint=scratch/'checkpoints'
    checkpoint.mkdir(exist_ok=True)
    saved=checkpoint/f'{variant}-{sample}-{repeat}-{mask}-{key}.csv'
    print(f'start {variant} {sample} repeat={repeat} mask={mask}',file=sys.stderr,flush=True)
    if saved.exists():
        line=saved.read_text().strip()
    else:
        command=[str(scratch/(variant+'-depth')),str(scratch/(sample+'.txt')),str(mask)]
        result=subprocess.run(command,text=True,stdout=subprocess.PIPE,check=True)
        lines=result.stdout.splitlines()
        if len(lines)!=1:
            raise RuntimeError(f'expected one fit: {command}')
        line=lines[0]
        temporary=saved.with_suffix('.tmp')
        temporary.write_text(line+'\n')
        temporary.replace(saved)
    print(f'{variant},{repeat},{line}',flush=True)
    print(f'finish {variant} {sample} repeat={repeat} mask={mask}',file=sys.stderr,flush=True)

samples=sys.argv[1:] or ['golden500','cauchy500','golden10000','cauchy10000']
with concurrent.futures.ThreadPoolExecutor(max_workers=2) as pool:
    for sample in samples:
        for repeat in range(3 if sample.endswith('500') else 1):
            jobs=[pool.submit(run,variant,sample,repeat,0) for variant in ('baseline','after')]
            for job in jobs:
                job.result()
        for repeat in range(3):
            order=('baseline','after') if repeat%2==0 else ('after','baseline')
            for mask in (1,2,3):
                for variant in order:
                    run(variant,sample,repeat,mask)
