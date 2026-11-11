#!/usr/bin/env python3
"""Replay eight delegate families: frozen audit cases, seed 16802 GED cases,
and explicit dependency probes plus three inherited held-power regressions.
Run `python3 <this file> before|after` under the CPU wrapper.
"""
from pathlib import Path
import sys,subprocess,tarfile,json,collections,os
HERE=Path(__file__).resolve().parent
ROOT=HERE.parents[3]
mode=sys.argv[1];assert mode in ['before','after']
work=ROOT/'build/scratch/fallback-delegate-study';work.mkdir(parents=True,exist_ok=True)
if mode=='before':
 with tarfile.open(HERE/'delegate-before-headers.tar.gz') as archive:archive.extractall(work/'before',filter='data')
 inc=work/'before/include';binding=work/'before'
else:inc=ROOT/'univariateML-cpp/include';binding=ROOT/'bindings'
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip()
binary=work/(mode+"-fit")
subprocess.run(['g++','-O2','-std=c++17',f'-I{inc}',f'-I{binding}',f'-I{boost}',str(HERE/'delegate_study.cpp'),'-o',str(binary)],check=True)
cases=json.loads((HERE/'delegate-cases.json').read_text());lines=[]
for c in cases:
 held={k:v for k,v in c['held'].items() if v is not None}
 values=[c['family'],str(c['index']),str(len(c['data'])),str(len(held)),str(c.get('iterlim',1000)),str(c.get('tol',1e-8))]
 values+=list(map(repr,c['data']))
 for k,v in held.items():values += [k,repr(v)]
 lines.append(' '.join(values))
with (HERE/f'delegate-{mode}.tsv').open('w') as output:
 subprocess.run([str(binary)],input='\n'.join(lines)+'\n',text=True,stdout=output,check=True)
counts=collections.defaultdict(collections.Counter)
for line in (HERE/f'delegate-{mode}.tsv').read_text().splitlines():
 fields=line.split('\t');counts[fields[1]][fields[2]]+=1
(HERE/f'delegate-{mode}-counts.json').write_text(json.dumps(counts,indent=2)+'\n')
print(json.dumps(counts,indent=2),flush=True)
