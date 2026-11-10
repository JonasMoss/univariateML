#!/usr/bin/env python3
import csv,json,pathlib,subprocess
from reference import CASES,NAMES
from compare import read
HERE=pathlib.Path(__file__).resolve().parent;ROOT=HERE.parents[3]
rows=read('after');lines=[];ids=[]
for ref in json.loads((HERE/'reference.json').read_text()):
 i=ref['index'];r=rows[i]
 if r['status']!=0:continue
 c=CASES[i];coef=[r['theta'][k] for k in NAMES[c['family']]]
 if len(coef)==2:coef+=['1']
 lines.append(f"{c['family']} {i} {len(c['data'])} "+' '.join(coef+list(map(str,c['data'])))+'\n');ids.append(i)
p=subprocess.run([str(ROOT/'build/scratch/density-check')],input=''.join(lines),text=True,capture_output=True,check=True)
with (HERE/'density-seams.csv').open('w') as f:
 f.write('family,index,fitted_logLik,density_logLik\n')
 for row in csv.reader(p.stdout.splitlines()):f.write(','.join(row[:2]+[rows[int(row[1])]['logLik'],row[2]])+'\n')
