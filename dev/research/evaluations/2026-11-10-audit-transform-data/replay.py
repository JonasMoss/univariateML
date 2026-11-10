#!/usr/bin/env python3
"""Replay frozen cases with the audit harness, retaining native CSV and guards.
Run from repository root: python3 <this-file> before|after.
First restore/build the archived harness as described in the report.
"""
import gzip,json,pathlib,subprocess,sys,time
ROOT=pathlib.Path(__file__).resolve().parents[4]
HERE=pathlib.Path(__file__).resolve().parent
OLD=HERE.parent/'2026-11-09-fit-audit-rerun-data'
CASES=json.loads(gzip.decompress((HERE.parent/'2026-11-07-fit-audit-data/cases.json.gz').read_bytes()))
TARGETS=[r['index'] for r in json.loads((HERE/'targets.json').read_text())]
def line(c):return f"{c['family']} {c['mask']} {c['index']} {len(c['data'])} "+' '.join(map(str,c['data']))+'\n'
def main():
 phase=sys.argv[1]; work=ROOT/'build/scratch/fit-audit'; rows=[]
 # The original reproducer is used for every requested case, with only ROOT
 # relocated to accommodate its restored scratch directory.
 with (HERE/f'{phase}-reproduce.log').open('w') as out:
  for index in TARGETS:
   subprocess.run([sys.executable,str(work/'reproduce.py'),str(index)],stdout=out,stderr=out,check=True,timeout=120)
 with (HERE/f'{phase}.csv').open('w') as out:
  for c in CASES:
   if c['family'] not in {'invgamma','lgamma','naka','lnorm','logitnorm','invburr','llogis','invweibull'}:continue
   try:
    p=subprocess.run([str(work/'stress')],input=line(c),text=True,capture_output=True,timeout=20)
    p.check_returncode(); result=p.stdout.strip()
   except subprocess.TimeoutExpired:result=f"{c['family']},{c['mask']},{c['index']},-997,0,0,0,0,nan"
   out.write(result+'\n');out.flush(); rows.append(result)
 print(phase, len(rows),'cases; native stops/time guards retained',flush=True)
if __name__=='__main__':main()
