#!/usr/bin/env python3
"""Compare protected density kernels to raw high-precision likelihoods."""
import csv,json,pathlib,subprocess
HERE=pathlib.Path(__file__).resolve().parent;ROOT=HERE.parents[3]
refs={r['index']:r for r in json.loads((HERE/'reference.json').read_text())}
fits={int(r[2]):r for r in csv.reader((HERE/'after.csv').read_text().splitlines())}
checks={r['index']:r for r in json.loads((HERE/'comparison.json').read_text())['cases']}
lines=[]
for i,r in refs.items():
 p=dict(v.split('=') for v in fits[i][9:]);v=[p['shape1'],p['shape2'],p.get('scale','1'),p.get('min','0'),p.get('max','1')]
 lines.append(f"{i} {r['family']} {len(r['data'])} "+' '.join(v+list(map(str,r['data']))))
out=subprocess.check_output([str(ROOT/'build/scratch/density-check')],input='\n'.join(lines)+'\n',text=True)
with (HERE/'density-seams.csv').open('w') as dest:
 writer=csv.writer(dest);writer.writerow(['index','family','fit_logLik','raw_mp_logLik_at_returned','dpqr_log_density_sum'])
 for i,family,value in csv.reader(out.splitlines()):
  i=int(i);row=[i,family,fits[i][8],checks[i]['after']['raw_logLik'],value];writer.writerow(row);print(','.join(map(str,row)))
