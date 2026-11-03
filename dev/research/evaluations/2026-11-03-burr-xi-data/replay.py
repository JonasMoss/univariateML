"""Replay the unchanged C++ fitter; this does not validate its certification."""
import csv
import json
from pathlib import Path
import subprocess

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]


def main():
    samples = json.loads((HERE/'samples.json').read_text())
    payload = ''.join(str(len(x))+' '+' '.join(format(v,'.17g') for v in x)+'\n'
                      for x in samples.values())
    proc = subprocess.run([str(ROOT/'build/scratch/burr-xi-probe')],input=payload,
                          text=True,capture_output=True,check=True)
    lines = proc.stdout.splitlines()
    assert len(lines)==len(samples)
    rows=[]
    for name,line in zip(samples,lines):
        status,k,c,s,ll = line.split()
        rows.append(dict(sample=name,status=int(status),k=float(k),c=float(c),
                         scale=float(s),logLik=float(ll)))
    with (HERE/'existing-fitter.csv').open('w') as f:
        writer=csv.DictWriter(f,fieldnames=list(rows[0])); writer.writeheader(); writer.writerows(rows)
    print('Unchanged C++ fitter replayed on',len(rows),'samples.')
    print('Statuses:',{status:sum(r['status']==status for r in rows) for status in sorted({r['status'] for r in rows})})


if __name__ == '__main__':
    main()
