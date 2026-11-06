"""Summarize the retained paired n=500 timing runs (run from worktree root)."""
from pathlib import Path
import csv
import statistics

root = Path(__file__).resolve().parent
names = {'tlambda': ['lambda','mu','sigma'], 'emg': ['mu','sigma','lambda'],
         'bhatt': ['sigma','a','mu'], 'std': ['mean','sd','nu'],
         'lst': ['mu','sigma','df'], 'sstd': ['mean','sd','nu','xi'],
         'sged': ['mean','sd','nu','xi'], 'slash': ['sigma','mu'],
         'fpareto': ['min','shape1','shape2','shape3','scale'],
         'gb2': ['shape1','shape2','shape3','scale']}

def load(pattern):
    result = {}
    for rep in range(1,4):
        for line in (root / pattern.format(rep=rep)).read_text().splitlines():
            row = line.split()
            result.setdefault((row[0],int(row[2])),[]).append(row)
    return result

def held(family,mask):
    return ', '.join(n for bit,n in enumerate(names[family]) if mask & (1<<bit)) or 'none'

def stats(values):
    return statistics.median(values),min(values),max(values)

baseline = load('before-timings-{rep}.txt')
with (root/'baseline-timings.csv').open('w') as stream:
    writer = csv.writer(stream)
    writer.writerow(['family','mask','held','median_wall_s','min_wall_s','max_wall_s','status'])
    for (family,mask),rows in baseline.items():
        assert len(rows)==3 and all(r[:-1]==rows[0][:-1] for r in rows)
        writer.writerow([family,mask,held(family,mask),*stats([float(r[-1]) for r in rows]),rows[0][3]])

cpu = {}
for line in (root/'cpu-timings.txt').read_text().splitlines():
    family,mask,binary,rep,user,system=line.split()
    cpu.setdefault((family,int(mask),binary),[]).append(float(user)+float(system))
before,after=load('final-before-timings-{rep}.txt'),load('final-after-timings-{rep}.txt')
with (root/'final-timings.csv').open('w') as stream:
    writer=csv.writer(stream)
    writer.writerow(['family','mask','held','before_wall_s','after_wall_s','before_wall_min_s',
                     'before_wall_max_s','after_wall_min_s','after_wall_max_s',
                     'before_cpu_s','after_cpu_s','status','identical_outputs'])
    for (family,mask),rows in before.items():
        new=after[(family,mask)]
        assert len(rows)==len(new)==3 and all(r[:-1]==rows[0][:-1] for r in rows+new)
        ta,tb=stats([float(r[-1]) for r in rows]),stats([float(r[-1]) for r in new])
        ca,cb=cpu[(family,mask,'before')],cpu[(family,mask,'after')]
        assert len(ca)==len(cb)==3
        writer.writerow([family,mask,held(family,mask),ta[0],tb[0],ta[1],ta[2],tb[1],tb[2],
                         statistics.median(ca),statistics.median(cb),rows[0][3],True])
print(f'{len(baseline)} baseline subsets; {len(before)} final paired subsets; all outputs identical.')
