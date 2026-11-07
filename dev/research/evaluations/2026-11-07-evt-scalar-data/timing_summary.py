from pathlib import Path
import statistics,json
p=Path(__file__).parent
rows=[]
for n in [7,500]:
 for mode in range(5):
  row=dict(n=n,mode=mode)
  for side in ['before','after']:
   vals=[]
   for i in range(1,6):
    ls=(p/f'{side}-{i}.txt').read_text().splitlines()
    for j,s in enumerate(ls):
     if s.startswith(f'n {n} mode {mode} '):vals.append(float(ls[j+1].split()[1]))
   row[side+'_microseconds_median']=statistics.median(vals)
   row[side+'_microseconds_range']=[min(vals),max(vals)]
  row['ratio']=row['after_microseconds_median']/row['before_microseconds_median'];rows.append(row)
# Default fits must remain bit-identical, including the evaluated likelihood.
for n in [7,500]:
 for mode in [0,4]:
  before=next(s for s in (p/'before.txt').read_text().splitlines() if s.startswith(f'n {n} mode {mode} '))
  after=next(s for s in (p/'after.txt').read_text().splitlines() if s.startswith(f'n {n} mode {mode} '))
  assert before==after,(before,after)
(p/'timing-summary.json').write_text(json.dumps(dict(default_identity='bit-identical',rows=rows),indent=2)+'\n')
print('PASS: paired timing summary; both default routes bit-identical at n=7 and n=500.')
