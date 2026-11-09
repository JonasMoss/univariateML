import pathlib,json
p=pathlib.Path(__file__).resolve().parent
rows=[]
for name in ['baseline-seeded.txt','final-seeded.txt']:
 parsed=[]
 for line in (p/name).read_text().splitlines():
  if line.startswith('cases'):continue
  a=line.split();parsed.append((tuple(a[:3]),list(map(float,a[10:13]))))
 rows.append(parsed)
assert len(rows[0])==len(rows[1])==189
maximum=0;witness=None;zero=0
for before,after in zip(*rows):
 assert before[0]==after[0]
 for parameter,(b,a) in enumerate(zip(before[1],after[1])):
  if b==0:
   assert a==0
   zero+=1
   continue
  error=abs(a-b)/abs(b)
  assert error<=1e-10,(before,after,parameter,error)
  if error>maximum:maximum=error;witness={'generating_shape_n_rep':before[0],'parameter':['mu','sigma','xi'][parameter],'before':b,'after':a}
result={'cases':len(rows[0]),'maximum_relative_coefficient_drift':maximum,'witness':witness,'zero_coefficients_preserved':zero,'passed':True}
(p/'coefficient-drift.json').write_text(json.dumps(result,indent=2)+'\n')
print(json.dumps(result,indent=2))
