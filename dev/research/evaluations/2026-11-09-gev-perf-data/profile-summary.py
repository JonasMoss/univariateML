import json,pathlib
p=pathlib.Path(__file__).resolve().parent
result={}
for stage in ['baseline','final']:
 native=[json.loads(l) for l in (p/f'{stage}.jsonl').read_text().splitlines()]
 instrumented=[json.loads(l) for l in (p/f'{stage}-profile.jsonl').read_text().splitlines()]
 for n,i in zip(native,instrumented):
  assert n['status']==i['status']==0 and n['budget']==i['budget']==False
  assert n['evaluations_or_iterations']==i['evaluations_or_iterations']
  for x,y in zip(n['coef'],i['coef']):assert abs(x-y)<=1e-10*max(abs(x),1e-5)
 rows=[]
 for line in (p/f'{stage}-profile.txt').read_text().splitlines():
  a=line.split()
  if a[0]=='case':
   rows.append({'case':native[int(a[1])]['case'],'n':native[int(a[1])]['n'],'scopes':{},'depth_counts':{}})
  elif a[0]=='depth':rows[-1]['depth_counts'][a[1]]=int(a[2])
  else:rows[-1]['scopes'][a[0]]={'calls':int(a[1]),'inclusive_seconds':float(a[2])}
 result[stage]=rows
(p/'profile-summary.json').write_text(json.dumps(result,indent=2)+'\n')
print('Instrumented/native status, coefficient and ledger agreement passed for eight fits')
