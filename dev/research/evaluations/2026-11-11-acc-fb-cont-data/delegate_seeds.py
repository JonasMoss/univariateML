"""Append frozen parent-transport seeds; does not regenerate existing inputs."""
from pathlib import Path
import json,copy
P=Path(__file__).resolve().parent;ROOT=P.parents[3]
cs=json.loads((P/'cases.json').read_text());by={str(c['index']):c for c in cs};new=[]
golden=json.loads((ROOT/'univariateML-cpp/include/cppunivariate/distributions/burr/golden.json').read_text())['data']
def add(name,family,data,held,it=1000):
 key='strict-seed-'+name
 if key in by:return
 c=dict(index=key,family=family,data=data,held=held,tol=1e-10,iterlim=it,source='parent-transport branch seed')
 cs.append(c);new.append(key);by[key]=c
for family in ['gb2','fpareto']:
 for inverse in [False,True]:
  data=[1/v if inverse else v for v in golden]
  held={'shape1' if inverse else 'shape3':1}
  if family=='fpareto':held['min']=0
  add(family+('-inverse-unit' if inverse else '-unit-joint'),family,data,held,16)
 h={'shape2':1,'shape3':1}
 if family=='fpareto':h['min']=0
 add(family+'-unit-power',family,[1,1,1,1,6.0001],h)
 c=by['route-conditional-budget64'];data=[v*v for v in c['data']];held={'shape2':.5,'shape3':c['held']['shape1']}
 if family=='fpareto':data=[v-1 for v in data];held['min']=-1
 add(family+'-small-power',family,data,held,64)
 # Reciprocal axis exercises the opposite held beta shape.
 c=by['delegate-rounded-power-gb2'];data=[1/v for v in c['data']];held={'shape2':c['held']['shape2'],'shape1':c['held']['shape3']}
 if family=='fpareto':held['min']=0
 add(family+'-reverse-power',family,data,held)
(P/'cases.json').write_text(json.dumps(cs,indent=2)+'\n')
(P/'delegate-seed-ids.json').write_text(json.dumps([str(c['index']) for c in cs if str(c['index']).startswith(('strict-seed-gb2','strict-seed-fpareto'))],indent=2)+'\n')
spots=json.loads((P/'spot-cases.json').read_text());ids={str(c['index']) for c in spots}
for key in ['strict-seed-gb2-small-power','strict-seed-fpareto-small-power','strict-seed-gb2-inverse-unit']:
 if key in ids:continue
 c=copy.deepcopy(by[key]);c['control']={'reltol' if c['family']=='fpareto' else 'gradtol':c['tol'],'iterlim':c['iterlim']};spots.append(c)
(P/'spot-cases.json').write_text(json.dumps(spots,indent=2)+'\n')
print('added',new,'total',len(cs))
