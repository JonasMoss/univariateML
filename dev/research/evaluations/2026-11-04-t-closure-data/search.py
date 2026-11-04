import sys, json, warnings
from pathlib import Path
sys.path.insert(0,str(Path('dev/research/evaluations/2026-11-04-t-closure-data').resolve()))
from research import *
warnings.filterwarnings('ignore')
rng=np.random.default_rng(140)
found={}; records=[]
for trial in range(1500):
    m=int(rng.integers(3,8));x=np.sort(rng.uniform(-1,1,m));x=np.sign(x)*np.abs(x)**rng.uniform(.3,4)
    if trial%3==0:x=np.concatenate([rng.normal(0,.02,3),rng.uniform(1,3,m)])
    w=rng.integers(1,12,len(x)).astype(float)
    if max(w)>=sum(w)/2:continue
    mu=np.dot(w,x)/sum(w);scale=np.sqrt(np.dot(w,(x-mu)**2)/sum(w));x=(x-mu)/scale
    try:
      grid=np.geomspace(1,500,65);ps=profile(x,w,grid);norm=moments(x,w)['normal']
    except (RuntimeError,ValueError,OverflowError):continue
    roots=[]
    for l,r in zip(ps,ps[1:]):
      if l['dnu']*r['dnu']<0:
        v=brentq(lambda v:conditional(x,w,v)['dnu'],l['nu'],r['nu'],xtol=1e-9)
        p=conditional(x,w,v);p['kind']='max' if l['dnu']>0 else 'min';roots.append(p)
    maxima=[p for p in roots if p['kind']=='max']
    key=None
    if len(maxima)>=2:key='two-interior-maxima'
    elif maxima and ps[0]['dnu']<0:key='cauchy-stable-interior'
    elif any(p['nu']>2 and p['ll']>max(conditional(x,w,2)['ll'],norm)+1e-5 for p in maxima) and conditional(x,w,2)['dnu']<0:key='t2-stable-finite-winner'
    if key and key not in found:
      rec={'trial':trial,'x':x.tolist(),'weights':w.tolist(),'moments':moments(x,w),'roots':roots,'lower':ps[0],'two':conditional(x,w,2),'profiles':ps}
      found[key]=rec
      print(key,trial,flush=True)
      Path('build/scratch/search-found.json').write_text(json.dumps(found,indent=2))
    if trial%100==0:print('trial',trial,flush=True)
    if len(found)==3:break
print('DONE',list(found),flush=True)
