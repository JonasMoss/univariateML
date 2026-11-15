"""Per entry: returned point, held values, 50-digit maximizer from the refinement cache, relative errors."""
import sys, json, gzip
sys.path.insert(0,'dev/validation')
from strict_common import load, number
E=sys.argv[0]
S='dev/research/evaluations/2026-11-15-fit-snapshot-data/'
snap={'double':{**load(S+'double.jsonl.gz'),**load(S+'routes-double.jsonl.gz')},
      'native':{**load(S+'native.jsonl.gz'),**load(S+'routes-native.jsonl.gz')}}
cache={}
for l in gzip.open('dev/validation/strict-refine.jsonl.gz','rt'):
    e=json.loads(l); cache.setdefault((e['family'],e['source'],e['case']),[]).append(e)
known=json.load(open('dev/validation/strict-known.json'))['known']
out=[]
for k,v in known.items():
    if v['ref']!='TASK-214.5' or not k.startswith('refine|'): continue
    _,mode,fam,src,case=k.split('|')
    r=snap[mode][(fam,src,case)]
    ests={n:number(x) for n,x in r['result']['estimates'].items()}
    held={n:x for n,x in r['held'].items()}
    e=cache[(fam,src,case)][0]   # one entry per case and held values; start is whichever mode ran first
    star=e['estimates']
    import mpmath as mp
    mp.mp.dps=50
    rel={n:float((mp.mpf(ests[n])-mp.mpf(star[n]))/(mp.mpf(star[n]) or 1)) for n in ests if n in star}
    out.append(dict(key=k,mode=mode,family=fam,case=case,held=held,est={n:repr(x) for n,x in ests.items()},star=star,rel=rel,
                    logLik_ret=r['result'].get('logLik'),logLik_star=e.get('logLik'),n=r.get('n'),
                    result={x:y for x,y in r['result'].items() if x!='estimates'}))
json.dump(out,open('dev/research/evaluations/2026-11-15-refine-causes-data/extract.json','w'),indent=1)
for o in out: print(o['family'],o['mode'],o['case'],o['held'],{n:'%.3g'%x for n,x in o['rel'].items()})
