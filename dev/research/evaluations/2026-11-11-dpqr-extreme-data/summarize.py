"""Create a strict JSON index of measured failures and unfinished numerical work."""
import collections,json,math
from pathlib import Path
H=Path(__file__).resolve().parent
load=lambda name:json.loads((H/name).read_text())
def clean(x):
 if isinstance(x,float) and not math.isfinite(x):return 'NaN' if math.isnan(x) else 'Inf' if x>0 else '-Inf'
 if isinstance(x,dict):return {k:clean(v) for k,v in x.items()}
 if isinstance(x,(list,tuple)):return [clean(v) for v in x]
 return x
before=load('before-failures.json');after=load('after-failures.json');seam=load('seam-final.json');stats=load('stats-findings.json');coverage=load('coverage.json');sizes=load('final-sizes.json')
rank=[]
for family,count in collections.Counter(r['family'] for r in before).most_common():
 old=[r for r in before if r['family']==family];new=[r for r in after if r['family']==family]
 rank.append(dict(family=family,before=count,after=len(new),active_after=sum(not r.get('excluded_reason') for r in new),worst_before=max(r['error'] for r in old),worst_after=max([r['error'] for r in new]+[0.])))
opens=collections.defaultdict(list)
for r in seam['mismatches']:
 if r['attribution'] in ('dpqr','both'):opens[r['family']].append(r)
open_seam=[]
for family,rs in sorted(opens.items(),key=lambda p:-len(p[1])):
 witness=max(rs,key=lambda r:r['dpqr_error'])
 open_seam.append(dict(family=family,cases=len(rs),witness={k:witness[k] for k in ('index','parameters','dpqr','reference','dpqr_error')}))
fitter=collections.Counter(r['family'] for r in seam['mismatches'] if r['attribution'] in ('fitter','both','named precision-region NaN; fitter'))
notes=dict(baseline=sizes['baseline'],ranking=rank,before=load('before-summary.json'),after=load('after-summary.json'),after_double=load('after-double-summary.json'),
 sizes={k:dict(before=sum(r['before'][k] for r in sizes['families']),after=sum(r['after'][k] for r in sizes['families'])) for k in ('bytes','points','values','quantiles')},
 seam=dict(records=seam['records'],goldens=99,mismatches=len(seam['mismatches']),attributions=dict(collections.Counter(r['attribution'] for r in seam['mismatches'])),fitter_errors=dict(fitter.most_common()),dpqr_open=open_seam),
 stats=dict(checks=stats['checks'],failures=len(stats['failures']),by_function=dict(collections.Counter(r['function'] for r in stats['failures']).most_common())),
 oracle_gaps=[dict(family=r['family'],gaps=r['oracle_gaps']) for r in coverage if r['oracle_gaps']],
 fresh_exclusions=[r for r in after if r['family'] not in ('tbinom','tpois')],
 gap_count=sum(len(r['oracle_gaps']) for r in coverage),samplers={k:v for k,v in load('sampler-streams.json').items() if k!='changes'})
(H/'report-data.json').write_text(json.dumps(clean(notes),indent=2,allow_nan=False)+'\n')
print('strict report index:',len(rank),'ranked families;',notes['gap_count'],'oracle gaps')
