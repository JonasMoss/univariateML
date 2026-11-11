"""Aggregate the paired seeded studies; no third-party dependencies."""
import csv, json, statistics
from pathlib import Path
folder=Path(__file__).resolve().parent
output={}
for name in ['held-before','study-local-100','study-local-1000','allfree-before','allfree-local-100','allfree-local-1000']:
    path=folder/(name+'.csv')
    if not path.exists(): continue
    rows=list(csv.DictReader(path.open()))
    groups={}
    for row in rows:
        scope=row['family']+('/'+row['hold'] if not name.startswith('allfree') else '')
        groups.setdefault(scope,[]).append(row)
    result={}
    for scope, values in groups.items():
        times=sorted(float(v['milliseconds']) for v in values)
        success=[v for v in values if v['status']=='0']
        # Old all-free GEV returned only certified successes; its per-fit
        # field was not populated. Preserve the raw field in the input CSV.
        certified=sum(v['certified']=='1' or (name=='allfree-before' and v['family']=='gev') for v in success)
        result[scope]={'fits':len(values),'certified':certified,'uncertified':len(success)-certified,'stops':len(values)-len(success),
                       'median_ms':statistics.median(times),'p95_ms':times[min(len(times)-1,int(.95*len(times)))],
                       'maximum_ms':max(times),'total_ms':sum(times),
                       'uncertified_ids':[v['sample_id'] for v in success if v['certified']=='0' and not(name=='allfree-before' and v['family']=='gev')],
                       'stop_ids':[v['sample_id'] for v in values if v['status']!='0']}
    output[name]=result
for phase in ['before','after']:
    path=folder/('edges-'+phase+'.csv')
    if not path.exists(): continue
    rows=list(csv.DictReader(path.open()))
    output['cancellation-'+phase]={}
    for family in ['gev','frechet']:
        selected=[v for v in rows if v['family']==family]
        success=[v for v in selected if v['status']=='0']
        output['cancellation-'+phase][family]={
            'fits':len(selected),'certified':sum(v['certified']=='1' for v in success),
            'uncertified':sum(v['certified']=='0' for v in success),
            'stops':len(selected)-len(success),
            'median_ms':statistics.median(float(v['milliseconds']) for v in selected)}
for label in ['before','after-rescue','before-1000','after-1000']:
    path=folder/('seam-'+label+'.csv')
    if not path.exists(): continue
    rows=list(csv.DictReader(path.open()))
    if len(rows)!=4: continue
    success=[v for v in rows if v['status']=='0']
    item={'fits':len(rows),'certified':sum(v['certified']=='1' for v in success),
          'uncertified':sum(v['certified']=='0' for v in success),'stops':len(rows)-len(success)}
    if all('milliseconds' in v for v in rows):
        times=sorted(float(v['milliseconds']) for v in rows)
        item.update(median_ms=statistics.median(times),maximum_ms=max(times),total_ms=sum(times))
    output['seam-'+label]=item
(folder/'summary.json').write_text(json.dumps(output,indent=2)+'\n')
print(json.dumps(output,indent=2))
