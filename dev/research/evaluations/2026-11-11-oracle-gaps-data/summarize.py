"""Read-only final accounting of the exact gap manifest and consumer receipts."""
import collections,json,math,subprocess,sys
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4]
HERE=Path(__file__).resolve().parent
sys.path.insert(0,str(ROOT/'dev/validation'))
from quality.truth import DISTS
BASELINE='7825e912b3e5177254853cc020d52b662faeeaa8'


def clean(x):
    if isinstance(x,float) and not math.isfinite(x):return 'NaN' if math.isnan(x) else 'Inf' if x>0 else '-Inf'
    if isinstance(x,list):return [clean(v) for v in x]
    if isinstance(x,dict):return {k:clean(v) for k,v in x.items()}
    return x


def key(family,parameters):return family,tuple(parameters)


def main():
    gaps=json.loads((ROOT/'dev/research/evaluations/2026-11-11-dpqr-extreme-data/oracle-gaps.json').read_text())
    grouped=collections.defaultdict(list)
    for entry in gaps:grouped[entry['family']].append(entry['gap'])
    failures=[];consumer_receipts={}
    for label in ('final-native','final-double'):
        path=HERE/(label+'-failures.json')
        if not path.exists():raise FileNotFoundError('Required final consumer receipt: '+str(path))
        consumer_receipts[label]=json.loads((HERE/(label+'-summary.json')).read_text())
        failures += [dict(f,mode=label) for f in json.loads(path.read_text())]
    by_point=collections.defaultdict(list)
    for f in failures:by_point[key(f['family'],f['parameters'])].append(f)
    points=[];families=[]
    for family,entries in sorted(grouped.items()):
        path=DISTS/family/'truth.json';table=json.loads(path.read_text())
        old=json.loads(subprocess.check_output(['git','show',f'{BASELINE}:{path.relative_to(ROOT)}'],text=True,cwd=ROOT))
        old_points=old['parameters']+[e['parameters'] for e in old.get('excluded_parameter_points',[])]
        present=table['parameters']+[e['parameters'] for e in table.get('excluded_parameter_points',[])]
        receipt=HERE/'generation'/(family+'.json')
        records=json.loads(receipt.read_text()).get('records',[]) if receipt.exists() else []
        records={tuple(r['parameters']):r for r in records}
        closed=0;already=0;remaining=[];defect_points=0
        for gap in entries:
            params=gap['parameters'];settled=params in present
            closed+=settled;already+=params in old_points
            errors=by_point[key(family,params)]
            if errors:defect_points+=1
            reason=('Explicit scope exclusion: gengamma is owned by the concurrent fitter job.' if family=='gengamma'
                    else records.get(tuple(params),{}).get('reason','No final reference row present.'))
            if not settled:remaining.append(dict(parameters=params,reason=reason))
            points.append(dict(family=family,parameters=params,original_reason=gap['reason'],
                               already_present_at_baseline=params in old_points,
                               reference_status='closed' if settled else 'open',
                               remaining_reason=None if settled else reason,
                               consumer_failures=len(errors),active_consumer_failures=sum(e['excluded_reason'] is None for e in errors),
                               functions=sorted({e['function'] for e in errors}),
                               max_normalized_error=max([e['error'] for e in errors] or [0]),
                               nonfinite_actual=sum(not math.isfinite(e['actual']) for e in errors)))
        families.append(dict(family=family,before=len(entries),closed=closed,remaining=len(entries)-closed,
                             already_present_at_baseline=already,consumer_defect_points=defect_points,open=remaining))
    result=dict(baseline=BASELINE,consumer_receipts=consumer_receipts,
                consumer_total_active_failures=sum(r['active_failures'] for r in consumer_receipts.values()),requested_points=len(gaps),requested_families=len(grouped),
                in_scope_points=sum(g['family']!='gengamma' for g in gaps),
                in_scope_families=len(grouped)-('gengamma' in grouped),
                closed=sum(f['closed'] for f in families),remaining=sum(f['remaining'] for f in families),
                already_present_at_baseline=sum(f['already_present_at_baseline'] for f in families),
                new_consumer_defect_points=sum(f['consumer_defect_points'] for f in families),
                new_active_consumer_failures=sum(p['active_consumer_failures'] for p in points),
                families=families,points=points)
    (HERE/'summary.json').write_text(json.dumps(clean(result),indent=2,allow_nan=False)+'\n')
    print(json.dumps({k:v for k,v in result.items() if k not in ('families','points')},indent=2))


if __name__=='__main__':main()
