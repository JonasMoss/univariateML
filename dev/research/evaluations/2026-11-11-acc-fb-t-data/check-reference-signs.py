"""Audit final one-sided reference signs, independently at 90 digits.

The oracle's initial boundary nomination is not used as final KKT evidence.
Run through /tmp/umvl-cpu/run after all references have been written.
"""
import json
from pathlib import Path
import mpmath as m
from oracle import objective,names

D=Path(__file__).resolve().parent
m.mp.dps=90
cases={r['index']:r for r in json.loads((D/'cases.json').read_text())}
refs={r['index']:r for r in map(json.loads,(D/'stationary.jsonl').read_text().splitlines())}
assert set(cases)==set(refs)
checked=[]
for i,r in refs.items():
    if 'theta' not in r:continue
    assert r['precision_digits']==90
    assert r['positive_information']
    assert m.mpf(r['reference_coordinate_error'])<m.mpf('1e-55')
    c=cases[i];f=c['family']
    if 'mode_left_score' in r:
        left=m.mpf(r['mode_left_score']);right=m.mpf(r['mode_right_score'])
        assert left<0 and right>0,(i,left,right)
        if r.get('active_shape'):
            assert m.mpf(r['shape_right_score'])>0,(i,r['shape_right_score'])
        checked.append(dict(index=i,family=f,kind='knot one-sided mode and shape scores',
                            mode_left_score=str(left),mode_right_score=str(right),
                            shape_right_score=r['shape_right_score']))
        continue
    if not r.get('active_shape'):continue
    p=[m.mpf(r['theta'][k]) for k in names[f]]
    x=[m.mpf(v) for v in c['data']]
    shape=3 if f=='st' else 2
    def cost(d):
        v=p[:];v[shape]=d
        return -objective(f,x,v)
    value=cost(p[shape])
    score=m.diff(cost,p[shape],direction=1)
    if f=='lst' and 'mu' in c['held']:
        # This subset exempts the df>=1 restriction; df=1 is interior.
        assert abs(score)<m.mpf('1e-55')*max(1,abs(value)),(i,score)
        kind='interior held-location df score'
    else:
        assert score>=0,(i,score)
        kind='final right shape cost score'
    checked.append(dict(index=i,family=f,kind=kind,shape=str(p[shape]),
                        right_cost_score=str(score),scaled_score=str(score/max(1,abs(value)))))
(D/'reference-sign-verification.json').write_text(json.dumps(dict(precision_digits=90,
    finite_references=sum('theta' in r for r in refs.values()),checked=checked),indent=2)+'\n')
print('final independent reference signs PASS:',len(checked),'boundary/knot rows')
