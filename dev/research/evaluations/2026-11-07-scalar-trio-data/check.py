"""Independent 60-digit score/likelihood checks of check.cpp output.
Run check.cpp first; pass its output path as argv[1]. No optimizer output is
used as a global proof; complete-search proofs live in the dossiers.
"""
import json
import sys
import mpmath as mp
mp.mp.dps = 60
counts = {}; maxima = {}

def check(label, value, limit):
    value = float(abs(value)); maxima[label] = max(maxima.get(label, 0), value)
    assert value < limit, (label, value, limit)
    counts[label] = counts.get(label, 0) + 1

for line in open(sys.argv[1]):
    row = line.split(); kind = row[0]
    if kind == 'sample':
        x = list(map(mp.mpf, row[2:])); n = len(x)
    elif kind in ('fatigue', 'profile'):
        assert row[1] == '0', row
        a, b, ll = map(mp.mpf, row[2:])
        score = sum(1/(v+b)-1/(2*b)+v/(2*a*a*b*b)-1/(2*a*a*v) for v in x)
        value = sum(mp.log(v+b)-mp.log(2*a)-mp.log(b)/2-3*mp.log(v)/2-mp.log(2*mp.pi)/2-(v-b)**2/(2*a*a*v*b) for v in x)
        check(kind+'_score', score*b, 2e-9)
        check(kind+'_loglik', value-ll, 2e-10)
        if kind == 'profile':
            check('alpha_rms', a*a-sum((v-b)**2/(v*b) for v in x)/n, 2e-10)
    elif kind == 'sgomp':
        assert row[1] == '0', row
        b,e,ll=map(mp.mpf,row[2:5]); value=mp.mpf(0);sb=mp.mpf(0);se=mp.mpf(0)
        for v in x:
            ev=mp.exp(-b*v); av=-mp.expm1(-b*v)
            value+=mp.log(b)-b*v-e*ev+mp.log1p(e*av)
            sb+=1/b-v+e*v*ev*(1+1/(1+e*av))
            se+=-ev+av/(1+e*av)
        check('sgomp_rate_score',sb*b,2e-7)
        check('sgomp_shape_score',se*(1+e) if e else max(se,0),2e-7)
        check('sgomp_loglik',value-ll,2e-10)
    elif kind == 'genbeta':
        assert row[1]=='0',row
        tiny=mp.mpf(row[3])<mp.mpf('1e-12')
        mp.mp.dps=400 if tiny else 60
        p,q,t,ll=map(mp.mpf,row[2:]); y=[-mp.log(mp.mpf(v)) for v in ('.02','.2','.6','.9')]
        score=4-p*t*sum(y)+(q-1)*sum(t*v/mp.expm1(t*v) for v in y)
        value=4*(mp.log(t)-mp.log(mp.beta(p,q)))-p*t*sum(y)+(q-1)*sum(mp.log(-mp.expm1(-t*v)) for v in y)+sum(y)
        check('genbeta_score',score,2e-9)
        if tiny:check('genbeta_tiny_q_relative_score',score/q,2e-10)
        check('genbeta_loglik',value-ll,2e-10)
    elif kind == 'genbeta_large':
        assert row[1]=='0',row
        mp.mp.dps=400
        p,q,t,ll=map(mp.mpf,row[2:]); y=mp.log(2);z=t*y
        score=1-p*z+(q-1)*z/mp.expm1(z)
        value=mp.log(t)-mp.log(mp.beta(p,q))-p*z+(q-1)*mp.log1p(-mp.exp(-z))+y
        check('genbeta_large_q_relative_score',score/(p*z),2e-10)
        check('genbeta_large_q_loglik',value-ll,2e-9)
    elif kind == 'translation':
        assert row[1:]==['3','3','2'],row
        counts['translation_status']=3
    elif kind == 'near':
        assert row[1]=='3',row
summary={'precision_digits':60,'extreme_q_precision_digits':400,'seed':11622,'counts':counts,'maximum_absolute_errors':maxima}
print(json.dumps(summary,indent=2))
