"""Collect final, machine-readable evidence after all suites and audits pass."""
import hashlib,json,platform,re,subprocess
from pathlib import Path
import mpmath as m

D=Path(__file__).resolve().parent;root=D.parents[3]
m.mp.dps=90
def read(name):return json.loads((D/name).read_text())
def rows(name):return {r['index']:r for r in map(json.loads,(D/name).read_text().splitlines())}
cases={r['index']:r for r in read('cases.json')}
refs=rows('stationary.jsonl')
data={mode:rows(mode+'.jsonl') for mode in ['before-native','before-double','after-native','after-double']}
verification=read('verification.json');spots=read('spot-verification.json')
spots={k:spots[k] for k in ['after-r','after-wasm']}
(D/'spot-verification.json').write_text(json.dumps(spots,indent=2)+'\n')
golden=read('golden-sstd-verification.json')
assert set(golden['checks'])=={'before-native','before-double','after-native','after-double','after-r','after-wasm'}
signs=read('reference-sign-verification.json')
inventory=read('route-inventory.json')
assert len(inventory['routes'])==24

def hidden_certificate(c):
    """Successful dedicated C++ dispatches whose binding omits the flag."""
    h=c['held'];f=c['family'];hm='mean' in h;hs='sd' in h;hn='nu' in h;hx='xi' in h
    if f=='sstd':
        return ((hx and h['xi']==1 and not(hm or hs or hn)) or
                (hm and hs and hn) or (hn and hx and (hm or hs)))
    if f!='st':return False
    loc='xi' in h;scale='omega' in h;skew='alpha' in h;shape='nu' in h
    if loc and scale and skew and shape:return True
    if skew and h['alpha']==0 and not(loc or scale or shape):return True
    if loc and scale and not skew and shape:return True
    if not loc and scale and not skew and len(set(c['data']))==1:return True
    if not loc and scale and skew and shape:return True
    if loc and not scale and shape:
        if skew and h['alpha']==0:return True
        pos=any(v>h['xi'] for v in c['data']);neg=any(v<h['xi'] for v in c['data'])
        if not skew and not(pos and neg):return True
    return False

hidden=[i for i,c in cases.items() if not data['after-native'][i]['error'] and hidden_certificate(c)]
explicit=[i for i,r in data['before-native'].items() if not r['error'] and r.get('certified')==1]
certificates={}
for mode in ['native','double']:
    before={json.loads(line)['index']:line for line in (D/f'before-{mode}.jsonl').read_text().splitlines()}
    after={json.loads(line)['index']:line for line in (D/f'after-{mode}.jsonl').read_text().splitlines()}
    assert all(before[i]==after[i] for i in explicit+hidden)
    certificates[mode]=dict(explicit=explicit,unexposed_identified_by_dispatch=hidden,
                            byte_identical=len(set(explicit+hidden)))
(D/'certified-preservation.json').write_text(json.dumps(certificates,indent=2)+'\n')

def external(c):return c['family']=='sged' and c['held'].get('xi')==1 and 'nu' not in c['held']
owned={i for i,c in cases.items() if not data['after-native'][i]['error'] and i not in explicit+hidden and not external(c)}
def route(i):
    c=cases[i];f=c['family'];h=c['held'];held=','.join(h) or 'all free'
    if f=='lst' and list(h)==['df']:
        return 'lst: joint held df<1' if h['df']<1 else 'lst: joint inclusion fallback df>=1'
    if f=='std' and list(h)==['nu']:return 'std: held nu / lst transport'
    if i==33000:return 'lst: near df=1 / held sigma'
    if i==33001:return 'st: near nu=1 / held xi,omega,alpha'
    if i==33101:return 'st: large-df CDF / held xi,omega,alpha'
    if i==33002:return 'sged: near nu=1 / held mean,sd,xi'
    if f=='sged' and 'mode_left_score' in refs[i]:
        kind='subunit corner' if float(refs[i]['theta']['nu'])<1 else 'shape-one corner'
        return f'{f}: {kind} / {held}'
    return f'{f}: {held}'

details=read('coefficient-errors.json');table={}
for i in sorted(owned):
    key=route(i);g=table.setdefault(key,dict(cases=0,modes={}))
    g['cases']+=1
    for mode in data:
        p=data[mode][i];modes=g['modes'];a=modes.setdefault(mode,dict(successes=0,stops=0,maximum_scaled_error=None,worst=None))
        if p['error']:a['stops']+=1
        else:
            r=next(r for r in details if r['mode']==mode and r['index']==i)
            a['successes']+=1
            if a['maximum_scaled_error'] is None or r['maximum_scaled_error']>a['maximum_scaled_error']:
                a['maximum_scaled_error']=r['maximum_scaled_error'];a['worst']=i
table['sstd: all free (golden seed 12791)']=dict(cases=1,modes={k:dict(successes=1,stops=0,
    maximum_scaled_error=v['maximum_scaled_error'],worst=40001) for k,v in golden['checks'].items()})
(D/'route-contract-summary.json').write_text(json.dumps(table,indent=2)+'\n')

expected=['cpp-dev','cpp-double','js-build','js-test','r-full','r-dev','site']
lines=(D/'test-runs.txt').read_text().splitlines()[-7:]
assert [s.split()[0] for s in lines]==expected,lines
assert all(' workstation PASS ' in s for s in lines)
tests={name:dict(machine='workstation',result='PASS',total_seconds=int(line.split()[-1][:-1])) for name,line in zip(expected,lines)}
for name in ['cpp-dev','cpp-double']:
    log=(root/f'build/scratch/{name}.log').read_text()
    count=int(re.findall(r'100% tests passed, 0 tests failed out of (\d+)',log)[-1])
    tests[name]['tests']=count
    tests[name]['test_seconds']=float(re.findall(r'Total Test time \(real\) =\s*([\d.]+)',log)[-1])
for name in ['r-full','r-dev']:
    log=(root/f'build/scratch/{name}.log').read_text()
    a=re.findall(r'\[ FAIL (\d+) \| WARN (\d+) \| SKIP (\d+) \| PASS (\d+) \]',log)[-1]
    tests[name].update(zip(['fail','warn','skip','assertions'],map(int,a)))
    assert tests[name]['fail']==tests[name]['warn']==0
log=(root/'build/scratch/js-test.log').read_text()
tests['js-test']['tests']=int(re.findall(r'# tests (\d+)',log)[-1])
assert int(re.findall(r'# fail (\d+)',log)[-1])==0

def command(args):return subprocess.check_output(args,cwd=root,text=True,stderr=subprocess.STDOUT).strip()
versions=dict(python=platform.python_version(),mpmath=m.__version__,
              cpp=command(['c++','--version']).splitlines()[0],
              node=command(['node','--version']),R=command(['Rscript','-e','cat(R.version.string)']))
versions['wasm']=command(['bash','-c','source "${EMSDK:-$HOME/emsdk}/emsdk_env.sh" >/dev/null 2>&1; em++ --version']).splitlines()[0]
changed=command(['git','--no-optional-locks','diff','--name-only']).splitlines()
new_headers=[f'univariateML-cpp/include/cppunivariate/distributions/{f}/accuracy.hpp' for f in ['lst','sged']]
hashes={p:hashlib.sha256((root/p).read_bytes()).hexdigest() for p in sorted(set(changed+new_headers)) if (root/p).is_file()}
(D/'source-sha256.json').write_text(json.dumps(hashes,indent=2)+'\n')

families={}
for f in ['std','lst','sstd','sged','st','slash']:
    families[f]={}
    for mode in data:
        a=[r for r in details if r['mode']==mode and r['index'] in owned and cases[r['index']]['family']==f]
        maximum=max((r['maximum_scaled_error'] for r in a),default=0)
        if f=='sstd':maximum=max(maximum,golden['checks'][mode]['maximum_scaled_error'])
        families[f][mode]=maximum
out=dict(baseline=inventory['baseline'],versions=versions,tests=tests,
         main_replay_cases=len(cases),supplementary_cases=1,independent_cases=len(cases)+1,
         main_finite=len([r for r in data['after-native'].values() if not r['error']]),
         owned_main_finite=len(owned),owned_with_supplement=len(owned)+1,
         certificate_preservation=certificates,by_family=families,
         main_verification=verification,spots={k:{p:v for p,v in q.items() if p not in ['owned','preserved_or_external']} for k,q in spots.items()},
         supplementary_spots={k:v for k,v in golden['checks'].items() if k in ['after-r','after-wasm']},
         final_reference_sign_rows=len(signs['checked']),
         stopped_main_indices=[i for i,r in data['after-native'].items() if r['error']],
         original_false_positive_indices=[r['index'] for r in read('held-adjudication.json') if not r['before_miss_confirmed']])
(D/'final-summary.json').write_text(json.dumps(out,indent=2)+'\n')
print('Final suites:',json.dumps(tests))
print('Hidden certified rows:',hidden,'; all certified replay rows byte identical:',len(set(explicit+hidden)))
print('Independent cases:',len(cases)+1,'; owned finite:',len(owned)+1,'; false-positive held misses:',out['original_false_positive_indices'])
print('Versions:',json.dumps(versions))
print('Family maxima:',json.dumps(families))
print('Route | n | native before | double before | native after | double after')
for key,g in sorted(table.items()):
    values=[]
    for mode in data:
        a=g['modes'][mode];v=a['maximum_scaled_error']
        values.append('refused' if v is None else f'{v:.3e}')
    print(key,'|',g['cases'],'|',' | '.join(values))
