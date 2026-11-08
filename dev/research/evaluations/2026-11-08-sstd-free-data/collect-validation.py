import hashlib,json,pathlib,re
p=pathlib.Path(__file__).resolve().parent
repo=p.parents[3]
checks={}
for file in ['source.sha256','golden.sha256']:
    for line in (p/file).read_text().splitlines():
        expected,name=line.split(None,1)
        actual=hashlib.sha256((repo/name).read_bytes()).hexdigest()
        assert actual==expected,(name,expected,actual)
for line in (p/'simbox-status.tsv').read_text().splitlines():
    label,code=line.split();assert code=='0',(label,code)
for label in ['cpp-dev','cpp-full','cpp-no-long-double']:
    s=(p/f'frozen-{label}.txt').read_text()
    m=re.findall(r'(\d+)% tests passed(?:, (\d+) tests failed)? out of (\d+)',s)[-1]
    assert m[0]=='100' and (not m[1] or m[1]=='0')
    checks[label]={'machine':'simbox GCC 13.3','passed':int(m[2]),'failed':0,'seconds':float(re.findall(r'Total Test time \(real\) =\s*([\d.]+) sec',s)[-1])}
for label in ['r-dev','r-full']:
    s=(p/f'frozen-{label}.txt').read_text()
    failed,warn,skip,passed=map(int,re.findall(r'\[ FAIL (\d+) \| WARN (\d+) \| SKIP (\d+) \| PASS (\d+) \]',s)[-1])
    assert failed==warn==0
    if label=='r-full':assert skip==0
    checks[label]={'machine':'simbox','passed':passed,'failed':failed,'warnings':warn,'skipped':skip,'test_duration_seconds':float(re.findall(r'Duration: ([\d.]+) s',s)[-1])}
s=(p/'frozen-cpp-no-long-double-local.txt').read_text()
passed=s.count('Test Passed.')
failed=s.count('Test Failed.')
total=int(re.findall(r'\d+/(\d+) Testing:',s)[-1])
assert passed==total==129 and failed==0
checks['cpp-no-long-double-local']={'machine':'workstation GCC 15.2','passed':passed,'failed':failed,'log_source':'persisted CTest LastTest.log'}
s=(p/'local-final.txt').read_text()
for label in ['js-build','js-test','site-check']:assert f'PASS {label}' in s,label
s=(p/'js-test.txt').read_text()
js={k:int(re.findall(r'# '+k+r' (\d+)',s)[-1]) for k in ['tests','pass','fail','cancelled','skipped']}
assert js['tests']==js['pass'] and js['fail']==js['cancelled']==js['skipped']==0,js
checks['js-test']={'machine':'workstation',**js}
checks['js-build']={'machine':'workstation','passed':True}
checks['site-check']={'machine':'workstation','passed':True}
math=json.loads((p/'math-checks.json').read_text())
assert math['violations']==0 and math['unchanged_routes_bitwise_equal']==15
result={'all_required_runs_green':True,'production_sources_match_frozen_hashes':True,'golden_bytes_unchanged':True,'certification':'partial; new xi-only route with held moments and nu, including explicit arithmetic stops','checks':checks,'math':math}
(p/'validation.json').write_text(json.dumps(result,indent=2)+'\n')
print(json.dumps(checks,indent=2))
