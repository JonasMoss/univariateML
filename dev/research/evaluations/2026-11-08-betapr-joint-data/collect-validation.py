"""Collect completed frozen-source validation logs; run only after all suites end."""
import hashlib,json,pathlib,re
root=pathlib.Path(__file__).resolve().parents[4]
data=pathlib.Path(__file__).resolve().parent
checks=[]
machines={row.split('\t')[0]:row.split('\t')[1] for row in (data/'suites.tsv').read_text().splitlines()}
local_results={row.split('\t')[0]:int(row.split('\t')[2]) for row in (data/'local-suites.tsv').read_text().splitlines()}
assert all(local_results.get(key)==0 for key in ['js-build','js-test','site-check'])
for label,file,machine in [
 ('C++ dev','cpp-dev.log',machines['cpp-dev']),('C++ full','cpp-full.log',machines['cpp-full']),
 ('C++ no-long-double','cpp-no-long-double-simbox.log',machines['cpp-no-long-double-simbox']),
 ('C++ no-long-double','local-cpp-test-no-long-double.log','workstation')]:
    text=(root/'build/scratch'/file).read_text()
    match=re.findall(r'100% tests passed.*out of (\d+)',text)
    assert match,(label,file)
    checks.append({'suite':label,'machine':machine,'passed':int(match[-1]),'failed':0})
for label,file in [('R dev','r-dev.log'),('R full','r-full.log')]:
    text=(root/'build/scratch'/file).read_text()
    match=re.findall(r'\[ FAIL (\d+) \| WARN (\d+) \| SKIP (\d+) \| PASS (\d+) \]',text)[-1]
    fail,warn,skip,passed=map(int,match)
    assert fail==warn==0
    if label=='R full':assert skip==0
    checks.append({'suite':label,'machine':machines['r-dev' if label=='R dev' else 'r-full'],'passed':passed,'failed':fail,'warnings':warn,'skipped':skip})
text=(root/'build/scratch/local-js-test.log').read_text()
passed=re.findall(r'# pass (\d+)',text);failed=re.findall(r'# fail (\d+)',text)
assert passed and failed and int(failed[-1])==0
checks.append({'suite':'JS build and tests','machine':'workstation','passed':int(passed[-1]),'failed':0})
site=(root/'build/scratch/local-site-check.log').read_text()
assert 'Total dist:' in site
checks.append({'suite':'site-check','machine':'workstation','passed':True,'summary':site.splitlines()[-1]})
for line in (data/'source-sha256.txt').read_text().splitlines():
    digest,file=line.split(maxsplit=1)
    assert hashlib.sha256((root/file).read_bytes()).hexdigest()==digest,file
(data/'validation.json').write_text(json.dumps({'baseline':'9fee8c34b838db7b11b7518a071f61495d23f282','source_manifest':'source-sha256.txt','checks':checks},indent=2)+'\n')
print(json.dumps(checks,indent=2))
