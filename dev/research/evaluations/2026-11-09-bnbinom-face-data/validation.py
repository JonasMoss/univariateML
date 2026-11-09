"""Freeze completed validation evidence; refuse any missing green stage."""
import hashlib
import json
import re
from pathlib import Path
here=Path(__file__).parent
scratch=Path('build/scratch/bnbinom-face')
source=json.loads((here/'source.json').read_text())
assert all(hashlib.sha256(Path(p).read_bytes()).hexdigest()==h for p,h in source.items())
marker=(scratch/'final-workstation.log').read_text()
records=[];excerpts=[]
for suite in ['cpp-dev','cpp-full','cpp-no-long-double','test','test-full']:
    assert f'GREEN workstation {suite} ' in marker,suite
    f=scratch/(suite+'-final.log');s=f.read_text()
    r=dict(command={'cpp-dev':'just cpp-test','cpp-full':'UMVL_TEST_TIER=full just cpp-test','cpp-no-long-double':'just cpp-test-no-long-double','test':'just test','test-full':'just test-full'}[suite],machine='workstation',exit=0,sha256=hashlib.sha256(f.read_bytes()).hexdigest())
    if suite.startswith('cpp'):
        n=re.findall(r'100% tests passed, 0 tests failed out of (\d+)',s);assert n,suite;r['shards']=int(n[-1])
    else:
        n=re.findall(r'\[ FAIL (\d+) \| WARN (\d+) \| SKIP (\d+) \| PASS (\d+) \]',s);assert n,suite
        r.update(zip(['fail','warn','skip','pass'],map(int,n[-1])));assert r['fail']==r['warn']==0
    records.append(r);excerpts.append(suite+'\n'+'\n'.join(s.splitlines()[-15:]))
js=(scratch/'js-final.log').read_text()
for stage in ['js-build','js-test']:assert f'GREEN workstation {stage} ' in js
assert '# fail 0' in js
records.append(dict(command='just js-build; just js-test',machine='workstation',exit=0,tests=int(re.findall(r'# tests (\d+)',js)[-1])))
site=(scratch/'site-final.log').read_text();assert 'Total dist:' in site and 'PASS:' in site
records.append(dict(command='just site-check',machine='workstation',exit=0,sha256=hashlib.sha256(site.encode()).hexdigest()))
target=(here/'targeted.txt').read_text();assert target.count('[doctest] Status: SUCCESS!')==2
records.append(dict(command='targeted.sh',machine='workstation',modes=['native','double-width'],cases_per_mode=51,exit=0))
hp=json.loads((here/'high-precision.json').read_text());assert float(hp['gamma_max_error_to_bound'])<1 and float(hp['nb_max_error_to_bound'])<1
large=json.loads((here/'large-budget.json').read_text());assert large[0]['proved'] and large[0]['evaluations']<=128*100000
(here/'validation.json').write_text(json.dumps(dict(source_fingerprints_match=True,runs=records,simbox='Open: remote system commands return I/O errors; no suite started. See simbox-health.txt.'),indent=2)+'\n')
(here/'validation.txt').write_text(marker+'\n\n'+'\n\n'.join(excerpts)+'\n\nJS\n'+'\n'.join(js.splitlines()[-12:])+'\n\nSITE\n'+'\n'.join(site.splitlines()[-16:])+'\n')
print(json.dumps(records,indent=2))
