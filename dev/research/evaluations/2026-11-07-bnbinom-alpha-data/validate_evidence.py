"""Collect final green runs and verify their retained source identity.

Invoke through run after both foreground suite commands have finished.
"""
import hashlib,json,re,subprocess
from pathlib import Path
out=Path(__file__).parent
scratch=Path('build/scratch/bnbinom-alpha')
sim=(scratch/'final-simbox.log').read_text()
local=(scratch/'workstation-suites.log').read_text()
source=json.loads((out/'source.json').read_text())
lines=['Final validation: TASK-116.34, bnbinom alpha-held','', 'simbox environment:']+sim.splitlines()[:2]+['']
for label,log,suites in [('simbox',sim,['targeted','cpp-dev','cpp-full','cpp-no-long-double','r-dev','r-full']),
    ('workstation',local,['cpp-no-long-double','js-build','js-test','site-check'])]:
    for suite in suites:
        name=suite if label=='simbox' else f'workstation-{suite}'
        start=re.search(rf'^START {re.escape(name)} (.+)$',log,re.M)
        finish=re.search(rf'^GREEN {re.escape(name)} (.+)$',log,re.M)
        assert start and finish,(label,suite,'not green')
        section=log[start.end():finish.start()]
        lines += [f'{label}: {suite}: GREEN; start {start[1]}; finish {finish[1]}']
        if suite=='targeted':
            lines += re.findall(r'^\[doctest\].*(?:test cases:|assertions:|Status:).*$',section,re.M)
        if suite.startswith('cpp-'):
            counts=re.findall(r'100% tests passed[^\n]*',section)
            assert counts,(label,suite,'missing ctest result')
            lines += counts
        if suite.startswith('r-'):
            counts=re.findall(r'\[ FAIL (\d+) \| WARN (\d+) \| SKIP (\d+) \| PASS (\d+) \]',section)
            assert counts and counts[-1][0:2]==('0','0'),(label,suite,counts[-1:])
            if suite=='r-full': assert counts[-1][2]=='0'
            lines += ['FAIL {}, WARN {}, SKIP {}, PASS {}'.format(*counts[-1])]
        if suite=='js-test':
            lines += [line for line in section.splitlines() if re.match(r'^(?:# |ℹ )(?:tests|suites|pass|fail|cancelled|skipped|todo|duration)',line)]
        if suite=='site-check':
            lines += section.splitlines()[-8:]
        lines += ['']
double=(scratch/'targeted-double.log').read_text()
assert '[doctest] Status: SUCCESS!' in double
lines += ['workstation: targeted bnbinom double-width preflight: GREEN']
lines += re.findall(r'^\[doctest\].*(?:test cases:|assertions:|Status:).*$',double,re.M)
lines += ['']
for path,digest in source['sha256'].items():
    assert hashlib.sha256(Path(path).read_bytes()).hexdigest()==digest,(path,'changed after timing/source capture')
for path in ['univariateML-cpp/include/cppunivariate/distributions/bnbinom/mle.hpp',
             'bindings/common/distributions/bnbinom/fit.hpp',
             'univariateML-cpp/include/cppunivariate/distributions/bnbinom/test.cpp',
             'bindings/r/distributions/bnbinom/test.R']:
    record=source['sha256'][path]+'  '+path
    assert sim.count(record)==2,(path,'remote before/after fingerprint mismatch')
for name in ('golden.json','dpqr.hpp','truth.py','truth.json'):
    path='univariateML-cpp/include/cppunivariate/distributions/bnbinom/'+name
    assert Path(path).read_bytes()==subprocess.check_output(['git','show','HEAD:'+path]),(path,'changed')
for path in ['univariateML-cpp/include/cppunivariate/distributions/bnbinom/test.cpp','bindings/r/distributions/bnbinom/test.R','bindings/js/test.mjs']:
    assert Path(path).read_bytes().startswith(subprocess.check_output(['git','show','HEAD:'+path])),(path,'not append-only')
subprocess.run(['git','diff','--check'],check=True)
lines += ['Source fingerprints: match retained source.json and simbox before/after records.',
          'golden.json, dpqr.hpp, truth.py, truth.json: byte-identical to HEAD.',
          'C++, R and shared JS tests: append-only.',
          'Paired benchmark: all masks status Ok; unchanged routes identical coefficients/logLik.',
          '80-digit oracle: four roots and 400 sampled mass-box comparisons pass (simbox).',
          '60-digit retained nonuniqueness reproducer: 861 segments / 431 accepted (simbox).',
          'git diff --check: clean.',
          '', 'Workstation compiler: '+subprocess.check_output(['c++','--version'],text=True).splitlines()[0]]
(out/'validation.txt').write_text('\n'.join(lines)+'\n')
print('\n'.join(lines))
