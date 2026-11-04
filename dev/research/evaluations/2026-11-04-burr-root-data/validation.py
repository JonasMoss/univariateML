"""Retain compact final suite evidence and toolchain versions; no suite launches."""
import json
from pathlib import Path
import re
import subprocess
from isolate import HERE

root=HERE.parents[3]
checks=[('C++ dev','cpp-dev','/tmp/umvl-cpu/run4 just cpp-test','100% tests passed, 0 tests failed out of 106'),
        ('R dev','r-dev','/tmp/umvl-cpu/run4 just test','[ FAIL 0 | WARN 0 | SKIP 153 | PASS 20507 ]'),
        ('C++ full','cpp-full','/tmp/umvl-cpu/run4 env UMVL_TEST_TIER=full just cpp-test','100% tests passed, 0 tests failed out of 127'),
        ('R full','r-full','/tmp/umvl-cpu/run4 just test-full','[ FAIL 0 | WARN 0 | SKIP 0 | PASS 43324 ]')]
rows=[]
for name,slug,cmd,expected in checks:
    log=(root/f'build/scratch/burr-root-{slug}.log').read_text()
    assert expected in log,(name,expected)
    assert not re.search(r'^Error:|^Execution halted|\b[1-9][0-9]* tests failed out of',log,re.M)
    rows.append(dict(name=name,machine='local workstation',command=cmd,result=expected,
                     final_output=log.splitlines()[-7:]))
versions={key:subprocess.check_output(cmd,text=True).strip() for key,cmd in [
    ('cxx',['c++','--version']),('R',['Rscript','-e','cat(R.version.string)']),
    ('BH',['Rscript','-e','cat(as.character(packageVersion("BH")))'])]}
result=dict(source_revision=subprocess.check_output(['git','rev-parse','HEAD'],text=True).strip(),
            branch=subprocess.check_output(['git','branch','--show-current'],text=True).strip(),
            suites=rows,versions=versions)
(HERE/'validation.json').write_text(json.dumps(result,indent=2)+'\n')
print('Final suites:',len(rows),'green; toolchain retained.')
