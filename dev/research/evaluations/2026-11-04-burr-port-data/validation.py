"""Check compact final-run records. Run after all foreground suites finish."""
import hashlib
import importlib.metadata
import json
import platform
from pathlib import Path
import re
import subprocess
HERE=Path(__file__).resolve().parent
ROOT=HERE.parents[3]
commands=[
 ('cpp-dev.log','/tmp/umvl-cpu/run4 just cpp-test',106),
 ('cpp-no-long-double.log','/tmp/umvl-cpu/run4 just cpp-test-no-long-double',127),
 ('r-dev.log','/tmp/umvl-cpu/run4 just test',None),
 ('cpp-full.log','/tmp/umvl-cpu/run4 env UMVL_TEST_TIER=full just cpp-test',127),
 ('r-full.log','/tmp/umvl-cpu/run4 just test-full',None)]
records=[]
for filename,command,count in commands:
    raw=(ROOT/'build/scratch'/filename).read_bytes();text=raw.decode(errors='replace')
    row=dict(command=command,machine='local workstation',exit_code=0,log_sha256=hashlib.sha256(raw).hexdigest())
    if count:
        assert f'100% tests passed, 0 tests failed out of {count}' in text,filename
        row.update(passed=count,failed=0,seconds=float(re.search(r'Total Test time \(real\) =\s*([\d.]+) sec',text).group(1)))
    else:
        fail,warn,skip,passed=map(int,re.findall(r'\[ FAIL (\d+) \| WARN (\d+) \| SKIP (\d+) \| PASS (\d+) \]',text)[-1])
        assert fail==0 and warn==0 and skip==(153 if filename=='r-dev.log' else 0),filename
        row.update(passed=passed,failed=fail,warnings=warn,skipped=skip,seconds=float(text.strip().splitlines()[-1].split()[-1]))
    records.append(row)
record=dict(source_revision=subprocess.check_output(['git','rev-parse','HEAD'],text=True).strip(),
    environment=dict(platform=platform.platform(),libc=subprocess.check_output(['getconf','GNU_LIBC_VERSION'],text=True).strip(),
        compiler=subprocess.check_output(['c++','--version'],text=True).splitlines()[0],
        R=subprocess.check_output(['Rscript','-e','cat(R.version.string,"; BH ",as.character(packageVersion("BH")),"; Rcpp ",as.character(packageVersion("Rcpp")),"; testthat ",as.character(packageVersion("testthat")),sep="")'],text=True),
        python=platform.python_version(),mpmath=importlib.metadata.version('mpmath')),
    runs=records,
    evidence_command='/tmp/umvl-cpu/run sh dev/research/evaluations/2026-11-04-burr-port-data/run.sh',
    scratch_logs='build/scratch/; compact records and hashes retained here; no report link requires ignored logs')
(HERE/'validation.json').write_text(json.dumps(record,indent=2)+'\n')
print(json.dumps(record,indent=2))
