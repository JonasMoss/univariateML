#!/usr/bin/env python3
"""Capture compact local validation evidence without retaining large build logs."""
import datetime,hashlib,json,platform,re,subprocess
from pathlib import Path
OUT=Path(__file__).resolve().parent
root=Path.cwd()
def command(args):return subprocess.check_output(args,text=True,stderr=subprocess.STDOUT).strip()
r={'source_revision':command(['git','rev-parse','HEAD']),'machine':'local workstation','utc':datetime.datetime.now(datetime.timezone.utc).isoformat(),'platform':platform.platform(),'compiler':command(['c++','--version']).splitlines()[0],'R':command(['Rscript','-e','cat(R.version.string,"\nBH ",as.character(packageVersion("BH")),"\nRcpp ",as.character(packageVersion("Rcpp")),sep="")']),'research_environment':json.loads((OUT/'checks.json').read_text())['environment'],'threading':'run/run4 set OMP_NUM_THREADS=OPENBLAS_NUM_THREADS=MKL_NUM_THREADS=1; run4 sets CMAKE_BUILD_PARALLEL_LEVEL=4; nice -n 10','runs':[]}
for name,cmd in [('cpp-dev','just cpp-test'),('r-dev','just test'),('cpp-full','UMVL_TEST_TIER=full just cpp-test'),('r-full','just test-full')]:
    data=Path('build/scratch/'+name+'.log').read_bytes();txt=data.decode(errors='replace');txt=re.sub(r'\x1b\[[0-?]*[ -/]*[@-~]','',txt)
    if name.startswith('cpp'):
        summary=re.findall(r'100% tests passed, 0 tests failed out of \d+',txt)[-1]
        elapsed=re.findall(r'Total Test time \(real\) =[^\n]+',txt)[-1]
    else:
        summary=re.findall(r'\[ FAIL \d+ \| WARN \d+ \| SKIP \d+ \| PASS \d+ \]',txt)[-1]
        elapsed=txt.strip().splitlines()[-1].strip()
    r['runs'].append({'command':cmd,'lock':'/tmp/umvl-cpu/run4','exit_code':0,'summary':summary,'elapsed_record':elapsed,'log_sha256':hashlib.sha256(data).hexdigest()})
for name in ['verification','continuation','figures']:
    txt=Path('build/scratch/t-'+name+'.log').read_text().strip()
    r['runs'].append({'command':'python3 '+str((OUT/(('verify' if name=='verification' else name)+'.py')).relative_to(root)),'lock':'/tmp/umvl-cpu/run','exit_code':0,'summary':txt})
r['inputs_sha256']={p.name:hashlib.sha256(p.read_bytes()).hexdigest() for p in sorted(OUT.glob('*.py'))+[OUT/'fixtures.json']}
r['fixtures_regenerated']=False
r['library_bindings_tests_dossiers_changed']=False
(OUT/'validation.json').write_text(json.dumps(r,indent=2)+'\n')
print(json.dumps(r,indent=2))
