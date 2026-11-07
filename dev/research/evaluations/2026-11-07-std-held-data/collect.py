"""Collect finished validation logs; run only after the suite commands exit."""
from pathlib import Path
import gzip, hashlib, json, platform, re, shlex, subprocess
ROOT=Path(__file__).resolve().parents[4]
OUT=Path(__file__).resolve().parent
SCRATCH=ROOT/'build/scratch'
raw=(SCRATCH/'final-simbox-10.log').read_text()
no_ld=(SCRATCH/'no-long-double-restored.log').read_text()
r=re.findall(r'^\[ FAIL (\d+) \| WARN (\d+) \| SKIP (\d+) \| PASS (\d+) \]$',raw,re.M)
assert len(r)==2 and all(a=='0' and b=='0' for a,b,_,_ in r),r
cpp=re.findall(r'100% tests passed(?:, 0 tests failed)? out of (\d+)',raw)+re.findall(r'100% tests passed(?:, 0 tests failed)? out of (\d+)',no_ld)
assert cpp==['106','129','129'],cpp
assert (SCRATCH/'js-test-restored.log').stat().st_mtime >= (SCRATCH/'js-build-restored.log').stat().st_mtime
assert (SCRATCH/'site-check-restored.log').stat().st_mtime >= (SCRATCH/'js-test-restored.log').stat().st_mtime
js=(SCRATCH/'js-test-restored.log').read_text()
js_counts={k:int(v) for k,v in re.findall(r'^# (tests|pass|fail|cancelled|skipped) (\d+)$',js,re.M)}
assert js_counts['tests']==js_counts['pass'] and js_counts['fail']==js_counts['cancelled']==0,js_counts
site=(SCRATCH/'site-check-restored.log').read_text()
assert 'MathML on every family' in site and 'generated C++ snippets compile and link as C++17' in site
assert 'error: Recipe' not in site
files=[]
for family in ['lst','std']:
 for name in ['mle.hpp','README.md','test.cpp','golden.json','truth.json']:
  files.append(f'univariateML-cpp/include/cppunivariate/distributions/{family}/{name}')
 files += [f'bindings/common/distributions/{family}/fit.hpp',f'bindings/r/distributions/{family}/test.R']
files += ['univariateML-cpp/include/cppunivariate/distributions/lst/location_certificate.hpp','bindings/js/test.mjs']
files += [f'dev/research/evaluations/2026-11-07-std-held-data/{name}' for name in ['check.py','held_check.cpp','timing.cpp','derivation.md','collect.py']]
manifest={'base_revision':'017b9f8ec3bed7610ccd41c62738bce1ea987316','branch':'codex/std-held','uncommitted':True,'workstation':platform.platform(),'python':platform.python_version(),'local_compiler':subprocess.check_output(['c++','--version'],text=True).splitlines()[0],'sha256':{p:hashlib.sha256((ROOT/p).read_bytes()).hexdigest() for p in files}}
(OUT/'source-manifest.json').write_text(json.dumps(manifest,indent=2)+'\n')
logs=['final-simbox-10.log','final-simbox-9.log','no-long-double-stop-expectation.log','no-long-double-restored.log','js-build-final.log','js-test-final.log','site-check-final.log','final-simbox-8.log','final-simbox-7.log','final-simbox-6.log','js-build-restored.log','js-test-restored.log','site-check-restored.log','final-simbox.log','final-simbox-2.log','final-simbox-3.log','final-simbox-4.log','final-simbox-5.log','site-check.log']
for name in logs:
 with (OUT/(name+'.gz')).open('wb') as dest:
  dest.write(gzip.compress((SCRATCH/name).read_bytes(),compresslevel=6,mtime=0))
record={'base_revision':manifest['base_revision'],'uncommitted':True,'machines':{'r_and_cpp_suites':'simbox','cpp_no_long_double':'workstation','javascript_and_site':'workstation','independent_checks_and_timings':'workstation'},'final_native_command':"/tmp/umvl-cpu/simbox-run sh -c 'c++ -O2 -std=c++17 -IunivariateML-cpp/include -I<BH include> dev/research/evaluations/2026-11-07-std-held-data/held_check.cpp -o build/scratch/held-check && build/scratch/held-check && just test && just test-full && just cpp-test && UMVL_TEST_TIER=full just cpp-test'",'final_workstation_command':"/tmp/umvl-cpu/run4 sh -c 'just cpp-test-no-long-double && just js-build && just js-test && GOMAXPROCS=4 just site-check'",'r_dev':dict(zip(['fail','warn','skip','pass'],map(int,r[0]))),'r_full':dict(zip(['fail','warn','skip','pass'],map(int,r[1]))),'cpp':dict(zip(['dev','full','no_long_double_full'],map(int,cpp))),'cpp_elapsed_seconds':list(map(float,re.findall(r'Total Test time \(real\) =\s+([\d.]+) sec',raw+no_ld))),'javascript':js_counts,'site':'passed: MathML, links, graph, snippets and runtime assets','exit_codes':{'native_chain':0,'workstation_no_ld_js_site_chain':0},'simbox_fallback':False,'logs':{name+'.gz':hashlib.sha256((OUT/(name+'.gz')).read_bytes()).hexdigest() for name in logs},'notes':['Review feedback restored the three uncertified free-shape numerical searches; prior refusal-route logs are superseded.','R dev skips are declared tier exclusions; R full has no skips.','Superseded native chains and failed shared-restriction/site runs are retained as development evidence, not final validation.','No goldens or truth fixtures were regenerated.']}
compile_command="""c++ -O2 -std=c++17 -IunivariateML-cpp/include -I"$(Rscript -e 'cat(system.file("include",package="BH"))')" dev/research/evaluations/2026-11-07-std-held-data/held_check.cpp -o build/scratch/held-check"""
native_script=compile_command+" && build/scratch/held-check && just test && just test-full && just cpp-test && UMVL_TEST_TIER=full just cpp-test"
record['final_native_command']='/tmp/umvl-cpu/simbox-run sh -c '+shlex.quote(native_script)
record['native_source_fingerprints']='simbox-source.sha256'
record['warnings']=['Workstation JS build (also invoked by site-check) emits an existing -Wtautological-compare warning from unchanged binom/mle.hpp:160; builds exit zero. R suites report zero warnings.']
record['compiler_warnings']={'native':sorted(set(re.findall(r'^.*warning:.*$',raw,re.M))),'workstation_no_long_double':sorted(set(re.findall(r'^.*warning:.*$',no_ld,re.M))),'javascript':sorted(set(re.findall(r'^.*warning:.*$',(SCRATCH/'js-build-restored.log').read_text(),re.M)))}
record['targeted_checks']={'standalone_cpp':'passed on simbox; workstation rerun recorded in held-check.txt','high_precision':'80-digit mpmath checks passed','paired_timing':'completed on workstation; same deterministic n=500 workload; combined family timings'}
verified=(SCRATCH/'simbox-source-verified.sha256').read_text()
for line in verified.splitlines():
 digest,path=line.split('  ',1)
 assert hashlib.sha256((ROOT/path).read_bytes()).hexdigest()==digest,path
(OUT/'simbox-source.sha256').write_text(verified)
(OUT/'validation.json').write_text(json.dumps(record,indent=2)+'\n')
print(json.dumps({k:record[k] for k in ['r_dev','r_full','cpp','javascript','site']},indent=2))
