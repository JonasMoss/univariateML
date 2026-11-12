"""Build the completed TASK-212 report from the saved validation records."""
import hashlib,json,re,subprocess,sys
from decimal import Decimal
from pathlib import Path
HERE=Path(__file__).resolve().parent;ROOT=HERE.parents[3]
REPORT=HERE.parent/'2026-11-12-bnb-loglik-report.md'
subprocess.run([sys.executable,str(HERE/'record_environment.py')],cwd=ROOT,check=True,stdout=subprocess.DEVNULL)
env=json.loads((HERE/'environment.json').read_text())
comparison=json.loads((HERE/'comparison.json').read_text())
rechecks=json.loads((HERE/'reference-recheck.json').read_text())
symmetry=json.loads((HERE/'symmetry.json').read_text())
suites=[line.split('\t') for line in (HERE/'suites.tsv').read_text().splitlines()]
assert len(suites)==5 and all(int(r[2])==0 for r in suites)
js=(HERE/'js-final.log').read_text()
js_counts={k:int(re.findall(r'^# '+k+r' (\d+)$',js,re.M)[-1]) for k in ['tests','pass','fail','skipped','cancelled','todo']}
assert js_counts['fail']==js_counts['cancelled']==js_counts['todo']==0
assert js_counts['tests']==js_counts['pass']+js_counts['skipped']
target=(HERE/'targeted-before-js-retry.log').read_text()
target_results=json.loads(re.findall(r'^Results: (.+)$',target,re.M)[-1])
assert all(r['exit']==0 for r in target_results[:-1])
assert target_results[-1]['command']==['just','js-test']
assert re.findall(r'100% tests passed, 0 tests failed out of (\d+)',target)==['2','2']
def r_counts(log):
 records=re.findall(r'\[ FAIL (\d+) \| WARN (\d+) \| SKIP (\d+) \| PASS (\d+) \]',log)
 fail,warn,skip,passed=map(int,records[-1]);assert fail==warn==0
 return dict(passed=passed,skipped=skip,fail=fail,warn=warn)
def cpp_count(name):
 return int(re.findall(r'100% tests passed, 0 tests failed out of (\d+)',(HERE/name).read_text())[-1])
rdev=r_counts((HERE/'test.log').read_text());rfull=r_counts((HERE/'test-full.log').read_text());rtarget=r_counts(target)
assert len(rechecks)==sum(len(s['changes']) for s in comparison.values())==100
assert all(r['reference_relative_difference']<1e-88 for r in rechecks)
assert all(len(rows)==4 and all(r['passed'] for r in rows) for rows in symmetry.values())
for mode,s in comparison.items():
 before=json.loads((HERE/f'before-{mode}.json').read_text());after=json.loads((HERE/f'after-{mode}.json').read_text())
 assert len(before)==len(after)==s['rows']==120
 for a,b in zip(before,after):assert {k:v for k,v in a.items() if k!='logLik'}=={k:v for k,v in b.items() if k!='logLik'}
 assert [a['index'] for a,b in zip(before,after) if a['logLik']!=b['logLik']]==[r['index'] for r in s['changes']]
 assert all(r['normalized_error']['after']<=1e-10 for r in s['changes'])
source_prefix='univariateML-cpp/include/cppunivariate/distributions/bnbinom/'
expected={source_prefix+'mle.hpp',source_prefix+'test.cpp'}
changed=set(subprocess.check_output(['git','diff','--name-only'],cwd=ROOT,text=True).splitlines())
assert changed==expected,changed
mle=(ROOT/source_prefix/'mle.hpp').read_text()
group='''    std::map<int,double> counts;
    for(int v:x)counts[v]+=1;
    CPPUNIV_EXTENDED_REAL loglik=0;
    for(const auto& v:counts)loglik+=static_cast<CPPUNIV_EXTENDED_REAL>(v.second)*
      distributions::dbnbinom(v.first,r,stopped.alpha,b,true);'''
prior='''    CPPUNIV_EXTENDED_REAL loglik=0;
    for(int v:x)loglik+=distributions::dbnbinom(v,r,stopped.alpha,b,true);'''
assert mle.count(group)==1
stages={'final_mle_sha256':hashlib.sha256(mle.encode()).hexdigest(),'dev_suite_mle_sha256_before_frequency_grouping':hashlib.sha256(mle.replace(group,prior).encode()).hexdigest(),'prior_stage_reconstruction':'Exact reversal of the final frequency-grouping block; all other source lines were identical.'}
(HERE/'source-stages.json').write_text(json.dumps(stages,indent=2)+'\n')
validation=dict(final_checks_green=True,suites=suites,cpp_dev=cpp_count('cpp-test.log'),cpp_double_fallback=cpp_count('cpp-test-no-long-double.log'),cpp_double_workstation=cpp_count('cpp-test-no-long-double-workstation.log'),R_dev=rdev,R_full=rfull,R_targeted=rtarget,JS=js_counts,paired_replays=240,changed_log_likelihoods=100,reference_rechecks=len(rechecks),symmetry_checks=8,tracked_changes=sorted(changed),commits_created=False)
(HERE/'report-validation.json').write_text(json.dumps(validation,indent=2)+'\n')
(HERE/'final-status.txt').write_text(subprocess.check_output(['git','status','--short'],cwd=ROOT,text=True))
def link(name,label=None):return f'[{label or name}]({HERE.name}/{name})'
def ref(s):return format(Decimal(s),'.22g')
def error(v):return f'{v:.3e}'
lines=['# TASK-212: bnbinom fitter log-likelihood accumulation','',
'The successful fitter result now reports a frequency-weighted sum of the public log masses at its returned coefficients. All 240 before/after comparisons preserve coefficients, statuses, iteration counts and certification flags exactly. All final checks passed; no commits were made.','',
'Baseline: `'+env['baseline']+'`, branch `codex/bnb-loglik`, with the uncommitted '+
'[`mle.hpp`](../../../'+source_prefix+'mle.hpp) and [`test.cpp`](../../../'+source_prefix+'test.cpp) changes. '+
link('environment.json','Environment and source/input fingerprints')+' and '+link('source-stages.json','tested source stages')+' record the source identity.','',
'## Implementation and evidence','',
'Only `Ok` results are recomputed, after the search, certificate decisions and coefficient refinement. The search objective and comparisons retain their existing arithmetic. The two interchangeable count-shape coordinates are ordered only for density evaluation, preserving exact size/beta swap symmetry. Repeated counts are evaluated once and weighted by frequency.','',
'For `r = size`, `a = alpha`, `b = beta`, the independent reference evaluates the exact identity','',
'```text\nlog p(x) = logGamma(r+x) - logGamma(r) - logGamma(x+1)\n           + logBeta(r+a, x+b) - logBeta(a,b).\n```','',
'The public density uses stable gamma ratios at large counts, avoiding cancellation between rising-factorial and factorial terms of order `x log(x)`. The identity is algebraic; the error measurements here are independent numerical evidence at the returned exact binary64 coefficients. They concern likelihood evaluation.','',
'All changed rows were evaluated with a separate 100-decimal-digit mpmath log-gamma expression and rechecked at 200 digits with the beta-function identity. The references agree to at least 88 significant decimal places. Error throughout this report is `abs(logLik - reference) / max(1, abs(reference))`; the [accuracy contract](../../design/accuracy-contract.md#log-likelihood) requires at most `1e-10`.','',
'## Replay census','',
'The frozen archive supplies all 112 bnbinom cases. The frozen 500-observation golden (original seed 13086) is replayed with all eight held/free subsets, using its stored estimates for held values. Every fit uses C++ `tol = 1e-10`, `maxit = 600`. Both precision modes rebuild and replay the baseline and final fitter. No fixtures or stored golden estimates were regenerated.','',
'| Precision | Paired cases | Changed logLik | Largest new normalized error |\n|---|---:|---:|---:|']
for mode,s in comparison.items():lines.append(f"| {mode} | {s['rows']} | {len(s['changes'])} | {error(max(r['normalized_error']['after'] for r in s['changes']))} |")
lines += ['', 'Coefficient and status equality is checked on the 17-digit round-trip serialization, including stopped rows. The scripts additionally require identical iteration counts and certification flags. '+link('comparison.json','Complete comparison')+', '+link('reference-recheck.json','200-digit rechecks')+' and '+link('symmetry.json','eight focused symmetry checks')+' retain the measurements.','',
'## Case 579','',
'Data: `[1000000,1000001,1000003,1000008,1000011]`; size `1.0672234787479615` and beta `5.36473383510874` held. Alpha remains `0.0825043451682516`.','',
'| Mode | Before logLik | After logLik | Before error | After error |\n|---|---:|---:|---:|---:|']
for mode,s in comparison.items():
 r=next(r for r in s['changes'] if r['index']==579)
 lines.append(f"| {mode} | {r['before']} | {r['after']} | {error(r['normalized_error']['before'])} | {error(r['normalized_error']['after'])} |")
lines += ['', 'Independent reference: `'+next(r for r in comparison['native']['changes'] if r['index']==579)['reference']+'`.','',
'The appended regression requires `Ok`, unchanged held values, the expected alpha, reference agreement at `1e-10`, and density-sum agreement at `1e-14`. The latter also detects the original native-precision discrepancy. The case passed in both precision modes.','',
'## Validation','',
'| Final check | Machine | Result |\n|---|---|---|',
f"| C++ dev¹ | Workstation, GCC 15.2 | {validation['cpp_dev']}/{validation['cpp_dev']} shards passed; {link('cpp-test.log','log')} |",
f"| R dev¹ | Workstation | {rdev['passed']} pass, {rdev['skipped']} skips, 0 failures/warnings; {link('test.log','log')} |",
f"| R full | Workstation | {rfull['passed']} pass, {rfull['skipped']} skips, 0 failures/warnings; {link('test-full.log','log')} |",
f"| C++ all shards, double-only, simbox fallback | Workstation, GCC 15.2 | {validation['cpp_double_fallback']}/{validation['cpp_double_fallback']} passed; {link('cpp-test-no-long-double.log','log')} |",
f"| Explicit workstation double-only run | Workstation, GCC 15.2 | {validation['cpp_double_workstation']}/{validation['cpp_double_workstation']} passed; {link('cpp-test-no-long-double-workstation.log','log')} |",
f"| Final targeted native + double C++ | Workstation | 2/2 selected shards in each mode; {link('targeted-before-js-retry.log','component log')} |",
f"| Final targeted R dev | Workstation | {rtarget['passed']} pass, {rtarget['skipped']} skips, 0 failures/warnings; same component log |",
f"| JavaScript build | Workstation, Emscripten | Passed; same component log |",
f"| Final JavaScript selected + common tests | Workstation | {js_counts['pass']}/{js_counts['tests']} passed, {js_counts['skipped']} skips; {link('js-final.log','log')} |",'',
'¹ C++ dev and R dev passed before the final reporting-only frequency grouping. After that adjustment, the changed-family C++ entries in both modes and the targeted R file/filtered shared checks were rerun. Full R and both all-shard double-only runs used the final source. The selected families were bnbinom and the conservative reverse-include selection yulesimon.','',
'Simbox was initially unreachable; an R dev attempt later started there but lost its connection. The prescribed wrappers returned exit 75, and all final remote-requested runs completed through the authorized local fallback. There is no successful simbox/GCC 13 validation in this record. '+link('suites.tsv','Machine/exit-code ledger')+' and '+link('final_runs.sh','full-run driver')+' preserve these outcomes.','',
'The first density-sum implementation exposed two failures in the existing exact size/beta swap test in double-only mode; ordering the evaluation coordinates fixed them without editing that test. '+link('targeted-initial.log','Initial failure log')+'. The focused replay and final family suites passed afterward.','',
'The first JS test invocation failed because I restored an unrelated generated gengamma metadata entry before testing. Regenerating the runtime metadata fixed those facade failures; the final JS retry passed, then the tracked generated file was restored byte-for-byte. '+link('generated-metadata-drift.json','Cleanup record')+'. No JS source change remains.','',
'Workstation environment: '+env['compiler']+'; '+env['R'].replace('\n','; ')+'; Python '+sys.version.split()[0]+'; mpmath '+env['mpmath']+'. Exact OS, Boost header version, Node, Emscripten, installed SciPy version (unused), flags and hashes are in the environment record. Slot wrappers provided one-thread BLAS/OpenMP and nice level 10.','',
'## Artifact viewer','',
'The completed report could not be opened in the artifact pane: `bin/show` returned HTTP 403 because this worktree is outside the viewer’s registered readable roots. The remaining delivery item is to register this worktree through a Nook thread or the server’s `CODEX_MATH_WEB_VIEW_ROOTS` configuration. Viewer configuration and other worktrees were left unchanged. '+link('viewer-status.json','Recorded viewer result')+'.','',
'## Reproduction','',
'```sh\nexport TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"\nmkdir -p "$TMPDIR" build/scratch\n/tmp/umvl-cpu/run python3 '+str(HERE.relative_to(ROOT))+'/replay.py\n/tmp/umvl-cpu/run python3 '+str(HERE.relative_to(ROOT))+'/check_references.py\n/tmp/umvl-cpu/run python3 '+str(HERE.relative_to(ROOT))+'/check_symmetry.py\n```','',
link('replay.py','Replay source')+' reconstructs the baseline fitter with `git show` in ignored scratch and rejects changes to shared header dependencies. '+link('check_references.py','Reference checker')+' and '+link('check_symmetry.py','symmetry checker')+' are independent executable checks. '+link('report-validation.json','Report validation')+' records the completed matrix. The regression census is finite; it is not a proof over all datasets and parameter values. No implementation or replay items remain open.','',
'## Every changed likelihood','',
'References below are rounded to 22 significant decimal digits for display. The linked JSON retains 90-digit references and the 200-digit rechecks. `golden` means all free; in `golden-mask-k`, bits 1, 2 and 4 hold size, alpha and beta respectively at the frozen golden estimates. Errors use the unrounded high-precision reference.']
for mode,s in comparison.items():
 lines += ['', '### '+mode,'','| Case | Before logLik | After logLik | Reference | Before error | After error |','|---|---:|---:|---:|---:|---:|']
 for r in s['changes']:lines.append(f"| {r['index']} | {r['before']} | {r['after']} | {ref(r['reference'])} | {error(r['normalized_error']['before'])} | {error(r['normalized_error']['after'])} |")
REPORT.write_text('\n'.join(lines)+'\n')
for href in re.findall(r'\]\(([^)]+)\)',REPORT.read_text()):
 assert (REPORT.parent/href.split('#')[0]).resolve().exists(),href
assert sum(line.startswith('| ') and ('e-' in line or 'e+' in line) for line in lines)>=100
print(REPORT)
print(json.dumps(validation,indent=2))
