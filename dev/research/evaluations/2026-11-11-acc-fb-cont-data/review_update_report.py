"""Update the report only after every requested review-round check is green."""
from pathlib import Path
import json,re,datetime
root=Path.cwd();p=root/'dev/research/evaluations/2026-11-11-acc-fb-cont-data';work=root/'build/scratch/review-round';f=p.parent/'2026-11-11-acc-fb-cont-report.md'
a=json.loads((p/'review-validation.json').read_text());assert a['checks'] and all(c['exit_code']==0 for c in a['checks']);assert 'ALL REVIEW CHECKS FINISHED' in (work/'validation.log').read_text()
audit=json.loads((p/'review-scope-audit.json').read_text());assert not audit['errors']
def clean(name):return re.sub(r'\x1b\[[0-9;]*m','',(work/(name+'.log')).read_text())
cpp={}
for mode in ['native','double']:
 text=clean('cpp-families-'+mode);m=re.search(r'(\d+)% tests passed, (\d+) tests failed out of (\d+)',text);assert m and int(m[2])==0;cpp[mode]=int(m[3])
r=re.findall(r'\[ FAIL (\d+) \| WARN (\d+) \| SKIP (\d+) \| PASS (\d+) \]',clean('r-families'));assert r;fail,warn,skip,passed=map(int,r[-1]);assert fail==warn==skip==0
js={key:int(value) for key,value in re.findall(r'^# (tests|pass|fail|cancelled|skipped) (\d+)',clean('js-full'),re.M)};assert js['fail']==0 and js['pass']==js['tests']
g=json.loads((p/'review-ged-31101-results.json').read_text());assert len(g)==6;maximum=max(v for row in g for v in row['errors'].values())
summary=dict(cpp_passed=cpp,r=dict(passed=passed,fail=fail,warn=warn,skip=skip),js=js,ged_max_scaled_error=maximum,production_source_unchanged=True,inputs_and_fit_arguments_preserved=True,private_proposal_unchanged_from_bc8ff7be=True,checks=a['checks'])
(p/'review-summary.json').write_text(json.dumps(summary,indent=2)+'\n')
s=f.read_text();old='**The all-green acceptance condition remains open.**';i=s.index(old);j=s.index('\n\nThe implementation changes',i)
lead=f'''**Review round completed: all requested checks are green.** The nine regular/slow C++ shards for Burr, inverse Burr, Pareto IV, Bhatt and GED pass in each arithmetic mode; the five R files pass at full tier; the complete JavaScript suite passes {js['pass']}/{js['tests']}. The five expectation conflicts were resolved under Jonas's existing accuracy/fallback rules and the reviewer's explicit authorization. Original samples and fit arguments are unchanged.

GED already fixes SGED unit-skew case 31101 in this tree. New GED-owned C++ and R regressions verify the parent and delegated child; independent 100-digit comparison in native, double-only, R and WASM has maximum scaled error **{maximum:.6e}**. No production fitter change was needed in this review.'''
s=s[:i]+lead+s[j:]
s=s.replace('appended C++ regressions and this evaluation bundle.','appended C++ regressions, the explicitly authorized C++/R/JS expectation replacements, and this evaluation bundle.',1)
s=s.replace('core/common/binding sources','core/common/production binding sources',1)
s=s.replace('## Completed validation runs','## Original full validation, before expectation review',1)
s=s.replace('All final runs used the local workstation','The original full runs used the local workstation',1)
s=s.replace('The manifests record nonzero broad-suite exits explicitly.','These historical manifests record the pre-review nonzero broad-suite exits explicitly. The final requested review checks and their green results follow below.',1)
i=s.index('## Open decisions and existing search limitation');j=s.index('**Open existing search limitation:',i)
review=f'''## Review round: authorized expectations and final results

The reviewer confirmed that the five conflicts follow from decisions already made by Jonas: a proved refutation or failed search may stop; an unreachable tolerance is a numerical-accuracy failure. Only the specified expectations were replaced. No production fitter header, control, message, dossier, golden, d/p/q/r source or unrelated test changed during this review.

| Authorized change | New assertion and retained evidence |
|---|---|
| Nearly-constant Burr / inverse Burr / Pareto IV in C++, R and JS | Numerical/refutation outcome; the old nomination remains a finite, positive, bounded-likelihood witness. C++ matches its coefficients exactly to the private nomination; R/JS identify the retained large-shape nomination. Weibull/inverse-Weibull closure witnesses, when carried, retain finite positive coefficients and valid likelihood bounds |
| Zero-certificate-allowance Burr cases | Numerical refutation, not a budget-conditioned `Ok`; the original nomination remains a witness and its scale box stays below the smallest observation |
| Bhatt `tol=1e-30` in C++, R and JS | Numerical-accuracy failure; the finite best candidate retains held `sigma=1,a=2`, finite location and valid physical likelihood bounds |
| Bhatt `maxit=1` | Local-search iteration failure; no unverified finite replacement is accepted |
| Bhatt private/public bit identity at three unit scales | Each public coefficient satisfies the `1e-10` bound against the independent references, its physical likelihood is checked, and the private proposal remains finite. A separate source-identity audit proves `bhatt_detail::legacy_candidate` is byte-identical to `bc8ff7be` |

The Weibull-closure improvements are smaller than the floating likelihood enclosures. The tests preserve both witnesses and consistent bounds; they do not claim that those intervals strictly separate. The independent 420-digit comparisons above supply the improvement evidence.

### GED / SGED case 31101

Inputs remain `(0.7321666961121761, 2.2766042947262384, -0.5491691631533211)`, with held `sd=1.0178559902092952` and SGED `xi=1`. Independent differentiation of the raw GED log density at 100 digits gives

`mean* = 0.8637175657864586408862805910629875338...`

`nu* = 27.49733661683168433866852526549805668...`.

The reference information is positive definite. The existing GED local finish already corrects the old `27.497341303893556` nomination; no additional GED algorithm change was necessary. The appended GED C++ regression checks both public fitters in each arithmetic mode, including literal holds and exact child/parent transport at identical controls. The GED R regression checks the public parent and unit-skew delegate. This resolves acc-fb-t open item 2 for this source tree; no SGED file was edited.

| Backend / family | Returned nu | Mean scaled error | Nu relative error |
|---|---:|---:|---:|
'''
for row in g:review+=f"| {row['backend']} / {row['family']} | {row['estimates']['nu']:.17g} | {row['errors']['mean']:.3e} | {row['errors']['nu']:.3e} |\n"
review+=f'''
### Final requested review checks (local workstation)

| Check | Result | Exit |
|---|---|---:|
| Burr, inverse Burr, Pareto IV, Bhatt, GED C++ regular/slow shards, native | {cpp['native']}/{cpp['native']} passed | 0 |
| Same C++ shards, double-only | {cpp['double']}/{cpp['double']} passed | 0 |
| The five R test files, full tier | PASS {passed}, FAIL 0, WARN 0, SKIP 0 | 0 |
| WASM rebuild and complete JavaScript suite | {js['pass']}/{js['tests']} passed; zero failures, cancellations or skips | 0 |
| Case 31101 independent-reference and R/WASM comparisons | Six backend/family rows pass; maximum scaled error {maximum:.3e} | 0 |
| Scope, original-data/fit-argument and private-proposal audits | No violations | 0 |

All commands ran through the local slot wrappers and were waited for in the foreground. The validation driver freezes the authorized test files and checks all other pre-review source hashes before and after each command. The original full numerical replay results remain applicable because all production fitter files are unchanged. The original pre-authorization full-suite failures above are historical; the requested final review checks are all green.

'''
s=s[:i]+review+s[j:]
s=s.replace('**Open existing search limitation:', '**Preserved existing search limitation:',1)
s=s.replace('The authoritative final records are','The original full-validation records are',1)
s=s.replace('Reproduce the complete validation from this worktree with:','The final review records are `review-validation.json`, `review-summary.json`, `review-scope-audit.json`, `review-private-proposal-identity.json` and `review-ged-31101-reference.json` / `review-ged-31101-results.json` in the same data directory. Reproduce the requested review checks from this worktree with:',1)
s=s.replace('/validate_all.py\n```','/review_validate.py\n```',1)
start=s.index('`validate_all.py` reconstructs baseline headers');end=s.index('\n\nFinal report assembled',start)
s=s[:start]+'''`review_validate.py` runs the targeted C++/R checks, WASM build, complete JS suite and delegated GED measurements. The final run used `review_refresh_js.py` to add the separate WASM build and repeat JS, GED measurements and the source audit after the initial JS pass. R measurement values use 17 significant digits, preserving the exact held binary64 scale. The pre-correction JSON-serialization failure is retained in `review-validation-before-ged-serialization-fix.json`; no fit or test failed in that measurement attempt. `review_audit.py` checks authorized test-block changes, unchanged original data/fit arguments and private-proposal source identity. `review_ged_reference.py` retains the independent 100-digit calculation. The original `validate_all.py` and pre-review manifests remain historical records of the original full validation; their original no-test-rewrite guard predates the review authorization. Large scratch builds/logs remain under `build/scratch/`.
'''+s[end:]
s=re.sub(r'Final report assembled at [^.]+\.', 'Final report assembled at '+datetime.datetime.now(datetime.timezone.utc).strftime('%Y-%m-%d %H:%M UTC')+'.',s)
f.write_text(s);print(json.dumps(summary,indent=2));print('Updated',f)
