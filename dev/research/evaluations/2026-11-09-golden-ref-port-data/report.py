#!/usr/bin/env python3
"""Write the report only after every local final tier has finished green."""
import json,re,subprocess
from pathlib import Path
out=Path(__file__).resolve().parent
root=out.parents[3]
rows=json.loads((out/'audit.json').read_text())
checks=json.loads((out/'checks.json').read_text())
latest={r['name']:r for r in checks}
required=['cpp-dev','r-dev','js-build','js-test','cpp-full','r-full','cpp-no-long-double']
assert all(k in latest and latest[k]['exit_code']==0 for k in required)
refs=json.loads((out/'reference-checks.json').read_text())
assert len(refs)==98 and all(r['input_sha256_unchanged'] for r in refs)
quality=json.loads((out/'quality.json').read_text());assert len(quality)==99 and all(r['passed'] for r in quality)
master=json.loads((out/'master-r-tolerances.json').read_text())
bhatt=json.loads((out/'bhatt-deferred.json').read_text())
def fmt(v):return f'{v:.3g}'
def maximum(r):return max(r['measured'].values())
def witness(r):
 return max(((e,m,k) for m,cs in r['components'].items() for k,e in cs.items()))[1:]
def method(r):
 text=r['method']
 if 'median interval' in text:return 'L'
 if 'integer size profile' in text or 'Poisson' in text:return 'P+N'
 if 'finite integer enumeration' in text:return 'E'
 if 'sample-extreme' in text:return 'S+N' if r['free'] else 'S'
 if r['id']=='dlaplace':return 'M+N'
 return 'N'
def evidence(r):
 g=r['global']
 if isinstance(g,str):return g.replace('|',';')
 if 'dossier' in g.get('kind',''):return 'dossier'
 return f"{g.get('starts',0)} starts; gap {g.get('gap',0):.2g}"
def summary(k):
 text=(root/'build/scratch'/f'final-{k}.log').read_text(errors='replace')
 text=re.sub(r'\x1b\[[0-9;]*[A-Za-z]','',text)
 if k.startswith('cpp'):
  matches=re.findall(r'100% tests passed, 0 tests failed out of \d+',text)
  if matches:return matches[-1]
 if k.startswith('r-'):
  matches=re.findall(r'\[ FAIL\s+\d+\s*\| WARN\s+\d+\s*\| SKIP\s+\d+\s*\| PASS\s+\d+\s*\]',text)
  if matches:return matches[-1]
 if k=='js-test':
  passes=re.findall(r'# pass (\d+)',text);fails=re.findall(r'# fail (\d+)',text)
  if passes:return f'{passes[-1]} passes; {fails[-1]} failures'
 return 'exit 0'
lines=['# Golden references port (TASK-149.1)', '',
'2026-11-09. Worktree `golden-ref-port`, branch `codex/golden-ref-port`, master base `a99ca0dc`. No commits, board edits, fitter changes, default changes, or changes to frozen inputs.', '',
'Ported the independent-reference machinery, 98 canonical reference blocks, C++ golden comparisons, R and JS regression policies, and quality-harness support. Bhatt’s canonical fixture and C++ tests/tolerance definitions are unchanged, as requested for the concurrent bhatt-allfree job. All local final development and full tiers completed successfully. The required simbox no-long-double run is open because that machine’s filesystem became read-only; its earlier double-intermediate fit capture completed successfully.', '',
'## References and frozen fields', '',
'All 98 references were recomputed or re-verified from the current README density at 60 working digits with a 50-digit Newton step target. The 98 input digests agree with the imported independent references. Frozen field values and their original JSON value text are byte-identical to master for every field except the intentionally replaced `estimates`, `logLik`, and added `reference`. Bhatt’s entire canonical file is byte-identical to master. No new canonical families or changed frozen inputs were found.', '',
'The port fixes an input-digest instability: writing a reference can change a held estimate’s representation from integer `0` to floating `0.0`. Held values now come from authoritative frozen `fit_args`. Ten affected families were rechecked after this fix; their original digests were recovered. A write-and-verify regression test covers this case. See [reference-checks.json](2026-11-09-golden-ref-port-data/reference-checks.json).', '',
'**N** is damped Newton on symbolic derivatives of the README log density. A reference requires a total score norm below `1e-45` and negative-definite observed log-likelihood Hessian; singular, nonconverged, infeasible and unchecked candidates fail without being written. **L** is the Laplace median interval and mean absolute deviation, with the upper median as representative. **S** uses support monotonicity and sample extremes. **M** uses the lower integer median for discrete Laplace. **E** exhausts a finite hyper/nhyper design. **P** independently profiles binomial probabilities and enumerates integer size below an analytically excluded Poisson-envelope tail. Boundary-active coefficients require a correctly signed one-sided score. Held binary64 inputs are interpreted exactly.', '',
'These are different kinds of evidence. Newton score/curvature checks establish local numerical evidence. The imported scoped dossier statements and multistart records are retained when their inputs agree; all 48 dossier records match the current parsed statements and reasons (see [dossier-evidence-checks.json](2026-11-09-golden-ref-port-data/dossier-evidence-checks.json)). This port does not independently re-prove those statements. The fresh all-family quality run also passed. Multistart agreement is numerical evidence, not a global optimality proof. Integer-tail comparisons are certified in exact arithmetic conditional on their computed high-precision comparisons, without an outward-rounded interval claim. Genbeta retains the high-precision rescoring that resolves its apparent binary64 oracle improvement; the current quality-oracle candidate is rescored again in the final audit. Integer winner/tail gaps are checked against a practical margin of `1e-40 * (1 + abs(logLik))` at 50-digit reference precision. See [final-audit.json](2026-11-09-golden-ref-port-data/final-audit.json). Triangular `c` remains explicitly unrefined at a kink; the regular-slice reference and multistart comparison do not prove that coordinate globally.', '',
'## Measurements and allowances', '',
'Every canonical family was replayed in native GCC 15.2, workstation double-intermediate mode, simbox double-intermediate mode (GCC 13), workstation R (GCC 15.2), and WASM. The double modes define `BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS`. Frozen `fit_args` are replayed, including held coefficients and required designs; the remaining controls use their defaults. R captures each double as a 17-digit string. Measurements include every coefficient and log likelihood: relative error against a nonzero reference, absolute error at zero, and zero for equal signed infinities. Calibration takes the maximum of error against the rounded binary64 fixture and against the stored 25-digit reference, over all five modes.', '',
'Allowances round upward on the decimal 1–2–3–5 lattice, with at most three times the measured error and no blanket floor. Missing families, missing modes, nonfinite errors and failed fits stop the audit. These are empirical allowances for these frozen samples and platforms, not universal accuracy bounds. For the 98 calibrated families, C++/JS golden comparisons remove the additive unit scale; R adds a componentwise assertion. Bhatt retains its pre-port JS per-component widths and additive scale, alongside its unchanged R and C++ allowances. One per-family envelope covers coefficients and log likelihood; independent density/score assertions retain their own tighter checks. Non-default/held checks retain separately scoped allowances, including CMP’s held-lambda solve and the explicit-tolerance replicated Lomax test.', '',
f'{sum(maximum(r)<=1e-10 for r in rows)} of 98 calibrated families measure at or below `1e-10`; {sum(maximum(r)>1e-10 for r in rows)} remain worse. Relative to the earlier rollout, {sum(r["tolerance"]<r["previous"]["tolerance"] for r in rows)} allowances tighten, {sum(r["tolerance"]==r["previous"]["tolerance"] for r in rows)} are unchanged, and two widen. Complete component errors and every mode are retained in [audit.json](2026-11-09-golden-ref-port-data/audit.json); old measurements are retained in [previous-measurements.json](2026-11-09-golden-ref-port-data/previous-measurements.json).', '',
'The two increases relative to the old rollout are SSTD (`5e-6` to `2e-5`) and GB2 (`5e-8` to `2e-7`). SSTD’s largest error is the current native/R all-free finite-difference BFGS candidate under unchanged `gradtol=1.220703e-4`. GB2’s largest error is the workstation double-intermediate all-free profile result under unchanged `gradtol=1e-7`; native error remains about `2.38e-13`. Their stopping rules do not bound every coefficient by the control value. The mode-specific differences are numerical observations; this port does not attribute them to a proved single arithmetic operation.', '',
'GED improves from about `2.25e-6` to `2.58e-11`, beta-prime from `1.45e-7` to `8.35e-12`, and Tukey lambda from `9.92e-8` to `7.30e-15`; these leave the old worse-than-`1e-10` table. EMG improves from `1.71e-7` to `4.07e-9`; Fréchet and Huber also tighten. SSTD and SGED’s all-free routes still use the legacy finite-difference controls; newer held-subset certificates must not be confused with a tighter all-free coefficient guarantee.', '',
'## Families still worse than 1e-10', '',
'| Family | Old measured error | Current error | Worst mode / component | Allowance | Cause |',
'|---|---:|---:|---|---:|---|']
for r in sorted(rows,key=maximum,reverse=True):
 if maximum(r)<=1e-10:continue
 m,k=witness(r)
 lines.append(f"| {r['id']} | {fmt(r['previous']['max_error'])} | {fmt(maximum(r))} | {m} / {k} | {fmt(r['tolerance'])} | {r['reason']} |")
lines += ['',f"Bhatt is also above `1e-10`: maximum error {fmt(max(bhatt['measured'].values()))} against the earlier independent reference. Its reference and allowance remain reserved for bhatt-allfree, rather than being recalibrated here. The five measurements and the unchanged-file check are retained in [bhatt-deferred.json](2026-11-09-golden-ref-port-data/bhatt-deferred.json).", '',
'## Changes from master’s stored-output tolerances', '',
'The old master assertions compared with the fitter’s earlier output. Switching to an independent reference can require a wider allowance even when the fitter is unchanged. The following table identifies every R allowance wider than master’s old threshold, its measured error, and its cause. C++ default golden checks use the same calibrated allowance; unrelated identity and historical checks preserve their own intent.', '',
'| Family | Master R allowance | New allowance | Current error | Cause |', '|---|---:|---:|---:|---|']
small_causes={'cauchy':'default absolute-step solve (abstol=1.220703e-4)', 'fpareto':'conditional profile solve (reltol=1e-7)', 'invpareto':'default Newton/profile solve (reltol=1.220703e-4)', 'nbinom':'default hybrid Newton size step (reltol=1.220703e-4)', 'yulesimon':'log-shape bracket width (reltol=1e-10)', 'zeta':'score/bracket stopping (reltol=1e-10)'}
for r in rows:
 if r['tolerance']>master[r['id']]:
  lines.append(f"| {r['id']} | {fmt(master[r['id']])} | {fmt(r['tolerance'])} | {fmt(maximum(r))} | {small_causes.get(r['id'],r['reason'])} |")
lines += ['', '## Family ledger', '',
'“Master error” is the old stored fixture’s error against the independent reference; it is not the current fitter’s error. Evidence is scoped as above. The JSON ledger retains the exact method, refined and unrefined coordinates, global record, all five modes, and component errors.', '',
'| Family | Method | Global evidence | Master error | Old measured error | Current error | Old allowance | New allowance |', '|---|---|---|---:|---:|---:|---:|---:|']
for r in rows:
 id=r['id'];link=f'[{id}](../../../univariateML-cpp/include/cppunivariate/distributions/{id}/README.md)'
 lines.append(f"| {link} | {method(r)} | {evidence(r)} | {fmt(r['old_error'])} | {fmt(r['previous']['max_error'])} | {fmt(maximum(r))} | {fmt(r['previous']['tolerance'])} | {fmt(r['tolerance'])} |")
lines += ['| bhatt | deferred | earlier independent reference retained separately | — | '+fmt(max(bhatt['measured'].values()))+' | '+fmt(max(bhatt['measured'].values()))+' | owned by bhatt-allfree | unchanged here |', '',
'## Final validation', '', '| Check | Machine | Result |', '|---|---|---|',
'| Independent reference re-verification | workstation | 98/98; all imported input digests match |',
'| Reference regression tests | workstation | 10/10 |',
'| Quality density/score/oracle harness | workstation | 99/99; explicit score/Hessian skips retained |',
'| Default-fit captures | workstation / simbox / WASM | all 99 families in every mode; R also retains one replicated-Lomax check |']
for k in required:
 cell=summary(k).replace("|",chr(92)+"|")
 lines.append(f"| `{' '.join(latest[k]['command'])}` | workstation | {cell} |")
lines += ['| `just cpp-test-no-long-double` | simbox | blocked before compilation: read-only filesystem |', '| Frozen JSON value/text checks; `git diff --check` | workstation | 98/98 frozen fixtures; bhatt byte-identical; clean diff |', '',
'Commands ran in the foreground under the shared CPU locks, at reduced priority, with at most four local workers and BLAS/OpenMP pinned to one thread. No started command remains running. See [checks.json](2026-11-09-golden-ref-port-data/checks.json) for exit codes and timings. No family README metadata/prose, fitter signatures, core fitter or binding fit implementations, settled d/p/q/r sources, or prohibited status/roadmap files changed.', '',
'## Open platform run and retained limitations', '',
'**Open: simbox no-long-double suite.** The initial simbox double-intermediate capture succeeded for all 99 families. During the later R build, simbox stopped with an I/O failure; repeated remote execution checks also reported `Input/output error`. A subsequent `nice -n 10 just cpp-test-no-long-double` attempt failed before compilation because the mirrored worktree filesystem was read-only (`os error 30` while creating `build/tmp/just-*`). The workstation no-long-double suite completed successfully. Missing evidence: a green no-long-double suite on simbox after its filesystem is writable again. The failure is retained in [simbox-unavailable.log](2026-11-09-golden-ref-port-data/simbox-unavailable.log). The failed simbox R capture was replaced by the successful workstation R capture, and is not presented as a successful simbox R run.', '',
'Bhatt’s reference/tolerance integration belongs to the explicitly excluded concurrent job. Triangular kink/global evidence and numerical multistart limitations remain explicit above. The tolerance increases and remaining default-fit errors are recorded for subsequent fitter work; this port changes no fitting defaults or algorithms.', '',
'Reproduction scripts and captures are in [the data directory](2026-11-09-golden-ref-port-data/). Run `verify_references.py`, regenerate the native/double/R/WASM captures (and simbox double capture when available), then `audit.py` and `apply_tolerances.py`; the latter changes only golden comparison allowances. `run_checks.py dev` and `run_checks.py full` run under `/tmp/umvl-cpu/run4`. The frozen-input audit uses read-only `git show HEAD:path` as its baseline.', '']
lines += ['**Artifact pane:** the display command returned HTTP 403 because this worktree is outside the viewer’s registered readable roots. Missing presentation setup: register this worktree in the viewer (or its `CODEX_MATH_WEB_VIEW_ROOTS`). No viewer configuration or files outside this worktree were changed.', '']
path=out.parent/'2026-11-09-golden-ref-port-report.md'
path.write_text('\n'.join(lines))
print(path)
