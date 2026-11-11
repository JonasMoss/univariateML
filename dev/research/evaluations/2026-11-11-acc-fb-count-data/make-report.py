"""Write the final report only from completed green checks and references."""
import csv,json,pathlib
P=pathlib.Path(__file__).resolve().parent
load=lambda name:json.loads((P/name).read_text())
A=load('assessment.json');T=load('final-checks.json');B=load('binding-checks.json');S=load('spot-checks.json')
assert len(T)==7 and all(r['exit_code']==0 for r in T.values())
assert len(B)==5 and all(r['exit_code']==0 for r in B.values())
assert all(z['n']==25 for z in S.values())
assert all(not A[m]['contract_failures'] for m in ['native','double'])
assert load('final-audit.json')['scope_check_passed']
link=lambda file:f'[ {file} ](2026-11-11-acc-fb-count-data/{file})'
fmt=lambda x:'0' if x==0 else f'{x:.4g}'
inv=list(csv.DictReader((P/'inventory.csv').open()))
inventory='| Route | Trigger | Previous local rule | Met contract? | Final rule |\n|---|---|---|---|---|\n'
for r in inv:
 inventory+='| '+ ' | '.join([r['family']+': '+r['route'],r['trigger'],r['previous_local_rule'],r['contract_before'],r['final_rule']])+' |\n'
summary='| Route | Rows | Native before → after | Double-only before → after |\n|---|---:|---:|---:|\n'
D={r['route']:r for r in A['double']['summary']}
for n in A['native']['summary']:
 if not n['n']:continue
 d=D[n['route']]
 summary+=f"| {n['route']} | {n['n']} | {fmt(n['max_before'])} → {fmt(n['max_after'])} | {fmt(d['max_before'])} → {fmt(d['max_after'])} |\n"
N={r['index']:r for r in A['native']['rows']};D={r['index']:r for r in A['double']['rows']}
cases=load('cases.json')
strict='| Case / route | Native before → after | Double-only before → after |\n|---|---:|---:|\n'
for i in sorted(N):
 n=N[i];d=D[i]
 if n['tol']>=8*2.220446049250313e-16 or 'after' not in n:continue
 strict+=f"| {i}: {cases[i]['sample']} | {fmt(n['before'])} → {fmt(n['after'])} | {fmt(d['before'])} → {fmt(d['after'])} |\n"
stops='| Case | Family / limiting coefficient | Smallest possible scaled binary64 error |\n|---|---|---:|\n'
names={1059:'bbinom: free shapes',1060:'bbinom: beta',1061:'bbinom: alpha',1069:'ztbinom: prob = 2/3',1070:'zmbinom: p0 = 1/3',1071:'zib: prob = 2/3',1072:'zib: pi = 1/3',1262:'zib: pi = 3/7'}
for i,name in names.items():
 assert 'after' not in N[i] and 'after' not in D[i]
 stops+=f"| {i} | {name} | {fmt(max(N[i]['nearest_double'].values()))} |\n"
labels={'cpp-test':'107/107 dev shards','cpp-test-no-long-double':'134/134 shards','test':'23,257 passes; FAIL 0, WARN 0, SKIP 154','test-full':'46,084 passes; FAIL 0, WARN 0, SKIP 0','js-build':'build passed','js-test':'615/615 tests; no failures or skips','site-check':'passed; 99 family pages and 99 C++17 snippets, links, MathML and browser checks'}
tests='| Command | Machine | Result | Elapsed seconds |\n|---|---|---|---:|\n'
for name,r in T.items():tests+=f"| `just {name}` | workstation | {labels[name]} | {r['seconds']:.1f} |\n"
body=r'''# Count-family uncertified fallback accuracy — TASK-150.2.5.1

Completed on 2026-11-11 in `codex/acc-fb-count`, against baseline
`bc8ff7be2e3bc4b9186a56fc4f2e8ee45c163714`. No commits, merges, pushes,
package installations, board edits or simbox runs were made.

The nonconforming fallback stopping rules now have a final check in public
coefficient coordinates. The substantive default-tolerance error was a nearly
flat held-inflation ZIB root: its scaled probability error fell from
2.1409e-6 to 9.1792e-13 in both precision modes. An exactly flat example fell
from 1.5311e-6 to 4.4409e-17. The other revised routes now justify their local
accuracy even where the previous estimates happened to be accurate.

Every returned fit with an independent reference passes its requested
tolerance: 188 returned reference rows per precision mode. Eight formerly
successful rows now report numerical-accuracy failure, all at `tol = 1e-18`
with nonrepresentable maximizers. There are no new default-tolerance stops.
The original 911 certified count outputs remain text-identical; all 913
certified double-only baseline rows, including two additional controls, also
remain text-identical. Both extra native certified controls are text-identical.

**Route inventory.** This inventory follows public return paths, explicit
flag assignments, restored stop witnesses, and delegate calls. Several
certificate exits share one final local solver and are grouped accordingly.
“Yes” concerns local coefficient accuracy, not a global comparison. The
machine-readable inventory is [inventory.csv](2026-11-11-acc-fb-count-data/inventory.csv).

@@INVENTORY@@

The BNB scalar rescue covers whichever of its three continuous coordinates
is free. Its all-free fallback includes an unclosed signed face comparison
and an unclosed joint cover; both pass through the existing final analytic
polisher. Held-alpha pair certification is included below as an unchanged
control, but is not an additional uncertified route. The all-held BNB result
has a false flag by default and no free coefficient.

Skellam has no public `certified` result member on this baseline. Its internal
tail-exclusion flag being false leads to `IterationFailure`, not an
uncertified `Ok`. An earlier harness annotation that called all successful
Skellam fits uncertified was not a library flag. This study labels them
`unflagged`, checks the successful joint and held-rate fits independently,
and leaves Skellam unchanged.

**Final local checks.** The changes are confined to bbinom, tbinom, ztbinom,
zib and zmbinom, their binding failure mapping, dossiers and regression tests.
Global face comparisons and integer searches retain their previous logic.
Refinement follows a successful uncertified search; it does not reject an
ordinary finite candidate merely because its global certificate is missing.
Sub-machine closed reductions also pass through a rounding-accuracy gate.
Existing controls and default golden data are unchanged.

For **bbinom**, fix the integer size N returned by the existing search. With
A_j = #{x > j}, C_j = #{x < N-j}, and n observations, the natural-coordinate
scores are

    g_alpha = sum_j [ A_j/(alpha+j) - n/(alpha+beta+j) ]
    g_beta  = sum_j [ C_j/(beta+j)  - n/(alpha+beta+j) ].

The diagonal observed-information entries replace each reciprocal by its
square; the off-diagonal entry is `-sum_j n/(alpha+beta+j)^2`.
The new helper evaluates these rational sums in two-component binary64
arithmetic, checks positive information (and the determinant for two free
shapes), and checks every component of `I^-1 g` in the returned natural
coordinates. It tracks contraction across corrections and a forward-noise
allowance with gamma = `128 (N+1) epsilon^2`. The information perturbation
check requires its computed relative conditioning allowance to be below 1/8.
Ordinary acceptance uses a factor-16 tolerance margin. A rounded stagnating
correction is accepted only when its full checked error is within tolerance;
a small objective change or gradient norm is never the criterion.

For symmetric size-two and size-three histograms, reflection removes the
mixed derivative and the total-shape profile reduces to a count ratio:
`alpha = beta = c1/(2 c0-c1)` for N=2, or `2 c1/(3 c0-c1)` for N=3.
The finite positive solution is a local maximum; the contrast curvature is
negative. These exact reductions include the near-binomial shape 1874.5,
without relying on an ill-conditioned log-shape solve. At N=1 with one held
shape, the remaining shape is another closed count ratio. Asymmetric shapes
still use the general information calculation; the `(2,3)` regression is
not a symmetry shortcut. The returned log likelihood is recomputed after
refinement. Certified ordinary paths bypass this helper.

For **tbinom**, the conditional binomial mean is strictly increasing in p
on a nonsingleton truncation support: its derivative is the positive
conditional variance divided by `p(1-p)`. The new final bracket evaluates
the mean relative to an integer mode instead of subtracting two large
means. Mode-relative probability recurrences have a geometric first-moment
tail bound. Their score margin is
`128 epsilon_ExtendedReal (1 + |centred target| + absolute moment / mass)`.
Bracket acceptance uses relative width `tol/32` and verifies padded opposite
signs around `p ± tol*p/16`. The selected integer size and observed
truncation endpoints stay exact. Its existing public fixed accuracy and work
allowance are retained; no controls were added or renamed.

For **ztbinom**, the positive conditional mean is
`N p / [1-(1-p)^N]`. It is likewise monotone. A centred mean-excess formula
removes cancellation near its lower limit 1. Ordinary acceptance uses a
relative `tol/32` probability bracket and a final sign check padded by
`64 epsilon_ExtendedReal (|target|+|left value|+|right value|)`.
The bracket starts near the successful search point. For stringent requests,
two-component rational scores retain an internal root and a forward-error
estimate before the public coefficient is rounded. Exactly dyadic identities
and closed count ratios are checked directly. The native and double-only
implementations therefore do not require long double for sub-machine
representable targets.

For **ZIB with held inflation**, clear positive score denominators to obtain

    F(p) = pi (S-k N p) + (1-pi) (1-p)^N (S-n N p),

where S is the total count and k is the number of positive observations.
Two-component evaluation, including a separately scaled power, supplies
signs for a probability bracket on the branch selected by the existing
search. Its practical sign pad is proportional to
`128 (N+1) epsilon^2` times the scaled term magnitudes. An unresolved sign
cannot masquerade as an exact zero. At the exceptional flat critical root,
bounded integer identities establish the closed rational root before its
rounding residual is checked. For counts `(20 zeros, 2 ones, 13 twos)`,
N=2 and pi=7/32, this gives p=4/5 despite zero quadratic curvature.

For **free-inflation ZIB**, the positive-count root is refined first and
`pi = 1-k/[n B(p)]`, where `B(p)=1-(1-p)^N`, is evaluated with `expm1`.
Concavity and B(0)=0 imply `p B'(p)/B(p) <= 1`, so a relative probability
bound controls the absolute inflation error in the interior. Stringent
requests retain the internal two-component root through this transformation
and check each public rounding separately, with a noise term
`128 (N+1) epsilon^2 / B(p)`. Active pi=0, no-zero samples, p=1 and N=1
use their corresponding closed reductions. In particular, N=1 with both
p and pi free has an MLE curve: only `(1-pi)*p` is identified. The stringent
case with empirical mean 1/3 chooses a feasible p just above that mean and
resolves pi on the same exact MLE curve; it does not falsely refuse a
nonidentified maximum merely because 1/3 is not representable.

For **zmbinom**, the parent ztbinom probability and accuracy status are
transported unchanged. Its independent zero-mass estimate is the empirical
zero fraction, with a compensated product residual checked against the full
requested tolerance. Each public coefficient has its own accuracy check.
The four common binding adapters map an accuracy failure to the existing
numerical condition and retain a finite candidate witness.

These arguments prove the local reductions, monotonicity implications and
coefficient transformations in exact arithmetic. Sign and information claims
are conditional on the computed comparisons with the stated practical
floating-point margins. They are not outward-rounded interval proofs.
The 80-digit comparisons below are independent numerical evidence; they
are not new global-optimality certificates. Work loops use the existing
solver/evaluation/enumeration allowances; there is no new fixed search cap
that overrides an existing user iteration control.

**Replay and reference method.** The kept
[cases.json](2026-11-11-acc-fb-count-data/cases.json) contains 1,267 rows:
the original 1,073-row count replay (including its 88 uncertified rows), five
directed BNB probes, sixteen Skellam cases from seed 150251, the 151
November 9 BNB uncertified rows, and 22 further directed precision probes.
The selected before/reference dataset has 196 successful reference rows per
mode, including exact integer and all-held controls. There are no failed
oracle solves. After refinement, 188 of these rows return fits and eight
report the binary64 accuracy failures listed below.

[oracle.py](2026-11-11-acc-fb-count-data/oracle.py) uses mpmath at 80 digits.
It differentiates log-gamma likelihoods independently for BB/BNB and the
Bessel likelihood for Skellam; it does not import production derivatives.
Truncated binomial references use direct high-precision probabilities or
incomplete beta differences. ZT probabilities use the high-precision
conditional mean, inflation/zero fractions use their exact profiles, and
the flat ZIB cubic is handled directly. Binary64 inputs are converted
exactly, rather than being silently replaced by rounded decimal inputs.

Every free coefficient is assessed by
`abs(estimate-reference)/max(1,abs(reference))`; held values must remain
bit-identical. Integer candidates are exact selected coordinates. The zero
hyper/nhyper errors below record those integer identities, not an independent
proof of the global integer winner. BNB's `size` is continuous and is
included in the numerical coefficient comparison. All other selected sizes
in the native/double replay are checked exactly against the reference size.

The before executables were rebuilt from `git archive bc8ff7be` under
`build/scratch/baseline`; no source tree archive is kept in evaluation data.
The original count replay's 29 relevant source hashes match that baseline.
The final native/double replay refreshes every changed-family row. BNB and
Skellam rows retained from the matching earlier captures have identical
payloads, backed by unchanged source/dependency hashes. Full final suites
also exercise those families. The 151 old BNB rows contained 77 finite fits
on November 9; the supplied baseline already has 71 finite fits, 74 face
outcomes and six numerical-accuracy failures. Those six predate this job.
All 158 compared BNB payloads and all sixteen Skellam payloads remain
unchanged in each precision mode.

The following maxima include ordinary requests and the 1e-14 BB certified
control (`tol >= 8 epsilon_binary64`). They exclude the nineteen 1e-18
requests, which are reported separately. The BNB held-alpha row is the
certified pair control. “Before → after” gives the largest scaled error
over every free public coefficient in each route.

@@SUMMARY@@

The eleven representable or nonidentified-curve 1e-18 requests still return
fits. Their largest scaled errors are:

@@STRICT@@

The tiny residual for the exactly represented BB `(alpha,beta)=(2,3)` is
the independent numerical root solver's residual (below 2e-61); the returned
coefficients themselves equal 2 and 3 exactly. These cases prevent the
accuracy gate from becoming a blanket rejection of requests below machine
epsilon. The two N=1 ZIB cases also distinguish a held-parameter root from
the freely selectable MLE curve.

The eight new numerical failures all request 1e-18. Even the closest
binary64 coefficient misses that request:

@@STOPS@@

The rational distances are exact count-ratio rounding residuals. The three
BB distances use the independent 80-digit stationary points and the
production information/contraction checks with their practical margins;
they are numerical evidence conditional on those comparisons, rather than
outward-rounded enclosures. For the two-free-shape case the table shows the
larger unavoidable scaled coordinate error. Existing sub-machine tests that
had required `Ok` at these seven original nonrepresentable roots were
tightened to require the numerical-accuracy outcome, finite witness and
unchanged held coefficients. No existing case was removed. The eighth
failure (pi=3/7) is a new directed probe. New C++ cases are appended to the
family test files; R and JS regressions check the binding behavior.

**Binding checks and preservation.** Twenty-five R and twenty-five WASM
fits cover all nine families, held and free routes, the flat roots, precise
ratios, the active boundary and the Bernoulli curve. All pass; the maximum
scaled error is 9.1792e-13 in each binding. Estimates are recorded with full
binary64 round-trip precision, including tiny inflation values.

WASM selects a different uncertified tbinom integer candidate for replay
row 317: size 11,224,310,574 rather than GCC's 1,000,011. Its global search
and integer candidate are not altered by the new final step. The binding
checker therefore solves the independent probability equation at WASM's
actual returned integer, rather than comparing it to a different conditional
model. Its probability error is 8.9057e-17. This is a local-accuracy check,
not a claim that the two machines agree on an uncertified global integer
winner. The full selected-size reference is kept in
[spot-checks.json](2026-11-11-acc-fb-count-data/spot-checks.json).
Recomputing that large-size reference at 120 working digits changes p by
only 1.219e-77 relatively, confirming more than fifty matching relative
digits; see [reference-stability.json](2026-11-11-acc-fb-count-data/reference-stability.json).

All 911 original native certified rows are text-identical, including their
coefficients and likelihood text. The 913 certified double-only baseline
rows are also text-identical. Both extra native certified probes are
text-identical as well. No default golden or
truth table was changed. The source audit verifies 58 unchanged files
across hyper, nhyper, bnbinom, skellam, core and special, in addition to the
old count replay source comparison. No changes to a parent outside this
job's families are needed. Dossier changes contain only the family-specific
accuracy reductions and exceptional cases described here.

**Completed final runs.** All runs below used the workstation, GCC 15.2
for native C++, and the existing local R/Emscripten/Node installations.
Simbox was not attempted, as required by the brief. Builds/tests were
foreground blocking children under `/tmp/umvl-cpu/run4`; the serial binding
and reference checks used `/tmp/umvl-cpu/run`, with `MAKEFLAGS=-j1` for the
R spot load. BLAS/OpenMP were pinned to one thread and jobs ran at nice 10.

@@TESTS@@

The seven checks all recorded the same family/common/R/JS-test source
fingerprint:

`42d7f973bc48d710ebd6fe2d35c59bdb80e010c0295147263617f074696803f4`.

No production or regression-test source changed after these runs began.
The later correction was confined to the study's binding checker, which
now conditions a WASM reference on its actual returned integer size.
All binding/reference checks and the final source/scope/diff audit passed
after that correction. Compiler diagnostics are retained in the raw logs;
the green build results do not mean warning-free C++ compilation. The R
warning counts above are zero.

Kept evidence consists of [assessment.json](2026-11-11-acc-fb-count-data/assessment.json),
[native references](2026-11-11-acc-fb-count-data/before-native-errors.json),
[double-only references](2026-11-11-acc-fb-count-data/before-double-errors.json),
[test receipts](2026-11-11-acc-fb-count-data/final-checks.json),
[binding receipts](2026-11-11-acc-fb-count-data/binding-checks.json),
[Markdown link check](2026-11-11-acc-fb-count-data/markdown-link-check.json),
[source audit](2026-11-11-acc-fb-count-data/final-audit.json),
[provenance](2026-11-11-acc-fb-count-data/provenance.json) and
[file hashes](2026-11-11-acc-fb-count-data/final-manifest.json), together with
the before/after TSVs, case data and reproduction scripts in the same data
directory. Raw logs live under `build/scratch/acc-fb-count-logs/` in accord
with the worktree scratch rule. The site check's unrelated historical PNG
capture drift was restored to its baseline bytes. The final diff is confined
to the owned family/binding/test files and this evaluation evidence.

There are no open implementation items from this brief. The global
comparisons intentionally left uncertified remain global questions; they
do not withdraw the checked local coefficient-accuracy claim.

**Open presentation item.** The requested `bin/show` invocation returned
HTTP 403: the running viewer's readable-root list omits this worktree.
The report exists at its requested path. Displaying it in the pane requires
registering `/home/jonas/Files/programming/univariateML.wt/acc-fb-count` as
a viewer-readable root (or opening a viewer thread there). This is a viewer
configuration issue outside the worktree; it does not affect the completed
implementation or validation. The diagnostic is recorded in
[presentation.json](2026-11-11-acc-fb-count-data/presentation.json).
'''
for key,value in [('INVENTORY',inventory),('SUMMARY',summary),('STRICT',strict),('STOPS',stops),('TESTS',tests)]:body=body.replace('@@'+key+'@@',value.rstrip())
report=P.parent/'2026-11-11-acc-fb-count-report.md'
report.write_text(body)
print(report)
