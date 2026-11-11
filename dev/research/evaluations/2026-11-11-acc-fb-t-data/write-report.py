"""Render the final report from completed, checked evidence; no fitter runs."""
import hashlib,json
from pathlib import Path

D=Path(__file__).resolve().parent;root=D.parents[3]
load=lambda name:json.loads((D/name).read_text())
s=load('final-summary.json');routes=load('route-contract-summary.json');inventory=load('route-inventory.json')
golden=load('golden-sstd-verification.json');signs=load('reference-sign-verification.json')
for p,digest in load('source-sha256.json').items():
    assert hashlib.sha256((root/p).read_bytes()).hexdigest()==digest,p
assert all(v['result']=='PASS' for v in s['tests'].values())
assert s['independent_cases']==687 and s['owned_with_supplement']==657
assert len(signs['checked'])==33
rel=D.name
link=lambda label,name:f'[{label}]({rel}/{name})'
fmt=lambda x:'—' if x is None else f'{x:.3g}'
parts=[]
parts.append('''# Student-type fallback accuracy and LST refusals — TASK-150.2.4

The owned uncertified routes in `std`, `lst`, `sstd`, `sged`, `st`, and `slash`
now finish with a coefficient-aware local accuracy check. The two requested
`lst` refusals have been replaced by real, locally checked searches when no
statistical certificate proves a stop. Public controls retain their names.

All required suites passed locally. Across 687 cases, the 657 owned
uncertified finite fits have maximum scaled coefficient error
**2.573e-12** in both native and double-only builds. The R/WASM spot maxima
are **1.304e-12** and **2.943e-12**. There are **no new stops**; 24 previously
refused cases now return accurate finite fits. Two accuracy issues remain
open under explicit scope/preservation constraints: 17 certified SGED replay
misses, and one unchanged GED-parent transport. They are detailed below.

Baseline: `bc8ff7be2e3bc4b9186a56fc4f2e8ee45c163714`. The worktree remains
uncommitted, as requested.

## Route inventory

The inventory follows public dispatch, result flags, and delegate calls in
all six families, including their certificate headers. “Before” describes
the baseline stopping rule, rather than whether a particular sample happened
to be accurate. Dedicated certificate routes that return finite `Ok` results
mark them certified; their non-`Ok` failures are not uncertified fits.
''')
parts.append('\n| Family / route | Trigger and baseline stopping rule | Contract before? | Final step |\n|---|---|---|---|\n')
for r in inventory['routes']:
    parts.append(f"| `{r['family']}`: {r['route']} | {r['trigger']}; {r['before']} | {r['before_contract']} | {r['after']} |\n")
parts.append('\n'+link('The machine-readable inventory','route-inventory.json')+
    ' also identifies every untouched dedicated certificate dispatcher.\n')
parts.append('''
The successful `sstd` and `st` common bindings omit the C++ certification
field; `slash` has no such member. A JSON `null` flag is therefore not an
observed `false`. The measurement harness conservatively checks these finite
rows too. An additional audit of the C++ dispatch found no hidden certified
rows in this corpus. All 21 explicitly certified replay outputs are
byte-identical before/after in each build mode.

## Local accuracy and mathematical evidence

The shared analytic second-order jet is packed over free coordinates only;
held values are differentiated as constants. The stopping rule requires
positive observed information and a mapped Newton correction `H^-1 g` no
larger than `tol/8` in any free public coefficient, followed by another
evaluated correction. Full-step contraction must be quadratic, or have
reached the practical arithmetic floor. Projected score-rounding checks and
public-coefficient representability checks guard ill-conditioned cases.
Objective decrease and score norms are search devices, not acceptance rules.
Refinement uses `min(requested_tol, 1e-10)`; solver/certificate work follows
the existing `iterlim` budgets.

The parameter maps include the offset in `nu-a`, the public skew coordinate,
and the ordinary-scale to standard-deviation conversion
`sigma_physical = sd * sqrt((nu-2)/nu)` for `std`. Natural shape coordinates
near one prevent a vanishing log-excess Jacobian or a likelihood tie from
silently replacing an interior root by the restriction. Shape scores are
rechecked after nuisance refinement. Very large residuals use logarithmic
kernel forms. Candidate-centered normalization avoids derivative underflow
from unrelated data/held-parameter units.

For SGED corners, let `p=nu`, `c` be an observed mode knot, and `a,b` be raw
left/right side scales. The independent reference uses

```
K = Gamma(2/p)/Gamma(1/p),  G = Gamma(3/p)/Gamma(1/p)
mean = c + K*(b-a)
sd^2 = G*(a^2+b^2-a*b) - K^2*(b-a)^2
xi = sqrt(b/a)
```

Production instead uses variance-scaled sides `A=a/lambda`, `B=b/lambda`,
where `lambda=sqrt(Gamma(1/p)/Gamma(3/p))` and `M=K*lambda`. This removes raw
gamma-moment overflow from the tangent problem. At `p=1`, raw mean and variance
reduce to `c+b-a` and `a^2+b^2`. Holds leave a smooth tangent solve or closed
forms. Strict left/right mode cost scores verify a shape-one corner; a free
shape additionally needs a positive right profile-cost score. For held
`0<p<1`, the positive `|mode-knot|^p` cost dominates smooth perturbations,
giving opposing infinite mode slopes. The tangent coefficients still need
their own Newton accuracy check. The unchanged all-free shape-one route uses
its existing knot enumeration and closed forms.

For `st`, CDF fraction convergence follows the actual df jet after packing,
including both derivative orders. Packed searches near the normal face allow
the stated multiplier-rounding floor; the final information-weighted score
check still controls coefficient accuracy. The final likelihood is evaluated
without unnecessary derivative axes and must be finite. In seeded case
33101 (held location 0, scale 1, alpha 2), native df improves from
999.994992094385 to 1000.000000000754; the independent root is
1000.0000000000064.

The `lst` subunit search uses 15 quantile/scale BFGS starts followed by the
analytic check, retaining the best checked likelihood. A searched witness
and its reflection may strengthen the existing symmetry proof by beating
the entire centered scale profile. Witnesses are not reported as maximizer
enclosures. At df at least one, failed joint inclusion retains the converged
and polished point uncertified. Likelihood witnesses use the rounded public
coefficients. Statistical atom/symmetry proofs and successful global
certificates keep their existing roles.

These are local certificates in exact arithmetic conditional on the computed
comparisons, with practical binary64 margins. They are not outward-rounded
interval proofs or new global-optimality certificates. The independent roots
below are numerical evidence, with separate derivative code and arithmetic.
''')
parts.append('\nImplementation: [shared jets](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/local_accuracy.hpp), '
    '[Student refinement](../../../univariateML-cpp/include/cppunivariate/distributions/lst/accuracy.hpp), '
    '[SGED corner chart](../../../univariateML-cpp/include/cppunivariate/distributions/sged/accuracy.hpp), '
    '[Student-CDF derivatives](../../../univariateML-cpp/include/cppunivariate/distributions/st/local_accuracy.hpp), '
    '[slash](../../../univariateML-cpp/include/cppunivariate/distributions/slash/mle.hpp).\n')
parts.append('''
## Independent adjudication and measurements

The main replay has 686 rows: all 121 requested held misses, all 462
uncertified finite rows from the 2026-11-08 Student coverage CSV (221 `lst`,
241 `std`), and 103 additional coverage rows. The CSV replay checks the exact
stored samples and held masks. A supplementary 500-observation all-free
`sstd` golden, seed 12791, closes coverage of the unchanged all-free route,
giving 687 cases in total.

Every returned free coefficient is compared with an independent 90-digit
mpmath stationary point, using direct densities and numerical derivatives.
References require positive information on their smooth free/tangent
coordinates and Newton correction below `1e-55` in normalized coordinates
(`1e-60` for knot tangents). Boundary nominations are not final evidence:
the completed audit checks the final one-sided scores for all 33 boundary/knot
rows. SGED references use raw sides, independently of production's normalized
chart. There are 679 finite reference cases; the eight existing stops have
no returned coefficient vector to compare.

The reported error is

```
E = max over free i of |theta_i - theta_i_reference|
                         / max(|theta_i_reference|, s_i).
```

Location floors are the fitted scale, except held-scale `lst`, which uses
the sample range (the supplied scale for constant data). `st` alpha uses
floor 1; positive coefficients use relative error. Under this metric, 117
of the 121 original held misses are confirmed. ST rows 7008, 7122, 7124,
and 7134 were already within `1e-10`. The 17 remaining certified SGED misses
are preserved and separated from the owned-uncertified statistics.

All 657 owned uncertified finite cases satisfy `E <= 1e-10` in native and
double-only builds. Twenty-four newly finite cases consist of two subunit
joint `lst` cases, 21 joint-inclusion fallback cases (including the translated
adjacent-ULP sample), and one `std` transport. The eight baseline stop indices
remain 31100, 32004, 32010, 32012, 34006, 34007, 34008, and 34009. They include
proved/retained statistical outcomes and three existing tiny-shape numerical
initialization failures; no new stop is introduced by refinement.
''')
parts.append('\n'+link('Per-row held adjudication','held-adjudication.json')+', '+
    link('all coefficient errors','coefficient-errors.json')+', '+link('final reference signs','reference-sign-verification.json')+
    ', and '+link('the all-free SSTD reference','golden-sstd-verification.json')+' retain the detailed evidence.\n')
parts.append('\n### Maximum scaled error by family\n\n| Family | Native before | Native after | Double before | Double after |\n|---|---:|---:|---:|---:|\n')
for f,v in s['by_family'].items():
    parts.append(f"| `{f}` | {fmt(v['before-native'])} | {fmt(v['after-native'])} | {fmt(v['before-double'])} | {fmt(v['after-double'])} |\n")
parts.append('''
### Maximum scaled error by route

Held names below use the public API. Corner and near-restriction routes are
split from smooth searches. “Refused” means that every baseline case in that
group stopped, so no before-error exists. The certified and inherited GED
exceptions are excluded and reported separately below.

| Route | Cases | Native before | Native after | Double before | Double after |
|---|---:|---:|---:|---:|---:|
''')
for name,g in sorted(routes.items()):
    vals=[]
    for mode in ['before-native','after-native','before-double','after-double']:
        value=g['modes'][mode]['maximum_scaled_error']
        vals.append('Refused' if value is None else fmt(value))
    parts.append(f"| {name} | {g['cases']} | "+' | '.join(vals)+' |\n')
parts.append('''
The 85 R and 85 WASM spot cases include the all-free SSTD supplement and
expected stops. Owned finite maxima are `1.304e-12` in R and `2.943e-12` in
WASM; the supplementary SSTD errors are `7.362e-15` and `9.239e-16`.
The explicit control mapping in the R driver preserves the existing names;
generated wrappers expose `x/...` rather than those names through `formals()`.
''')
parts.append('\n'+link('Public-interface measurements','spot-verification.json')+' and '+
    link('route summaries','route-contract-summary.json')+' provide the underlying values.\n')
parts.append('''
## Preservation and tests

In each build mode, all 21 certified replay rows are byte-identical, including
coefficients, likelihoods and iteration counts. All six bare-default golden
outputs are also byte-identical before/after. No golden samples or golden
estimates were rewritten. Dedicated certificate logic is unchanged apart
from allowing the new searched subunit LST witness to strengthen the existing
symmetry exclusion. That changes an old numerical refusal to a proved
nonuniqueness refusal, not a formerly successful fit to a stop.

New C++ cases are appended in all six families; R cases are appended for
LST subunit search and SGED shape-one/subunit contacts. Two existing LST
expectations are tightened: the symmetric subunit case now requires the
specific nonuniqueness proof and retains its witness checks; the translated
four-adjacent-ULP sample now requires an uncertified accurate fit, an analytic
scale reference and the actual rounded-point likelihood. No test was deleted
or weakened to accept a new refusal.

Family dossiers record only the relevant scale, shape/corner and transport
facts. No `core/`, settled d/p/q/r, `special/`, truth-table, codegen, NEWS,
status, known-issues or roadmap source was changed.

All final suites used the workstation. No simbox/GCC-13 pass is claimed:
the brief explicitly required local runs because simbox was unreachable.
Compute used the provided slot wrappers, at most four cores for this job,
with BLAS/OpenMP pinned and lowered priority. Wall times that include waiting
for shared reservations are not performance benchmarks.

| Final run | Result |
|---|---|
| `just cpp-test` | 107/107 |
| `just cpp-test-no-long-double` | 134/134 |
| `just js-build` | PASS |
| `just js-test` | 615/615; zero failures/skips |
| `just test-full` | 46,072 passes; FAIL 0, WARN 0, SKIP 0 |
| `just test` | 23,245 passes; FAIL 0, WARN 0, SKIP 154 |
| `just site-check` | PASS, including browser/static/live checks |
| Independent final-reference sign audit | 33/33 boundary/knot rows |
| Native/double, R/WASM coefficient and preservation checks | PASS, with the two explicit exceptions below |
''')
parts.append('\nThe additional repository-wide Markdown scan found 45 broken destinations in unchanged baseline files, including parser matches on C++ lambda syntax and missing local literature/site targets. The report and six changed dossiers have zero broken destinations. '+link('The baseline and scoped link audit','baseline-link-audit.json')+' verifies that all 45 source files/lines are inherited; unrelated files were left untouched.\n')
v=s['versions']
parts.append(f"\nToolchain: {v['cpp']}; {v['R']}; Node {v['node']}; Python {v['python']}; mpmath {v['mpmath']}; {v['wasm']}.\n")
parts.append('\n'+link('Final machine-readable summary','final-summary.json')+', '+
    link('test-run record','test-runs.txt')+', '+link('certified-output comparison','certified-preservation.json')+
    ', and '+link('source hashes','source-sha256.json')+' record provenance.\n')
parts.append('''
## Open items under explicit constraints

1. **Certified SGED: 17 coefficient misses remain unchanged.** Their maximum
   scaled error is `7.979474832513266e-10` in both builds. The fixed-shape/skew
   certificate ignores the requested `tol` and uses mode/public enclosures
   that are too coarse for `1e-10`. In case 6170, the returned mean is
   `-1.8564032985102169e149`, versus the independent
   `-1.8564033054720791e149`, with scale about `8.7247123865106e149`.
   This is beyond rounding. Fixing it requires a tol-aware certificate/root
   refinement and permission to change certified coefficients, conflicting
   with this brief's preservation requirement. The numerical root and
   affected rows are recorded; they were not relabelled uncertified to hide
   the error.

2. **Unchanged GED delegation: case 31101 fails the local contract.** Data are
   `(0.7321666961121761, 2.2766042947262384, -0.5491691631533211)`, with held
   `sd=1.0178559902092952` and `xi=1`. Parent and child return
   `nu=27.497341303893556`; the independent stationary value is
   `27.4973366168316843`, a relative error of `1.704551221489073e-7`.
   Mean error is only about `2.4e-15`. The required fix belongs to GED's
   held-scale/free-location/free-shape accuracy step in sibling job
   `acc-fb-cont`. This job preserves the exact parent transport, as explicitly
   instructed, and leaves GED files untouched. The added transport regression
   and the independent counterexample make the needed parent change concrete.

These are the only measured remaining coefficient misses: neither is counted
as an owned repaired fallback success. Both remain visible in the saved
verification data.

## Reproduction

From this worktree, the data directory's `reproduce.sh` extracts the baseline
headers/common bindings with `git archive` into ignored build scratch, builds
native/double capture binaries, and runs the root/comparison/preservation
checks. No source archive is kept in evaluation data. `cases.json`,
`inputs.txt`, `stationary.jsonl`, before/after captures and golden captures are
the frozen main inputs/results. `run-tests.sh` records the required suites.
Run `final-scientific-checks.sh` through a single `/tmp/umvl-cpu/run` reservation;
it contains no nested reservations or background jobs. It regenerates the
public-interface captures, final sign audit and supplementary SSTD root.
`finalize-evidence.py` and `write-report.py` collect the completed results and
render this document. All inputs, scripts and results remain in this
worktree; no other worktree was changed.
''')
path=D.parent/'2026-11-11-acc-fb-t-report.md'
path.write_text(''.join(parts))
print(path)
