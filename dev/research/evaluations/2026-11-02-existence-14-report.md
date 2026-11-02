# existence-14: completed dossier review

2026-11-02 — TASK-101.6.4.1. Worktree `existence-14`, branch
`codex/existence-14`, source revision
`9578745158d3d15c210707f34a7ce83f37b4296e` plus the six dossier edits described
below. All required checks have finished. No commits, board edits, dependency
installs, sub-agents or simbox runs were made.

The changes add `existence` statements, explain coefficient meanings and held
cases, qualify limit conditions, and separate R usage and implementation history
from statistical prose. Existence statements concern nonempty finite samples,
positive observations for power, valid held coefficients, and positive likelihood.
Mathematical arguments concern exact arithmetic; passing numerical tests does not
prove existence, uniqueness or global optimization.

Only the six owned `README.md` files and this report are authored changes.
Protected metadata, statuses, held sets, fitting bounds and exemptions, limit
firmness/numerical flags, defaults, examples, densities and goldens are unchanged.
The YAML comparison against HEAD checked this explicitly. No adapter contains
the old Huber advice as a literal string, and no owned R test matches it, so no
adapter or regex edit was needed. The generated required-argument diagnostic
uses the revised advice and passed conformance tests.

## Family results

**unif — statement:** With both endpoints free, a unique finite MLE exists
exactly for nonconstant samples; compatible holds give the unique smallest
positive-width interval, with all-held fits evaluating compatibility.

**Grade: proved; `uniqueness: proved`.** On a containing interval, likelihood is
`(b-a)^(-n)`, strictly decreasing with width. Every free endpoint is therefore
its sample extreme. For constant data a held exterior endpoint prevents
collapse; without one, free width can tend to zero and likelihood diverges.
The original incompatible-endpoint record incorrectly included invalid ordering,
which the fitter reports as `InvalidInput`, not `Incompatible`. The record now
covers exclusion of observations, and R notes retain the invalid-ordering rule.
**Status changes: none. Open mathematical questions: none in this domain.**

**dunif — statement:** Every nonempty integer sample has a unique free-endpoint
MLE; compatible held endpoints leave each free endpoint at its sample extreme.

**Grade: proved; `uniqueness: proved`.** Likelihood is `(b-a+1)^(-n)` and strictly
decreases with the number of integers in the interval. A singleton interval is
an admissible point mass, so constant samples attain their maximum. Both held
endpoints leave only compatibility evaluation. **Status changes: none.
Disagreements found: none. Open mathematical questions: none in this domain.**

**power — statement:** The joint MLE is unique for nonconstant positive data;
with endpoint held, free shape exists exactly when some observation is below
that endpoint; held shape permits constant and singleton samples.

**Grade: proved; `uniqueness: proved`.** At every positive shape, likelihood
strictly decreases with the upper endpoint. At the minimizing endpoint, the
shape log-likelihood is `n log(b) - b S` plus a constant, with
`S = sum(log(a/x))`. Its unique maximum is `b=n/S` when `S>0`; when `S=0`,
likelihood is unbounded as shape increases. A held endpoint below an observation
is incompatible and gives zero likelihood. **Status changes: none.**

**Disagreements/open metadata:** The existing `limits` list records only
zero-likelihood support failure, not the implemented `Unbounded` shape escape.
Adding another record is outside this job's ownership of existing limit text.
The protected `r.control.epsilon` text also says the endpoint is exactly the
sample maximum without qualifying that it is free. It should gain that
qualification in a later authorized metadata edit. The existence statement and
prose distinguish both held and free cases correctly. No mathematical existence
or uniqueness question remains for the stated positive finite domain.

**triang — statement:** Every compatible nonconstant sample attains a maximum;
constant data collapse exactly when every held value equals the observation;
maximizers need not be unique.

**Grade: proved for existence and the nonuniqueness counterexample;
`uniqueness: unknown` makes no universal uniqueness claim.** Density is at most
`2/(b-a)`. Sample spread, or a held value away from a constant observation,
bounds width away from zero. Likelihood tends to zero as width diverges, while
support containment bounds endpoint positions at bounded width. Upper
semicontinuity under the endpoint-mode convention gives attainment. In the
remaining compatible constant case, shrinking width proves divergence.
For observations `(0,1)`, the reflected maximizers `(a,b,c)=(0,1.5,0)` and
`(-0.5,1,1)` both have likelihood `16/27`. Convexity in mode between observations
reduces this example to the observed modes; at mode zero, maximizing
`(b-1)/b^3` gives `b=1.5`. Both held-mode fits were also checked numerically
against these coefficients and likelihood, within `1e-8`.

**Status changes: none. Disagreements:** Invalid ordering was previously grouped
under `Incompatible`, although the fitter uses `InvalidInput`; the limit text
and R notes now separate these cases. **Open metadata/certification question:**
The protected `fit.method: multistart` and partial-certification description are
conservative descriptions of code that now enumerates observation modes and
uses concave endpoint profiles and interval pruning bounds. Assessing/updating
that catalog classification requires a separate authorized certification review.
The existence proof does not depend on that numerical classification.

**tlambda — statement:** Unrestricted free-shape likelihood is unbounded; on
`-1 <= lambda <= 1`, strict tie multiplicities below half the sample give
attainment; held-location collapse and held-positive-shape support constraints
require separate treatment, and general conditional uniqueness is unknown.

**Grades: proved for the unrestricted spike path and stated collapse exponent;
argued for fitted-branch compactness; unknown for general conditional existence
and uniqueness. `uniqueness: unknown`.** With location at an observation and
`sigma=lambda*A`, likelihood grows exponentially in positive lambda at that
observation and only loses polynomial factors elsewhere. On the fitted branch,
the uniform bounds on standardized density and its squared-distance product
bound a collapse path by a constant times `sigma^(n-2k)`. Together with vanishing
likelihood at diverging scale/location and upper semicontinuity, this gives the
stated compactness argument. The dossier retains the uniform aliases, excluded
U-branch junction example, and fixed-negative-shape exponent
`(n-k)/abs(lambda)-k`.

**Status changes: none. Disagreements corrected in prose:** The collapse record
now describes the zero-exponent held-location case reported as `CollapseLimit`,
instead of loosely conflating every collapse outcome. The old generic
score-rescue sentence did not describe this fitter: it uses simplex diameter
and likelihood-spread convergence, and rejects a competitive unresolved start.
R notes now state those actual rules.

**Open:** A complete conditional existence classification outside the declared
branch and a global certificate for the multistart search remain unavailable.
The structured limits still lack a separate `Unbounded` record for negative
collapse exponents or free shape with a tie at held location. The default tie
guard is sufficient, not necessary, and can reject samples whose MLE exists.
These facts are explicit rather than inferred from optimizer success.

**huber — statement:** With all coefficients free, finite attainment is
data-dependent and can fail at the normal or Laplace limits; with shape held,
compatible nonconstant samples attain a maximum, with the stated constant-data
collapse exceptions and possible intervals of maximizing locations.

**Grades: proved for the fixed-shape attainment argument and named limit paths;
numerical for the implemented free-shape classification; unknown for general
free-shape uniqueness. `uniqueness: unknown`.** With inverse scale `eta` and
scaled location `nu`, the objective is `n log(eta) - sum(rho(eta*x-nu))` plus a
constant. Convex Huber loss gives concavity. Its linear-tail lower bound makes
positive level sets bounded for nonconstant data, or with location held away
from constant data; holding scale also prevents collapse. Linear tails can
leave an interval of maximizing locations. The Laplace path has scale
`b=sigma/epsilon` and requires free sigma; the normal path requires free shape
but permits held location or scale. The linked
[free-shape derivation](2026-10-29-huber-free-epsilon-report.md) proves the
strict normal-limit comparison for two distinct observations.

**Status changes: none. Disagreements corrected:** The scale-collapse detector
now explicitly requires free scale. The prose distinguishes proved limit paths
from numerical decisions at tolerance `1e-8*n`: a limit diagnostic does not
prove exact nonattainment and can discard a finite improvement below tolerance.
**Open:** No general exact data criterion or uniqueness theorem is claimed for
free shape. The grid and local refinements lack a global certificate excluding
an undetected interior peak. The existing partial-certification flag is retained.

No external theorem citation was added or used in place of these arguments.
Existing bibliographies are unchanged; web searches used: zero.

## Validation

All runs used this workstation through `/tmp/umvl-cpu/run` or `run4`, under
`nice -n 10`, with BLAS/OpenMP pinned to one thread per worker and at most four
slots. Slot waits were allowed to finish. All commands have exited.
Temporary directories were `build/tmp`; scratch files/logs were under
`build/scratch`. No fixtures or simulation seeds were regenerated. The extra
triangular check is deterministic.

Environment: Linux 7.0.0-38-generic x86_64; GCC 15.2.0; R 4.6.1
(2026-06-24), BH 1.90.0.1, testthat 3.3.2; Python 3.14.5, NumPy 2.4.6,
SciPy 1.18.0, SymPy 1.14.0, mpmath 1.3.0 and PyYAML 6.0.3.

| Check | Result |
|---|---|
| `/tmp/umvl-cpu/run4 just regen` | Passed; final `just test` also regenerated help from the final dossiers. |
| Focused `testthat::test_local` filter below, under `run4` | FAIL 0, WARN 0, SKIP 4, PASS 8179. All four skips require the full tier. |
| Supplied all-page `tools::checkRd` loop, excluding Non-ASCII diagnostics | No diagnostics for any owned family. Nine existing diagnostics occur only in `man/mlskellam.Rd:50` (unknown macros and lost braces), outside this job's scope. That source was not edited. |
| Final explicit six-page `tools::checkRd` audit after regeneration | Passed, no diagnostics. |
| `python3 site/scripts/data.py`, under `run` | Passed initially and again on the final dossiers; derived 99 families. |
| `just relations`, under `run` | 157 edges, 0 failed. Density and relation metadata remained unchanged afterward. |
| YAML comparison against HEAD | Passed for all six; only permitted prose/advice/limit text and the new existence fields differ. |
| Requested `grep -lP '[\x00-\x08\x0b\x0c\x0e-\x1f]'` on the six dossiers | Printed nothing; no control characters. |
| `/tmp/umvl-cpu/run4 just cpp-test` | 106/106 dev shards passed; 0 failed. Test execution: 49.58 seconds, excluding lock wait/build. |
| `/tmp/umvl-cpu/run4 just test` | R dev: FAIL 0, WARN 0, SKIP 151, PASS 20121. The 151 skips require the full tier. |
| Triangular reflected maximizers below | Both coefficient vectors and log likelihoods agree within `1e-8`. |
| `git diff --check` | Passed. |

The focused test expression ran from `univariateML-r` with
`UMVL_DISTRIBUTIONS_DIR=$PWD/../univariateML-cpp/include/cppunivariate/distributions`:

```r
testthat::test_local('.',
  filter = 'dist-(unif|dunif|power|triang|tlambda|huber)|conformance|restricted|generated-dpqr|parameter',
  stop_on_failure = TRUE)
```

The additional example can be reproduced after loading the generated package:

```r
for (mode in c(0, 1)) {
  f <- suppressWarnings(mltriang(c(0, 1), c = mode))
  expected <- if (mode == 0) c(0, 1.5, 0) else c(-0.5, 1, 1)
  stopifnot(isTRUE(all.equal(as.numeric(f), expected, tolerance = 1e-8)))
  stopifnot(abs(as.numeric(logLik(f)) - log(16/27)) < 1e-8)
}
```

The full oracle tier was not run: this job changed no fitting, d/p/q/r or test
infrastructure code. Numerical comparisons and golden regressions support the
tested cases, not the unresolved global-search guarantees listed above.

## Artifact delivery

The report is saved at the requested worktree path. The prescribed `show`
command returned HTTP 403 because this worktree is absent from the viewer's
configured readable roots. The helper supports file paths only, so displaying
this file requires the viewer to recognize this worktree. Viewer configuration
and files outside this worktree were left untouched.
