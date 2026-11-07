# bnbinom alpha-held global certificate

TASK-116.34, 2026-11-07. Worktree bnbinom-alpha, branch codex/bnbinom-alpha,
base d34a44cd1b0d7324e1f486d66e6b3f7e3db9210a.
[Final source fingerprints](2026-11-07-bnbinom-alpha-data/source.json).

The alpha-held fit now returns a certified global maximizer in size and beta,
or a structured budget, numerical, or comparison-tie stop. It retains both
swap-related maxima and a singular-diagonal witness. The default all-free
fit and the other alpha-free joint searches retain their previous algorithms
and outputs. Family-wide **fit.certified** remains **partial**; **fit.note**
now names the added coverage.

## Coverage and implementation

A positive sample below contains at least one positive count; all observations
are nonnegative integers and all held shapes are positive and finite.

| Free coefficients | Positive-sample coverage |
|---|---|
| size | Existing unique scalar global maximum certificate |
| alpha | Existing unique scalar global maximum certificate |
| beta | Existing unique scalar global maximum certificate |
| size, beta; alpha held | New global certificate; canonical representative, swapped maximum, and diagonal witness retained |
| alpha, beta; size held | Unchanged uncertified joint search and NB-face comparison |
| size, alpha; beta held | Unchanged symmetric joint search and NB-face comparison |
| size, alpha, beta | Unchanged uncertified joint search and NB/Poisson comparisons |
| None | Direct likelihood evaluation, excluded from fitting certification |

All-zero samples with any free coefficient retain the exact point-mass
supremum stop and Poisson(0) details. No structural hold exception was added.

The implementation is in
[bnbinom/mle.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/bnbinom/mle.hpp).
Routing and stop classification are in the
[binding-neutral family header](../../../bindings/common/distributions/bnbinom/fit.hpp),
shared by R and JS. Certificate failures are terminal. They never return an
optimizer endpoint as an estimate.

Successful core results retain finite witnesses, isolated roots and their
swapped labels, and the diagonal witness in **stop_detail**. Common binding
results also expose **swap_maximum** and **diagonal_witness**, in coefficient
order (size, alpha, beta). These appear in the internal R adapter list and
the public JS result. The public R constructor keeps its existing estimate
attributes. Public R and JS failures expose the structured candidates and
global likelihood bounds.

Known swap symmetry has an exact identity behind it, so the fitter returns
the representative with size no greater than beta. Distinct identified roots
whose likelihood intervals overlap produce **ComparisonTie**, exposed as a
**tie** condition.

## Proof and numerical contract

The canonical derivations are in the
[alpha-held certificate section](../../../univariateML-cpp/include/cppunivariate/distributions/bnbinom/README.md#alpha-held-pair-certificate).
The argument uses the previously proved scalar single-crossing theorem and
the strictly negative mixed size/beta derivative.

A finite witness supplies a lower likelihood bound. Small-shape mass bounds
give an effective positive lower cutoff. A resolved negative size score at
(upper, alpha, lower), together with scalar single crossing and the negative
mixed derivative, excludes stationary maxima beyond the upper cutoff in
either coordinate. Clipping a large coordinate inward increases likelihood.
Consequently every global maximum lies in the resulting finite stationary
rectangle. The dossier also derives full compact-superlevel cutoffs using
a finite-count CDF bound.

The certificate covers the stationary rectangle in log coordinates. Monotone
mass-ratio bounds exclude boxes below a witness; scalar conditional-mode
signs exclude boxes without stationary points. Boxes crossing the diagonal
remain in the cover. Local optimizers propose centers. Interval Hessian
bounds, a contraction mapping and strict negative definiteness establish
a unique maximum in each accepted root neighborhood. A second, narrow
contraction enclosure establishes relative coefficient accuracy, with radius
one quarter of the smaller of the requested tolerance and 1e-8. The cover
must close before any root becomes a successful estimate.

Root likelihood bounds include both the evaluated-center error and a Taylor
bound for variation over the coefficient enclosure. Every isolated contender
is compared. A diagonal scalar solve supplies a diagnostic witness; its
selection as an MLE requires the same two-dimensional checks and global
coverage as an off-diagonal root.

The implementation is **certified in exact arithmetic, conditional on the
computed comparisons, with a stated practical floating-point margin**.
It does not claim outward rounding. Scores use 128 times double epsilon
times the absolute digamma-term sum. Hessian and log-gamma-ratio evaluations
use 512 times double epsilon times their accumulated absolute terms.
Likelihood bounds additionally use 1e-10 times n times (1 + absolute
average log likelihood), and root bounds include coefficient-enclosure error.

Proposals, cutoff expansions, diagonal witness evaluations and covering boxes
share 128 times min(iterlim, 100000) certificate evaluations. Each nested
optimizer, Newton refinement and contraction-radius search has
min(iterlim, 100000) solver steps. Exhaustion stops with budget details.
Unresolved arithmetic or representability stops numerically. Diagnostic
candidates remain witnesses rather than estimates on any stop. The global
upper bound is infinite before the stationary rectangle closes, zero while
its cover remains unresolved, and a root-likelihood upper bound after closure.

The dossier also corrects the size-score tail coefficient to negative
n times beta divided by size; beta is symmetric. Its sign, and the earlier
scalar uniqueness conclusion, are unchanged.

## Independent numerical evidence and nonuniqueness

The [80-digit oracle](2026-11-07-bnbinom-alpha-data/oracle.py) uses direct
gamma/digamma expressions and retains
[four roots and cutoff checks](2026-11-07-bnbinom-alpha-data/oracle.json).
It also checks 400 sampled mass-box inequalities. These comparisons support
coefficient and bound accuracy; the global argument comes from the proved
exclusions and completed cover.

On the frozen 500-count sample with alpha held at 2, the certified canonical
maximum is approximately

| Quantity | Value |
|---|---:|
| size | 1.1509739520206237 |
| beta | 4.5037452914721273 |
| Log likelihood | -1275.7208277024461 |
| Best diagonal size = beta | 2.2046081607133645 |
| Diagonal log likelihood | -1276.6511150150281 |

Swapping size and beta gives a distinct labeled global maximum. Labeled
uniqueness therefore fails. The earlier independent
[diagonal-exclusion reproducer](2026-11-06-bnbinom-cert-data/nonuniqueness.py)
was rerun successfully: 861 segments, 431 accepted, 60-digit arithmetic and
a 1e-20 comparison margin. Its
[retained output](2026-11-07-bnbinom-alpha-data/nonuniqueness.json)
excludes every diagonal point below the finite witness (1, 2, 5).
Compactness and symmetry then establish nonuniqueness independently of
a local optimizer.

For this sample, the independently checked full superlevel box has lower
cutoff 1/16 and upper cutoff 2^68. Its large-shape log-likelihood upper bound
is about -1281.1311, below the finite witness. The production certificate
uses the smaller stationary box [1/16, 256] in both coordinates, justified
by inward clipping. The retained oracle gives both kinds of cutoff.

The sample (0, 0, 1, 2, 3, 6, 12, 24), with alpha held at 0.5, instead has
a certified diagonal maximum at size = beta approximately
1.1088158545386361, with log likelihood -23.848045048100717.
Its observed two-dimensional Hessian is strictly negative definite.
The population-information singularity on the diagonal remains part of the
existing inference classification.

Tests also force a budget stop with iterlim = 1 and a numerical-range stop
with alpha = 1e308. Both leave estimate coefficients unavailable and retain
finite diagnostic likelihood intervals when available.

## Goldens and timings

The golden sample and estimates are byte-identical to the base.
The paired benchmark checks all eight held subsets. Unchanged routes return
identical status, coefficients and log likelihood across all before/after
runs. The alpha-held coefficients refine the old joint optimizer's result
within the existing golden tolerance and agree with the independent oracle.
No golden was regenerated.

Workstation medians below are milliseconds per fit on the 500-count golden
sample, at held golden values. Each timing averages five calls; three paired
runs supply each median. Both builds use C++17 and O2. The
[summary and ranges](2026-11-07-bnbinom-alpha-data/timings.json),
[benchmark source](2026-11-07-bnbinom-alpha-data/benchmark.cpp) and
[reproducer](2026-11-07-bnbinom-alpha-data/benchmark.sh) are retained.

| Held coefficients | Before ms | After ms |
|---|---:|---:|
| None | 317.089 | 308.297 |
| size | 65.450 | 64.901 |
| alpha | 68.737 | 510.892 |
| size, alpha | 1.627 | 1.624 |
| beta | 59.380 | 59.022 |
| size, beta | 1.446 | 1.451 |
| alpha, beta | 1.461 | 1.456 |
| All three | 0.050 | 0.048 |

The new global certificate costs about 7.4 times the previous alpha-held
search on this workload. Timing variation on unchanged routes accompanies
identical numerical outputs.

## Final validation

Every final run finished green.
The [validation record](2026-11-07-bnbinom-alpha-data/validation.txt)
contains commands, timestamps, result counts and source-identity checks.

| Run | Machine | Result |
|---|---|---|
| Targeted native bnbinom | simbox | 22 cases, 186562 assertions, zero failures |
| Targeted double-width bnbinom | Workstation | 22 cases, 186562 assertions, zero failures |
| C++ dev | simbox | 106/106 shards |
| C++ full | simbox | 129/129 shards |
| C++ no-long-double | simbox | 129/129 shards |
| C++ no-long-double | Workstation | 129/129 shards |
| R dev | simbox | FAIL 0, WARN 0, SKIP 154, PASS 21377 |
| R full | simbox | FAIL 0, WARN 0, SKIP 0, PASS 44195 |
| JS build and full tests | Workstation | Build succeeds; 480/480 tests, zero failures or skips |
| Site check | Workstation | 99 family pages with MathML; 99 C++17 snippets; links and browser checks pass |
| 80-digit oracle and full superlevel cutoffs | simbox | Four roots, 400 sampled box comparisons, all checks pass |
| Retained nonuniqueness reproducer | simbox | All 861 segments close |
| Paired timing/output checks | Workstation | All subsets pass |
| Source fingerprints and append-only tests | Workstation | Match/pass |
| Git diff whitespace check | Workstation | Clean |

The native simbox C++ runs use GCC 13.3. The workstation uses GCC 15.2.
Simbox R is 4.6.1; its R compilation uses g++-15. Long jobs used the
provided slot wrappers, nice 10 and single-threaded BLAS/OpenMP.
No simbox exit-75 fallback was needed.

The final fitter, common binding and C++/R test hashes match the simbox
before/after records. The final dossier prose passed the workstation site
check. The site checker refreshed an unrelated archived mobile screenshot;
its original bytes were restored.

From this worktree, the retained launchers reproduce the checks:

~~~sh
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR"
/tmp/umvl-cpu/simbox-run sh dev/research/evaluations/2026-11-07-bnbinom-alpha-data/final-simbox.sh
/tmp/umvl-cpu/run4 sh dev/research/evaluations/2026-11-07-bnbinom-alpha-data/workstation-suites.sh
/tmp/umvl-cpu/run sh dev/research/evaluations/2026-11-07-bnbinom-alpha-data/targeted-double.sh
/tmp/umvl-cpu/run sh dev/research/evaluations/2026-11-07-bnbinom-alpha-data/benchmark.sh
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-07-bnbinom-alpha-data/oracle.py
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-06-bnbinom-cert-data/nonuniqueness.py
~~~

## Open J13b requirements

1. With size held and alpha/beta free, a complete certificate still needs
   stationary-profile enumeration and uniform comparisons against the
   reachable negative-binomial face, including unresolved ties and shallow
   exterior profiles. The beta-held size/alpha pair is symmetric.
2. With all three shapes free, the unique conditional alpha maximum can be
   profiled, but a complete two-dimensional cover and uniform comparisons
   against both NB and Poisson faces are missing. The existing multistart
   and face comparisons remain numerical searches.

These are outside the alpha-held target. No global identified-uniqueness
theorem is asserted for arbitrary alpha-held samples; the cover and
comparison logic handle isolated competitors or stop.

Changes are confined to bnbinom and its bindings, append-only tests, and the
retained evaluation evidence. Shared infrastructure headers, settled
d/p/q/r and special-function code, truth tables, code generators, release
notes, status files and roadmaps were not changed. No commits were made.

The artifact-pane display command returned HTTP 403 because the viewer's
readable roots exclude this worktree. The report is saved at the requested
path. Viewer configuration and other worktrees were left unchanged.
