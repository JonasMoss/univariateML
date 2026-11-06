# bnbinom: scalar certificates and multivariate requirements

TASK-116.10, 2026-11-06. Branch codex/bnbinom-cert, base
bb734cd78b846a565d8835606328bb1285a82db6, with the uncommitted changes
identified in [source.json](2026-11-06-bnbinom-cert-data/source.json).

All three one-free-shape fits now have global certificates. A nonzero sample
has a unique finite maximizer in each such submodel; the implementation
either isolates it to coefficient accuracy or stops with budget or numerical
details. Fixed-alpha two-shape existence is proved, and a concrete sample
proves failure of labeled uniqueness. The default three-free fit retains
its original search and golden output. The family-wide certified flag remains
'no' because the multivariate searches listed as open below lack a global
certificate.

## Coverage

“Positive sample” below means at least one count is positive, with all counts
nonnegative. Held values are finite and strictly positive.

| Free shapes | Positive-sample result | Implementation |
|---|---|---|
| size | Unique finite global maximum | Certified scalar solve |
| alpha | Unique finite global maximum | Certified scalar solve |
| beta | Unique finite global maximum | Certified scalar solve |
| size, beta; alpha held | A finite maximum exists; labeled uniqueness fails for some samples | Original uncertified multistart |
| alpha, beta; size held | Finite candidates compete with an NB face; a complete sample criterion remains open | Original uncertified multistart/face comparison |
| size, alpha; beta held | Symmetric to the previous row | Original uncertified multistart/face comparison |
| size, alpha, beta | Finite candidates, NB and Poisson faces; general existence and identified uniqueness remain open | Original uncertified multistart/face comparison |
| None | Direct likelihood evaluation | Unchanged; not counted as a fit certificate |

Every all-zero sample with at least one free shape has only an excluded
point-mass supremum. The existing exact stop and Poisson(0) details remain in
place. Every nonempty supported free subset is covered by the table; no new
structural hold exception was added.

## Mathematics and numerical contract

The canonical proofs and solver contract are in
[bnbinom/README.md](../../../univariateML-cpp/include/cppunivariate/distributions/bnbinom/README.md#scalar-certificates).

For each scalar score, a digamma integral representation has an integrand
whose sign changes once from negative to positive as its integration variable
increases. For alpha, this follows from strict convexity of the product of
two exponential increments minus the empirical concave function
A(t) = mean(1-exp(-x*t)). For size, the sign reduces to a strictly increasing
positive ratio times A(t), minus one; beta follows by symmetry.

At any zero of the integrated score, increasing the free shape weights its
negative part more strongly relative to its positive part. Thus the score
becomes strictly negative; decreasing the shape makes it strictly positive.
Its endpoint asymptotics prove that a root exists whenever there is a positive
count. This proves uniqueness and global optimality for each scalar fit.
It does not assert joint concavity or multivariate profile unimodality.

The solver expands a bracket in log shape and preserves strictly signed
endpoints. It accepts only a bracket of width at most min(gradtol, 1e-8);
the geometric midpoint has relative root error at most exp(width/2)-1,
conditional on those signs. A small score alone never establishes success.

A score sign must exceed 128 times machine epsilon times the average sum of
the absolute digamma terms. Unresolved midpoint signs trigger quarter-point
comparisons. Failure to make signed progress or represent a shape produces a
numerical stop. This is the package's exact-arithmetic certificate conditional
on computed comparisons, with a practical floating-point margin, rather than
outward-rounded interval arithmetic.

Refinement has min(iterlim, 100000) steps; expansion and refinement share
128*min(iterlim, 100000) evaluations, computed with a 64-bit counter.
Exhaustion preserves IterationFailure with budget_exhausted set, transported
as univariateML_budget. The appended NumericalFailure status is transported
as univariateML_numerical in R and kind='numerical' in JS.

Stops leave the result coefficient fields NaN. Where available, they retain
one evaluated finite witness in public coefficient order. Its likelihood
margin is 1e-10*n*(1+abs(logLik)/n); the global upper bound remains infinite.
These candidate bounds do not certify the stopped witness.

For example, size=1, beta=100000 with all observations equal to one has a
unique mathematical alpha root sqrt(beta*(beta+1)). Current double-precision
digamma comparisons cannot isolate it to the required width and correctly
stop numerically. Increasing iterlim does not repair this arithmetic gap.
A family-local stable digamma-difference evaluator could reduce such stops;
it is not needed to return a correct stop.

With alpha held and size and beta free, positive-sample likelihood superlevel
sets are compact. A shape approaching zero makes every positive-count mass
vanish uniformly in the other shape, using the bound
p(x) <= (shape)_x/x! for x>0. If a shape diverges while the other stays bounded
below, the gamma-mixture construction gives stochastic escape to infinity.
These alternatives exhaust escapes from the positive quadrant and prove
attainment.

### Concrete failure of labeled uniqueness

Use the exact 500-count golden sample, hold alpha=2, and consider the witness
(size, alpha, beta)=(1,2,5). Its mass is exactly

    p(x) = 60 / ((x+5)*(x+6)*(x+7)).

The [independent diagonal-exclusion certificate](2026-11-06-bnbinom-cert-data/nonuniqueness.py)
and [output](2026-11-06-bnbinom-cert-data/nonuniqueness.json) give:

| Quantity | Total log likelihood |
|---|---:|
| Finite witness | -1276.345658177647696 |
| Upper bound over every diagonal point size=beta | -1276.445827365689238 |
| Certified separation | At least 0.10016918804154 |

For diagonal shape s, write p(x;s)=p(0;s)*R_x(s), where p(0;s) decreases
and R_x(s)=(s)_x^2/((2+2*s)_x*x!) increases. Consequently, on [l,h],
p(x;s) <= p(0;l)*R_x(h). The reproducer closes 861 segments covering
[1e-6,256]. The small-shape bound gives an exterior upper log likelihood
of -5513.5490, and stochastic ordering with the CDF at the sample maximum
175 gives -78890.8420 for the other exterior tail. No diagonal region remains
unchecked.

These are exact-arithmetic inequalities conditional on 60-digit computed
comparisons with a 1e-20 absolute log-likelihood margin. No outward rounding
is claimed. Compactness gives an attained two-shape global maximum; the
strict diagonal exclusion forces it off diagonal. Swapping size and beta
therefore gives at least two distinct labeled global maximizers. They
represent the same distribution. The witness itself is not claimed to be a
maximizer, and uniqueness after imposing size<=beta remains open.

The exploratory [multivariate search](2026-11-06-bnbinom-cert-data/multivariate.py)
and [output](2026-11-06-bnbinom-cert-data/multivariate.json) are numerical
evidence only. Their fixed-alpha mirrored modes do not supply a general
certificate.

## Changes and golden accuracy

The changes are confined to the bnbinom dossier/fitter/tests, its common
family binding, its R tests, and this report/evidence directory. General
shared headers, settled density/special-function/truth/code-generation
sources, goldens, status files and roadmap were not changed.

The new dispatch is used exactly when one shape is free. All-held evaluation,
the all-zero stop and the two-/three-free searches retain their prior paths.
The default golden coefficients and log likelihood are bit-for-bit equal
to the base result in the paired native replay:

    size  = 1.0672234787479615
    alpha = 2.1419791546447171
    beta  = 5.3647338351087397
    logLik = -1275.6474863695587

The changed scalar conditional golden coefficients differ from their old
optimizer values by less than 6.1e-9 relatively; their evaluated golden
log likelihoods agree. No golden baseline was regenerated.

New tests are appended at the end of test.cpp. They cover exact sqrt(2)
roots in all three coordinates, noninteger roots from an independent
80-digit gamma/digamma oracle, perturbation likelihood comparisons, all-zero
limits, insufficient-budget witnesses, unattainable precision, scaled
closed-form roots, and terminal arithmetic failure. The oracle explicitly
checks its bracket signs and a total-score residual below 1e-70.
R and a retained JS smoke check verify the public structured conditions.

## Timings

Workstation GCC 15.2.0, C++17, -O2, nice 10, one CPU slot and one BLAS/OpenMP
thread. The workload is the 500-count golden sample; held values are the
golden estimates. Each table entry is the median of three paired batch
means, with five fits per batch. Original and new executables were run
alternately. Shared workstation load produces substantial variation on the
unchanged paths; their apparent timing differences are not algorithmic gains.

| Held shapes | Before, ms/fit | After, ms/fit |
|---|---:|---:|
| None | 531.565 | 479.394 |
| size | 82.409 | 86.589 |
| alpha | 87.557 | 79.915 |
| size, alpha | 8.676 | 1.771 |
| beta | 78.017 | 65.304 |
| size, beta | 6.991 | 1.566 |
| alpha, beta | 9.657 | 1.584 |
| All three | 0.065 | 0.052 |

The three scalar paths are 4.5–6.1 times faster on this workload.
[Timing summary and ranges](2026-11-06-bnbinom-cert-data/timing-summary.json),
the before/after paired CSV files, [benchmark source](2026-11-06-bnbinom-cert-data/benchmark.cpp)
and [reproducer](2026-11-06-bnbinom-cert-data/reproduce-benchmark.sh)
are retained. CSV fields are held-bitmask, status, size, alpha, beta,
log likelihood, milliseconds; mask bits 1,2,4 mean held size, alpha, beta.

## Final validation

All final validation commands completed successfully. The retained
[validation record](2026-11-06-bnbinom-cert-data/validation.txt) contains
suite starts/finishes and result summaries.

| Command/check | Machine | Final result |
|---|---|---|
| Targeted native bnbinom executable | Workstation | 19/19 cases; 185083 assertions; zero failures |
| just cpp-test, dev | simbox | 106/106 shards |
| UMVL_TEST_TIER=full just cpp-test | simbox | 128/128 shards |
| just cpp-test-no-long-double | simbox | 128/128 shards |
| just test, dev | simbox | FAIL 0, WARN 0, SKIP 154, PASS 20888 |
| just test-full | simbox | FAIL 0, WARN 0, SKIP 0, PASS 43708 |
| just js-build; just js-test | Workstation | Build succeeds; 432/432 tests; zero failures/skips |
| Additional JS scalar roots and stop payloads | Workstation | Three exact roots, three budget stops, one numerical stop |
| just site-check, after final dossier prose | Workstation | Green; 99 family pages with MathML; 99 C++17 snippets compile/link; browser/link checks pass |
| 80-digit scalar oracle; diagonal exclusion | Workstation | All assertions/comparisons pass |
| Final source fingerprints; git diff --check | Workstation | Match/clean |

Simbox used GCC 13.3.0 and R 4.6.1 on WSL2; the workstation used GCC 15.2.0,
R 4.6.1 and Python 3.14.5. The
[simbox environment](2026-11-06-bnbinom-cert-data/simbox-environment.txt)
and source fingerprints are retained. The no-long-double build uses
BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS. Native benchmark and oracle jobs
used run; JS/site jobs used run4. Long suites used simbox-run, nice 10,
four build/test workers and single-threaded BLAS/OpenMP. No final suite
needed the exit-75 local fallback.

The [suite script](2026-11-06-bnbinom-cert-data/run-suites.sh) records exact
tier commands. From the worktree root, reproduce with:

    export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
    mkdir -p "$TMPDIR"
    /tmp/umvl-cpu/simbox-run sh dev/research/evaluations/2026-11-06-bnbinom-cert-data/run-suites.sh
    /tmp/umvl-cpu/run sh dev/research/evaluations/2026-11-06-bnbinom-cert-data/reproduce-benchmark.sh
    /tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-06-bnbinom-cert-data/oracle.py
    /tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-06-bnbinom-cert-data/nonuniqueness.py
    /tmp/umvl-cpu/run node --v8-pool-size=1 dev/research/evaluations/2026-11-06-bnbinom-cert-data/js-scalar.mjs

The suite snapshot and final tree have identical fitter/binding/test code.
The final concrete-counterexample prose was added after that snapshot and
was checked by the final workstation site run.

Environment/exploratory corrections: the first standalone compiler attempt
lacked the installed BH include path and was rerun successfully with it.
An initial diagonal separation target of 0.5 was unattainable for the chosen
witness (the numerical gap is about 0.305); that run was interrupted, and the
0.1-target run closed every segment. The standard CMake system-Boost attempt
falls back successfully to BH. R documentation generation emits an existing
unresolved-topic warning for “1e-7,1e-6” outside bnbinom; final testthat warning
counts are zero. Site acceptance rewrites an archived mobile screenshot;
its baseline bytes were restored after validation.

## Open requirements

1. **Alpha held; size and beta free.** Existence and a genuine labeled
   nonuniqueness counterexample are settled, but the fitter still lacks
   isolation/comparison of every interior stationary point and the singular
   diagonal. There is now a concrete bounding route: p(0;r,b) decreases in
   both shapes, while R_x(r,b)=(r)_x*(b)_x/((alpha+r+b)_x*x!) increases
   in both. Opposite box corners therefore bound every mass. Small-shape
   mass bounds and stochastic CDF bounds can enclose the search in a finite
   rectangle. What remains is an implemented, validated subdivision/root
   isolation certificate with coefficient accuracy, including near-flat
   diagonal cases. A likelihood upper bound close to a candidate alone
   does not establish coefficient accuracy.

2. **Size held or beta held, with both other shapes free.** The scalar
   theorem supplies unique coordinate solves, not a certificate for the
   remaining profile. A complete stationary-point enumeration and uniform
   bounds against the reachable NB face, including unresolved ties and
   shallow exterior profiles, are missing.

3. **All three shapes free.** Profiling alpha is mathematically justified
   by its unique scalar maximum. A certified two-dimensional profile search,
   treatment of the symmetry seam, and uniform exterior comparisons against
   NB/Poisson faces are missing. The present multistart and face-likelihood
   comparisons are not a global existence or maximization proof. No general
   positive-sample existence criterion or identified uniqueness assertion
   is made.

4. **Legacy joint search controls.** Its fixed parameter window, finite
   starts and legacy line-search limits remain part of the unchanged
   multivariate implementation. The new scalar certificate follows the
   iterlim work-budget policy. Completing the joint replacement must also
   remove or explicitly certify those legacy search exclusions; increasing
   the number of starts cannot supply that guarantee.

These open entries are why the family retains certified='no' and its default
behavior. No unverified joint search was promoted to a certified fit. No
commits, board edits, package installations or publishing actions were made.


## Artifact delivery

The requested report is saved in this worktree. The artifact-pane show command
returned HTTP 403: the viewer does not include this worktree among its readable
roots. No viewer configuration or other checkout was changed.
