# Interior regularity metadata and covariance dispatch

TASK-102.14, 2026-11-06. Completed on `codex/infer-meta`, based on
`f30584cd1819744e130850bcb1e58a5d1c52ed28`, with uncommitted changes identified by
[source hashes](2026-11-06-infer-meta-data/source-sha256.json).

Updated `inference_numeric` in 25 dossiers and aligned its meaning with numerical
covariance availability. An available scope requires mathematical qualification
and an implemented numerical covariance path. Mathematical DQM alone cannot
enable a missing adapter. Structural singularities and invalid ordinary Hessians
are unavailable for both observed covariance and the Hessian-based sandwich.
Availability still does not prove estimator asymptotics, misspecification
assumptions or coverage.

No fitting code, defaults, goldens, d/p/q/r, special functions, truth files,
R code-generation scripts, roadmap or prohibited status files changed. No
installation, sub-agent, board edit, commit, push or publication was performed.
The board was read at session start. Work stayed in this worktree; remote suites
used the supplied simbox mirror wrapper.

## Family and regime changes

“Before” and “after” refer to numerical covariance eligibility, rather than a new
claim about mathematical regularity. Every affected family appears below.
[Exact before/after scope lists](2026-11-06-infer-meta-data/metadata-changes.json)
retain all conditions, reasons and classification links, including unchanged
supported scopes within an affected dossier.

| Family | Regime | Before | After |
|---|---|---|---|
| bbinom | size 0; size 1 with both shapes free | Available despite zero/rank-one information | Unavailable; identifiable size-one scalar submodels remain available |
| bnbinom | size = beta; size and beta both free | All positive subsets available | Explicit singular regime unavailable; exact runtime guard; holding either removes the null direction |
| cmb | size 0; size 1 with nu free | Available despite zero-information directions | Unavailable; size-one probability with nu known remains available |
| ged | held location; moving cusp; active nu bound | Pointwise C2 gate; remaining cusp classification called unfinished | DQM, rank and Hessian thresholds separated; three supported scopes retained; expanded held-shape scope withheld pending derivative validation; joint fitted cusp Hessian explicitly open |
| genbeta | shape2 = 1; shape1 and shape3 both free | All positive regular subsets available | Explicit singular regime unavailable; exact runtime guard; holding either removes the null direction |
| gev | regular, critical, Poisson and constrained shape regimes | Only held xi = 0 available; remaining classification called unfinished | Held Gumbel scope retained; regular xi > -1/2 expanded scope explicitly lacks validated adapter; critical/Poisson/constrained regimes unavailable for ordinary covariance |
| gpd | known threshold; regular and moving upper endpoint | Positive xi and held xi = 0 available; remaining classification called unfinished | Existing scopes retained; expanded xi > -1/2 including free xi = 0 withheld for derivative validation; critical/Poisson and free-threshold scopes unavailable |
| huber | finite positive scale and epsilon; any free subset | Unavailable because moving second-derivative joins fail pointwise C2 | DQM, full rank and valid a.e. Hessian established; unavailable because join-aware numerical derivatives and fitted covariance validation are not implemented |
| pig | positive finite mean and shape | Unavailable; DQM and score-tail domination called open | Those mathematical results are established; numerical derivative and covariance validation remain unimplemented |
| sged | moving mode, exponent and skew outside supported submodels | Unavailable remainder attributed to unfinished C2 classification | DQM above nu = 1/2 separated from Hessian invalidity at nu <= 1 and remaining fitted-Hessian/numerical join gaps; supported scopes retained |
| sn | alpha = 0 with xi and alpha both free | Singularity mentioned inside an available scope | Separate unavailable singular scope; existing exact runtime guard retained and tested for both covariance types |
| snorm | positive xi; skew free or asymmetric | Only xi = 1 held available; other scopes called non-C2 | Full-rank DQM and valid a.e. Hessian recorded; expanded join-aware numerical path withheld; symmetric held-skew scope retained |
| sstd | nu > 2; xi free, including xi = 1, or xi != 1 | Unavailable asymmetric scope attributed to moving second-derivative kink; free xi = 1 not stated | Regularity and a.e. Hessian recorded; unavailable expanded scope explicitly includes free xi = 1; validated symmetric held-skew scope retained |
| tbinom | singleton retained support | Available despite zero information | Unavailable |
| tlambda | lambda < 1/2; lower fitted bound; critical/Poisson endpoints | Unavailable; DQM and support-dependent rank called open | Regular full-rank model distinguished from unimplemented support-aware derivatives; lower fitted-bound and mixed-rate regimes separately unavailable |
| tpois | singleton retained support | Available despite zero information | Unavailable |
| zib | size 0, known point masses, size 1 with both probabilities free | Available despite structural singularity | Unavailable; scalar size-one known-nuisance scopes remain available |
| zinb | known degenerate base or complete inflation | Available despite zero information | Unavailable |
| zip | pi = 1 held; lambda free | Available despite zero information | Unavailable |
| zipf | known N = 1; s free | Available despite zero information | Unavailable |
| zmbinom | p0 = 1 held; or size 1 with prob free | Available despite zero-information directions | Unavailable; identifiable zero-mass scalar submodels remain available |
| zmgeom | p0 = 1 held; prob free | Available despite zero information | Unavailable |
| zmlogarithmic | p0 = 1 held; prob free | Available despite zero information | Unavailable |
| zmpois | p0 = 1 held; lambda free | Available despite zero information | Unavailable |
| ztbinom | size = 1; prob free | Available despite zero information | Unavailable |

Laplace already rejected free-location ordinary Hessian covariance and needed no
metadata change. Its exclusion, and GED/SGED at held nu = 1 and nu = 0.75 with
location free, were checked through public R `vcov()` for both covariance types.

## Mathematical basis and implementation decision

The mathematical results come from the
[DQM A](2026-11-04-dqm-a-report.md),
[DQM B](2026-11-04-dqm-b-report.md),
[DQM C](2026-11-04-dqm-c-report.md),
[moving-feature](2026-11-04-infer-moving-report.md) and
[closure](2026-11-04-infer-closure-report.md) reports. This job applies those
arguments; its finite numerical checks are not additional proofs.

For moving GED location, DQM and finite full-rank information require nu > 1/2;
ordinary location Hessians require nu > 1. At nu = 1 the ordinary Hessian misses
the kink contribution; below 1 its location curvature is nonintegrable. SGED has
the analogous moving-mode distinction. The integrable true-point Hessian at
1 < nu <= 2 does not settle the jointly fitted cusp Hessian. Snorm, sstd and Huber
have valid a.e. Hessians despite their moving second-derivative joins. PIG's DQM,
rank and domination results are established rather than open.

The moving-feature report's proposed `available: true` lists used the old
mathematical-only meaning. They were not copied wholesale. Inspection of
`inference_numeric_special.hpp` and the derivative validators shows that current
support/join scopes remain narrower: GEV only has held shape zero; GPD requires
positive free shape or held zero; snorm/sstd require held symmetric skew; the
expanded Tukey-lambda, Huber and PIG adapters do not exist. The existing nested
Richardson stencil is not a validated support/join-aware replacement. Their
newly regular models are therefore recorded accurately but remain unavailable
for this procedure. No new positive numerical covariance scope was enabled.
The already validated positive scopes were preserved.

The closure report concerns hypotheses against limit families, with Gaussian
cones or Poisson endpoint experiments and transformed transverse coordinates.
It does not authorize ordinary parent-family vcov at infinite coefficients or
new finite public members. Existing finite-interior scope bounds already exclude
these limits; no additional positive closure regime was introduced. Dedicated
exact/nonregular procedures and historical inventory rows retain their separate
meaning and sources.

The authored registry generator now emits exact guards for bnbinom size = beta
and genbeta shape2 = 1 when both coordinates in the null direction are free,
whether the other shape is held or estimated. The existing sn guard has the same
subset-aware contract. A finite-sample observed Hessian can be invertible at a
model-information singularity, so an eigenvalue test alone is insufficient.
Holding either coordinate in each null pair removes that structural obstruction;
35 deterministic production-dispatch checks cover these subsets and cusp stops.
The public R method reaches these guards through its existing C++ call; no second
R-only rule set was introduced.

The schema and shared inference note now distinguish these requirements.
The R loader's field shape remains unchanged and its metadata tests pass.
The availability generator rejects an available family without a dispatch
adapter, distinguishes unvalidated scopes from mathematically invalid ones,
and projects structural singular scopes as unavailable. Its regenerated catalog
has 355 rows: 145 implemented, 131 unreviewed, 4 available but missing and
75 invalid for the procedure. These counts include other procedures and
historical hypothesis rows, not just covariance.

## Validation

All required suites and numerical validators finished successfully before this
report was written. No failed
assertion, permitted failure or unreviewed validator outcome was accepted.
[Machine-readable run results](2026-11-06-infer-meta-data/test-results.json)
retain the final counts. All workstation builds and numerical validation used the supplied slot
wrappers, with their `nice -n 10` and single-thread BLAS/OpenMP settings.
The local R dev command waited approximately 36 minutes for all four slots;
it was left in the foreground until completion. Simbox was used for long suites
while local slots were busy; neither remote run required the exit-75 fallback.

| Check | Machine | Final result |
|---|---|---|
| `just test` (dev) | workstation, `run4` | FAIL 0, WARN 0, SKIP 154, PASS 20838; elapsed 163.803 s |
| `just test-full` | simbox | FAIL 0, WARN 0, SKIP 0, PASS 43654; elapsed 226.561 s |
| `just cpp-test` | simbox | 106/106 dev shards passed; no skips; ctest 41.41 s |
| `just site-check` | workstation, `run` | green; 99 family pages, 99 compiled/linked C++17 examples; 355-row inference browser and theory checks |
| Registry derivative validator | workstation, `run` | 66/66 families passed |
| Special derivative validator | workstation, `run` | 13/13 families and three additional check groups passed, including 24 singular cases |
| Standard / pilot validators | workstation, `run` | 11/11 and 4/4 families passed |
| New regime checker | workstation, `run` | 35/35 production-dispatch checks passed |
| Registry regeneration / availability / R metadata | workstation, `run` | generated header matches 66 entries; JSON current; metadata rejection tests pass |
| Scope and whitespace checks | workstation | no forbidden edits; `git diff --check` green |

The 154 dev skips explicitly require the full tier. The completed full tier has
zero skips, including the added public-vcov assertions (63 assertions in that
file). CMake's initial missing-system-Boost configure attempt used the recipe's
BH fallback successfully. Existing compiler, roxygen and shell-startup messages
were emitted; the testthat WARN counts above are zero.

Environment: R 4.6.1; C++17; workstation GCC 15.2.0 and simbox GCC 15.2.0.
R and derivative validators used installed BH headers under the R 4.6 library;
the site snippet checker selected the installed R 4.5 BH headers. The retained derivative JSON records Python, SymPy/mpmath
versions where the drivers emit them, source hashes, seeds and parameters.
Registry/standard/pilot seed: 20261101; special seed: 20261102; 40-digit mpmath
references. Registry/special checks use scaled absolute error
`abs(actual-reference)/(1+abs(reference))`, score tolerance 2e-6, Hessian
tolerance 2e-5, and golden total-score tolerance 1e-3. Standard/pilot tolerances
and maxima are retained in their own records. These seeded and golden checks
validate finite numerical workloads, not uniform tail bounds or coverage.

Retained outputs:
[registry](2026-11-06-infer-meta-data/registry-results.json),
[special](2026-11-06-infer-meta-data/special-results.json),
[standard](2026-11-06-infer-meta-data/standard-results.json),
[pilot](2026-11-06-infer-meta-data/pilot-results.json), and
[regime cases](2026-11-06-infer-meta-data/regime-results.json).
The reproducible [regime checker](2026-11-06-infer-meta-data/check-regimes.py)
uses the production probe built by the registry validator. Full build/test logs
remain in ignored `build/scratch/infer-meta-*.log`; their final outcomes and
numerical evidence are retained here and in the linked data.

```sh
mkdir -p build/tmp build/scratch
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp
export XDG_RUNTIME_DIR=$PWD/build/tmp
export BOOST_INCLUDE_DIR=/home/jonas/R/x86_64-pc-linux-gnu-library/4.6/BH/include
/tmp/umvl-cpu/run4 just test
/tmp/umvl-cpu/simbox-run just cpp-test
/tmp/umvl-cpu/simbox-run just test-full
/tmp/umvl-cpu/run just site-check
/tmp/umvl-cpu/run python3 dev/validation/inference-numeric-registry.py
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-06-infer-meta-data/check-regimes.py
/tmp/umvl-cpu/run python3 dev/validation/inference-numeric-special.py
/tmp/umvl-cpu/run python3 dev/validation/inference-numeric-standard.py
/tmp/umvl-cpu/run python3 dev/validation/inference-numeric-pilot.py
/tmp/umvl-cpu/run python3 dev/validation/inference-family-registry.py
/tmp/umvl-cpu/run python3 dev/validation/inference_availability.py --check
/tmp/umvl-cpu/run Rscript dev/validation/inference-numeric-metadata.R
```

## Open procedure work

These are explicit unavailable scopes, not unsettled interior DQM classifications:

1. Expanded tlambda, regular negative/zero-shape GPD/GEV, snorm/sstd and Huber
   need support/join-aware derivative evaluation with defined join behavior,
   independent derivative checks at the new regimes, and fitted-nuisance
   covariance/calibration evidence. PIG needs its numerical adapter and
   derivative/extreme-parameter validation. Current positive scopes were not
   extrapolated to cover them.
2. Joint fitted GED/SGED cusp Hessians for 1 < nu <= 2 need the fitted-point
   consistency and cusp-proximity control identified in the source reports.
   DQM and the true-point Hessian identity do not supply those results.
3. Critical/Poisson endpoint regimes, fractional cusps, active fitted bounds,
   singular models and closure hypotheses need their own procedures and
   calibration. No pseudoinverse, ordinary Wald or robust-sandwich validity is
   inferred from DQM, bootstrap availability or a successful fit.

No dpqr.hpp, special/, truth or generated R d/p/q/r change was needed for this
metadata and dispatch job. The mathematical and procedure gaps above are kept
in the dossier reasons and shared algorithm note as well as this report.

## Report checks and presentation

The final registry comparison, availability `--check`, all 14 local report links
and every tested source hash passed. `git diff --check` passed. The requested
artifact-pane `show` command was attempted after completion, but the viewer
returned HTTP 403 because this worktree is outside its configured readable
roots. No viewer configuration or file outside this worktree was changed. This
presentation failure is separate from the successful build and test results.
