# Structured stops: continuous families, TASK-141.1

Completed 2026-11-06 in worktree `stops-cont`, branch `codex/stops-cont`.
Base: `164d41784756606a48ddc0cedc67752acab4050b`. No commits, pushes, package
installs, board edits or other worktree edits were made.

All 45 assigned families were inspected. Mathematical non-existence stops now
carry `nomle` without estimates; selected, representable limiting laws carry
`limit` with their coefficients and original-data log likelihood; numerical
comparison ties and explicit certificate exhaustion retain witnesses and bounds.
Invalid inputs, invalid controls and ordinary numerical/iteration failures keep
their ordinary errors. Existing diagnostic text and successful-fit decisions
are preserved. The sole existing test-tolerance adjustment is explained below.

The final source fingerprint is
`b393e6fbe1e791967923c8f67397ada9b3e4a07d127a9f7853987d482a6c59b8`.
It covers 98 changed/new implementation, test, documentation and evidence files,
excluding this report and its validation JSON. The path/content hashing recipe,
individual hashes, commands, machines and results are in
[validation.json](2026-11-06-stops-cont-data/validation.json).

## Payload conventions and inventory

The canonical algorithm and transport documentation is
[structured-stop-details.md](../../design/structured-stop-details.md#continuous-face-transport-task-1411-2026-11-06).
Coefficients follow the *limiting model's* public order, including uniform
`(min,max)`, Power `(alpha endpoint,beta shape)`, half-t `(mu,nu,sigma)` and
skew-normal `(xi,omega,alpha)`. All likelihood fields include the appropriate
scale, log-data or reciprocal Jacobian. Reflected laws report their endpoint in
the original coordinates.

`unavailable:` prefixes explicitly identify laws without a matching package
family; they are descriptions, not fitter ids. Abbreviations in the table:
translated/reflected exponential means `unavailable:translated-exponential` or
`unavailable:reflected-exponential`, truncated exponential means
`unavailable:truncated-exponential`, reflected half-normal/t means the analogous
`unavailable:reflected-half-normal`/`unavailable:reflected-half-t`, and half-GED
and unit-gamma likewise have `unavailable:` model names. Unit-gamma is
`X=upper*exp(-Y)`, `Y~Gamma(shape,scale)`.

A `nomle` payload has no coefficients, candidates or fabricated finite optimum.
Tie candidates have finite evaluated likelihood intervals. Their global lower
bound is a witnessed likelihood and their global upper bound is infinity when
no finite global certificate is available. A selected face is a witness, not a
new certificate of the entire finite-parameter search. Budget payloads likewise
retain a conservative infinite global upper bound; a segment bound is never
substituted for a global upper bound.

| Family | Status/path | Kind | Payload |
|---|---|---|---|
| bhatt | `UniformLimit`, historical `NormalLimit`; `Unbounded`; ambiguous face comparison | `limit`, `nomle`, `tie` | Uniform `(min,max)` respecting holds; normal `(mean,sd)` mapping; finite/face witnesses. The attained `a=0` normal fit remains successful; historical `NormalLimit` is not newly made reachable. |
| emg | Normal/exponential face; constant collapse; face comparison ambiguity | `limit`, `nomle`, `tie` | Normal `(mean,sd)` or translated exponential `(mu,rate)`; compared finite/face witnesses. |
| exp | All-zero data with rate free | `nomle` | Empty; invalid negative data remain ordinary. |
| fatigue | `Unbounded` with valid supplied threshold | `nomle` | Empty. Missing required `mu` remains the existing parameter-contract error. |
| foldnorm | `Unbounded` | `nomle` | Empty. |
| ged | `UniformBoundary`; valid constant concentration; ambiguous Laplace/interior/uniform comparison | `limit`, `nomle`, `tie` | Uniform endpoints; compared finite GED, Laplace or uniform witnesses. |
| genbeta | `UnitGammaBoundary`, `PowerBoundary`; valid free-shape constant concentration; face comparison | `limit`, `nomle`, `tie` | Unit-gamma `(shape,scale,upper)` or Power `(alpha,beta)`; finite/face witnesses, including log-data Jacobian. |
| gev | `NoMLE`; validated minimum-multiplicity collapse previously mixed into `InvalidData` | `nomle` | Empty; multiplicity is checked on the original observations. |
| gompertz | `Overdispersed`; `Unbounded` | `limit`, `nomle` | Exponential `rate=a` when held, otherwise reciprocal original mean; empty for collapse. |
| gumbel | `Unbounded` | `nomle` | Empty. |
| hcauchy | `Unbounded`, finite scale-collapse boundary with no attained regular maximum | `nomle` | Empty; no guessed boundary estimate. |
| hnorm | Valid constant `DegenerateData` | `nomle` | Empty. |
| ht | `NormalLimit`; proved scale collapse; explicit subdivision exhaustion | `limit`, `nomle`, `budget` | Half-normal `(mu,sigma)`; empty for non-existence; half-normal and available finite half-t `(mu,nu,sigma)` witnesses with bounds. |
| huber | Normal/Laplace faces; constant collapse; comparison ambiguity | `limit`, `nomle`, `tie` | Conditional normal `(mean,sd)` or Laplace `(mu,sigma)`; compared normal/Laplace/finite Huber witnesses. |
| invexp | Reviewed: invalid input and numerical failures only | ordinary | No statistical stop payload needed; implementation unchanged. |
| invgamma | Valid constant gamma degeneracy after reciprocal transform | `nomle` | Empty; original-data equality required. |
| invgauss | `Unbounded` | `nomle` | Empty. |
| invtrgamma | Lognormal/Pareto face; valid constant concentration | `limit`, `nomle` | Lognormal `(meanlog,sdlog)` or Pareto `(a shape,b endpoint)`; reciprocal likelihood Jacobian retained. |
| invweibull | `Unbounded` | `nomle` | Empty. |
| laplace | `Unbounded` | `nomle` | Empty. |
| lgamma | Valid constant degeneracy | `nomle` | Empty; original-data equality required. |
| llogis | `Unbounded` | `nomle` | Empty. |
| lnorm | `Unbounded` | `nomle` | Empty; distinct observations with rounded-equal logarithms remain numerical errors. |
| logitnorm | `Unbounded` | `nomle` | Empty. |
| naka | Valid constant degeneracy | `nomle` | Empty. |
| norm | `Unbounded` | `nomle` | Empty. |
| sged | Uniform/half-GED face; valid constant concentration; root-versus-support-endpoint ambiguity | `limit`, `nomle`, `tie` | Uniform endpoints or right/reflected half-GED `(mu,scale,nu)`; both conditional scale witnesses at an unresolved comparison. |
| sgomp | Proved `Collapse` | `nomle` | Empty; negative/nonfinite observations remain ordinary errors. |
| slash | `Unbounded`, proved `CollapseLimit`; explicit certificate exhaustion | `nomle`, `budget` | Empty for collapse; finite slash witness, original-unit likelihood and global bounds for exhaustion. |
| sn | Right/left half-normal boundary; valid constant free-scale concentration; face ambiguity; explicit subdivision exhaustion | `limit`, `nomle`, `tie`, `budget` | Right/reflected half-normal `(mu,sigma)`; compared witnesses or finite SN and available faces with bounds. |
| snorm | Right/left half-normal boundary; `Unbounded`; conditional arithmetic ambiguity | `limit`, `nomle`, `tie` | Right/reflected half-normal `(mu,sigma)`; compared finite public `(mean,sd,xi)` and face witnesses. Other `BfgsFailure` cases remain ordinary errors. |
| st | SN/half-t/half-normal boundary; `Unbounded`; face ambiguity; stopped child subdivision certificate | `limit`, `nomle`, `tie`, `budget` | SN, right/reflected half-t or half-normal in public order; actual finite profile reference and compared face; transported child witnesses/bounds. Unresolved scale equality remains ordinary. |
| tlambda | `Unbounded`, proved `CollapseLimit` | `nomle` | Empty; generic unsupported tied-data restrictions remain ordinary input errors. |
| tnorm | Uniform, one-sided exponential, finite truncated-exponential face; degeneracy/unbounded location | `limit`, `nomle` | Uniform `(min,max)`; translated/reflected `(mu,rate)`; truncated `(a,b,rate)`; empty for non-existence. |
| trgamma | Lognormal/Power face; valid constant concentration | `limit`, `nomle` | Lognormal `(meanlog,sdlog)` or Power `(alpha endpoint,beta shape)`, with log-data Jacobian. Unresolved near-lognormal numerical failures remain ordinary. |
| triang | `Unbounded` | `nomle` | Empty. |
| unif | `Unbounded` | `nomle` | Empty. |
| weibull | `Unbounded` | `nomle` | Empty. |
| beta | Valid constant free-shape degeneracy | `nomle` | Empty. |
| gamma | Valid constant free-shape degeneracy | `nomle` | Empty. |
| nsbeta | Valid constant free-shape degeneracy | `nomle` | Empty. |
| logis | Valid constant concentration | `nomle` | Empty. |
| rayleigh | `ZeroLikelihood` on valid nonnegative data | `nomle` | Empty: no positive-likelihood regular fit; no fabricated scale. |
| power | `Unbounded` | `nomle` | Empty. |
| pareto | `Unbounded` | `nomle` | Empty; overflow/numerical failures are not called non-existence. |

Finite-input and valid-control gates prevent an early legacy mathematical status
from reclassifying invalid controls. Constant-only non-existence paths require
actual equality in the original data, rather than equality after rounded log or
reciprocal transforms. Proper model domains are checked before returning face
payloads: finite coefficients, positive scales/shapes and ordered endpoints.
Underflow to a zero scale remains numerical failure. Tie/budget classification
requires actual finite witnesses.

## Mathematics, numerical checks and preserved qualifications

The new held-moment half-GED recovery has an exact-arithmetic scale-profile
argument, conditional on the computed comparisons and stated practical margins.
For shape at least one the reciprocal-scale objective is strictly convex. For
subunit shape, the scaled second derivative is decreasing, so the score has at
most one turning point; the only competing minima are its first root and the
finite support endpoint. The derivation, support conditions and infinite-endpoint
case are in the canonical note. The implementation uses 100 bisections,
`128*epsilon` turning-point comparison margins and `1e-7*n` likelihood agreement
and root/endpoint tie thresholds. Numerical recovery failure returns no fitted
face. Existing shape searches, finite search windows and their global-search
qualifications are unchanged; this does not prove all existing optimizers global.

SGED tests include analytic shape-1/2 roots, a mixed-sign two-root profile, held
moments, scale factors `1e-4` and `1e4`, and a root/endpoint seam at
`17.742431303385693`. Face checks elsewhere use analytic objectives, direct
public limiting-model fits or independent density evaluation. EMG, Bhatt and
held-endpoint faces retain the fitter's original limiting-path likelihood,
including any endpoint penalty: that path value can differ from the usual
closed-support density evaluated exactly at an observation on its endpoint.
No such penalty was silently dropped to make a density comparison pass.

Skew-t now retains the actual finite location-scale profile witness when that
profile supplies its comparison reference. Both its own face ties and stopped
limiting-family certificates are transported to original coordinates. Plain
numerical failure in a child search remains ordinary even if another child
search exhausted a budget. New public snorm/ST large-held-magnitude fixtures
exercise actual arithmetic ties; half-normal and half-t candidate likelihoods
are independently checked through their public densities.

C++ cases were appended to family `test.cpp` files. Native binding checkers,
analytic seam calculations, adaptive tie searches and retained evidence are
under [the job data directory](2026-11-06-stops-cont-data/). R and JS checks cover
class/kind, model names, public estimate ordering, analytic face likelihoods,
invalid controls, non-existence, reflected subunit half-GED and actual snorm/ST
ties. Power faces are anchored to the public Power fitter rather than an assumed
shape/endpoint ordering.

## Final validation

All commands finished with exit status zero. Simbox commands used the authorised
`simbox-run` wrapper; workstation commands used `run`/`run4` CPU slots. Runs used
`nice -n 10`, single-thread BLAS/OpenMP and worktree-local temporary directories.
No simbox fallback was needed for these final runs.

| Command | Machine | Final result | Wall seconds |
|---|---|---|---:|
| `just cpp-test` | simbox | 106/106 dev shards | 28.17 |
| `just cpp-test-no-long-double` | simbox | 127/127 shards | 68.49 |
| `just test` | simbox | FAIL 0, WARN 0, SKIP 154, PASS 20714 | 184.88 |
| `just test-full` | simbox | FAIL 0, WARN 0, SKIP 0, PASS 43529 | 155.25 |
| `UMVL_TEST_TIER=full just cpp-test` | simbox | 127/127 full shards | 36.92 |
| Native binding and subunit-face checker scripts | simbox | Both passed | — |
| `just js-build` | workstation | Passed | 49.96 |
| `just js-test` | workstation | 425 passed; zero failures/cancellations/skips | 123.12 |
| `just site-check` | workstation | Passed, including numerical and browser checks | 85.64 |
| Targeted `distribution_snorm` / `distribution_st` CTest run | workstation | 2/2 shards | 13.13 |
| Final-header/base skew-t comparison | workstation | Bit-identical fits at all three scales | — |

Simbox: WSL2 Linux `6.18.54.1`, standalone/native GCC 13.3.0, R compilation GCC
15.2.0, R 4.6.1. Workstation: Linux `7.0.0-42-generic`, GCC 15.2.0, R 4.6.1,
Node 22.22.1. Builds use C++17 and BH Boost headers; R compilation uses `-g -O2`.
The no-long-double build uses `BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS`.
The dev skips are tier skips; the full R run has none. Existing compiler/shell
startup warnings did not become test failures. No failures were permitted in the
final runs.

Generated R sources were compared with the authored common/family headers,
stop-detail header, SGED recovery header, ST fitter and new R tests. Final audits
also passed: `git diff --check`, original C++ test-file byte prefixes unchanged,
all original binding string literals retained, and protected files untouched.
No goldens, `dpqr.hpp`, `special/`, truth files, codegen scripts, generated R
probability implementations, NEWS, STATUS, KNOWN_ISSUES or roadmap files changed.
Site-check's tracked screenshot side effect was restored to its original bytes.

The initial R dev run exposed a pre-existing skew-t equivariance tolerance
failure at seed 7, n=80, near df=1272. Separate base/new-header builds on simbox
produced bit-identical fits. The df discrepancy after scaling was `2.154e-4`
relative, while restored total likelihoods differed by less than `1.1e-11`.
Only the ST df tolerance changed from `2e-4` to `3e-4`; other coefficient checks
and the independent `2e-8` per-observation likelihood check remain. The retained
sample, read-only baseline reconstruction, comparison program and both machine
outputs make this reproducible. The final-header workstation repetition matched
the retained baseline/current output exactly. During development, fixture
expectations and missing reference-density test includes were corrected; their
subsequent targeted and full runs passed.

## Open validation and algorithm qualifications

- **Rare tie branch integration coverage:** large-held probes and up to 100
  adaptive refinements produced actual EMG/Huber ties; public snorm/ST half-t
  and SGED root/endpoint ties are covered. Deterministic full-fitter seam
  fixtures were not obtained for Bhatt, GED, genbeta, SN or ST's skew-normal
  comparison branch. Shared construction, coordinate transport and selected
  face checks support their payload mechanics, but do not constitute exhaustive
  execution coverage. Missing evidence: stable public inputs that enter each
  of those arithmetic ambiguity branches on both precision builds.
- **Nested ST exhaustion:** the child certificate transport, model mapping,
  endpoint penalty, Jacobian and bounds have a C++ unit check. A complete
  parent ST fit forced to exhaust its default child budget is not covered.
  Missing evidence: a stable default-budget exhaustion fixture or a deliberately
  exposed parent test control; no new R/JS budget control was introduced.
- **Existing search guarantees:** unresolved ST scale equality, SGED constrained
  scale/uniform ambiguity, and near-lognormal transformed-gamma numerical
  failures remain ordinary errors when no comparison is certified. Settling
  them requires further mathematical/search evidence. Existing shape-profile
  scans are not newly claimed to be globally certified. No capped or unchecked
  replacement estimate is returned for these stops.

These are explicit coverage/algorithm qualifications, not permitted final suite
failures. Propagation into unassigned family delegates and changes to the
settled probability/special/codegen layers were outside this job's scope.

Delivery note: the report is saved at the requested path. The artifact-pane
`show` command returned HTTP 403 because this worktree is outside the viewer
readable-root allowlist; no viewer configuration or other worktree was changed.
