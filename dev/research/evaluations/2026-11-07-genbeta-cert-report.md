# Generalized beta free-power certificates — TASK-116.39

Implemented certificates for the free-power subsets `{a}` and `{q,a}` with
the endpoint held. The general `{p,a}` and `{p,q,a}` routes remain open:
they retain the existing mesh, and neither their successful estimates nor
their winning-face decisions have a global certificate. The family's
`fit.certified` flag remains `partial`, and its note now names the scope.

Here `p=shape1`, `q=shape2`, `a=shape3`, and `b=scale>max(x)`.
The starting commit was `e99856a8c168473c120eed343593f98dca00e8b1`, on
`codex/genbeta-cert`. Validation finished on 2026-11-08. No commits were made.

## Implemented scope

| Free coefficients, with b held | Outcome |
|---|---|
| a; p,q held positive | Unique global maximum from strict concavity and independently validated opposite score signs; numerical failure if coefficient inclusion cannot be established. |
| q,a; p held, p != 1 | Uniform bounds exclude both power tails. Complete interval score/curvature enumeration isolates retained maxima and compares their likelihood enclosures. Numerical, budget and comparison-tie stops are terminal. |
| q,a; p held at 1 | Exact Kumaraswamy delegation, with coefficients and diagnostic witnesses transported to generalized beta. |
| p,a; q held at 1 | Existing attained power ridge; the identified product is fitted and a canonical representation returned. |
| p,a; q held, q != 1 | **Open GENBETA-PA**, below. |
| p,q,a | **Open GENBETA-PQA**, below. |

Fixed-power beta-shape routes and the structural stops for free b were
preserved. The all-free golden sample, coefficients and likelihood were
preserved; before/after executable results agree exactly at the printed
17-digit precision.

## Certificates and diagnostics

The power-only route retains its stable small-q and weighted exponential-tail
score formulas. After bisection, acceptance now independently requires padded
opposite score signs at the proposal times
`1 +/- min(gradtol,1e-10)/4`. Strict concavity then gives coefficient
inclusion, conditional on those comparisons. A rounded zero or an
unrepresentable requested radius does not establish convergence.

For held p with q,a free, put `v=Y/mean(Y)` and `h=a*mean(Y)`, where
`Y=log(b/x)`. The conditional q likelihood is strictly concave, its score
has a unique root, and q(h) increases. Uniform small- and large-h bounds
prove finite attainment for every nonconstant sample. On the retained compact
interval, bounds on the profile's first and second derivatives exclude
monotone cells and minima, and isolate maxima in strictly concave cells.
Other cells are subdivided. There is no global unimodality assumption.

Acceptance requires relative widths of both the h and q enclosures below
`min(gradtol,1e-9)/2`, with the returned coefficients inside the enclosures.
Root likelihoods have upper and lower bounds; an overlapping comparison
stops as a tie. Every popped cell and profile evaluation consumes the
`128*min(iterlim,100000)` certificate ledger. Inner solves and tail bracketing
use `min(iterlim,100000)` iteration caps. Exhaustion never falls back to the
mesh. The complete mathematical argument is in the
[held-p proof](2026-11-07-genbeta-cert-data/bounds.md), and the implementation
is [power_profile_certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/genbeta/power_profile_certificate.hpp).

These are certificates in the package's conditional exact-arithmetic sense,
with practical floating-point allowances, not outward-rounded intervals.
Allowances scale with the absolute operands: 64 epsilon for the beta score,
128 epsilon for trigamma differences, 256 epsilon for profile scores,
512 epsilon for likelihood/interval comparisons, and 1024 epsilon for
curvature comparisons. Compensated sums support double-width intermediates.
The workstation double-precision run exposed cancellation inside
`log(x)-digamma(x)` at a tail endpoint. The score allowance now includes
those uncancelled operands, and the conditional-shape bracket expands until
independent endpoint signs validate it.

Held p=1 uses the existing Kumaraswamy certificate on x/b, with likelihood
shift `-n*log(b)`. Its inclusion precision is inherited from Kumaraswamy:
the relative power bracket is at most `5e-7/max(1,a*mean(Y))`, and the
relative q enclosure is below `1e-6`. This supports golden accuracy; it
does not establish a tighter requested coefficient tolerance for that
delegate. Parent failures remain failures. A rounded transformed support
endpoint stops numerically.

The binding-neutral route preserves budget, numerical and tie outcomes,
including a parent budget with no finite witness. Candidate coefficients
use `(p,q,a,b)`, and likelihood bounds include the transformation shift.
Finite witnesses on stopped searches remain diagnostic; their global upper
bound is infinite when the search is unfinished. Family-specific failure
reasons distinguish shape solves, shape signs, tail bounds, power-root signs,
coefficient inclusion and arithmetic failures.

Distinct observations can have identical rounded logarithms. Such a sample
now stops numerically when concentration remains free, instead of reporting
constant-sample unboundedness. Actual constant samples retain the existing
proved unboundedness stop when concentration is free.

## Uniqueness and additional proved bounds

At held q=1, Y has exponential rate p*a. For `Y=(1/6,1/2)`, every positive
pair with `p*a=3` is a global maximizer. For example `(p,a)=(1,3),(3,1),
(6,1/2)` are distinct maximizers. The existing `a=1` convention selects an
identified representation and counts one free coefficient. This is an
attained identification ridge, not a fitting failure caused by singular
information. No general uniqueness theorem for the held-p pair is claimed;
the interval search permits multiple maxima and comparison ties.

Further exact-arithmetic inequalities were derived for the remaining
free-p profiles. At a conditional two-shape beta maximum, explicit shape
rectangles follow from the two sufficient-statistic contrasts. They bound
q uniformly in the small-power region and give a quantitative upper bound
against the correctly held or unrestricted gamma face. A separate bound
handles the moving power endpoint, including joint p-to-zero/q-to-infinity
paths, and provides a signed sufficiently large-power neighborhood below
that face. There is also a signed fixed-endpoint power neighborhood for
held q<1. Proofs and their limitations are in
[remaining-profile-bounds.md](2026-11-07-genbeta-cert-data/remaining-profile-bounds.md).
These additional bounds are **not implemented in the remaining fitters**.

## Independent evidence and timings

Evidence types are separate:

- The proofs establish concavity, tail exclusion, interval coverage and
  coefficient inclusion in exact arithmetic, conditional on computed comparisons.
- The [scalar oracle](2026-11-07-genbeta-cert-data/scalar-oracle.json) checks
  36 roots at 80–360 decimal digits, including tiny held q, the critical
  p=1/2 quadratic score and the large-q exponential tail. Its maximum relative
  coefficient error was `8.04e-14`. These extreme checks use iterlim=1000.
- The 70-digit stationary-coefficient oracle checks five held-p fits on the
  frozen sample. On [simbox](2026-11-07-genbeta-cert-data/pair-oracle-simbox.json),
  the maximum measured relative error was `2.04e-10`; the
  [workstation double-precision check](2026-11-07-genbeta-cert-data/pair-oracle-local.json)
  also passed the `1e-9` threshold. This oracle checks coefficients and
  likelihoods; the global claim comes from the separate interval proof.
- Independent 70-digit checks passed for the
  [held-p bounds](2026-11-07-genbeta-cert-data/bounds-check.json) and
  [16 remaining-profile cases](2026-11-07-genbeta-cert-data/remaining-bounds-check.json).
  Numerical checks are consistency evidence, not substitutes for the proofs.
- Golden regression found no change to the default fit or golden files.

Mean milliseconds per fit, 30 repetitions on the frozen n=500 sample,
simbox GCC 13.3, C++17 `-O2`, under nice 10:

| Route | Before | After |
|---|---:|---:|
| Default p,q,a free | 2.713 | 2.629 |
| Power only, p=2,q=0.7 held | 0.124 | 0.163 |
| p=1 held; q,a free | 1.941 | 0.645 |
| p=2 held; q,a free | 12.826 | 1.004 |

These are shared-machine timings, not a formal performance experiment.
The [timing script](2026-11-07-genbeta-cert-data/timings.py) and
[recorded results](2026-11-07-genbeta-cert-data/timings-simbox.json) use the
[frozen starting fitter](2026-11-07-genbeta-cert-data/mle-baseline.hpp.txt).
Run `timings.py` through a CPU slot to rebuild both versions without git writes.
The evidence directory also retains the independent C++ emitters and Python
checkers; compile emitters as C++17 with `-O2`, the library include directory
and the installed BH/Boost include directory. The workstation pair check
additionally defines `BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS`.

## Final validation

All builds and tests ran in the foreground through the prescribed CPU or
simbox wrappers, with at most four local slots and one BLAS/OpenMP thread.
The simbox suite commands explicitly used four build/test workers.

| Final run | Machine | Result |
|---|---|---|
| `just cpp-test` | simbox, GCC 13.3 | 106/106 |
| `UMVL_TEST_TIER=full just cpp-test` | simbox, GCC 13.3 | 129/129 |
| `just cpp-test-no-long-double` | simbox, GCC 13.3 | 129/129 |
| `just cpp-test-no-long-double` | workstation, GCC 15.2 | 129/129 |
| `just test` | simbox | FAIL 0, WARN 0, SKIP 154, PASS 21532 |
| `just test-full` | simbox | FAIL 0, WARN 0, SKIP 0, PASS 44347 |
| `just js-build`, `just js-test` | workstation, WebAssembly | 496/496 |
| `just site-check` | workstation | Green; 99 generated C++ snippets and site checks |
| Focused normal-width genbeta doctest | workstation | 24 cases, 3421 assertions |
| Local Markdown links | workstation | 16 resolve |

The [validation summary](2026-11-07-genbeta-cert-data/validation.json) records
the machines and numerical results. The site check's unrelated historical
screenshot was restored. No shared core header, settled d/p/q/r, special
function, truth fixture or code generator was changed. Shared JS tests were
appended only. No board edits, package installs, pushes or publishing occurred.

## Open entries

**GENBETA-PA — free p,a with q held, q!=1.** The legacy profile and face
selection remain numerical. Uniform unsigned gamma/power bounds and a
signed large-power bound for held q<1 are now proved, but are not wired into
this route. Missing are validated conditional-p enclosures, a complete
compact power search with coefficient inclusion, and a signed neighborhood
for a winning gamma face. The known quadratic seam coefficient can be
positive, negative or zero; a negative local coefficient alone does not
establish global face dominance. A zero coefficient requires higher-order
control or an explicit unresolved stop.

**GENBETA-PQA — free p,q,a.** The default mesh and its selected gamma/power
statuses remain uncertified globally. The new bounds control the small-power
conditional shapes and the moving-endpoint large-power region, but the
implementation still lacks a certified two-shape beta conditional solve
and conjugate upper enclosures, complete compact power coverage and joint
coefficient inclusion. A winning small-power gamma face still needs a
signed uniform neighborhood bound, including degenerate seam cases.
Nearly tied faces require explicit comparison margins or a correct
tie/numerical stop. These missing pieces prevent replacing the retained
mesh and prevent setting the family's certified flag to yes.

Both open routes also retain the fixed 240-point proposal mesh. It is not
a certificate ledger whose coverage increases with iterlim; replacing it
belongs to the open work. The new held-p certificate has the prescribed
scalable ledger and no fallback to that mesh.

## Artifact display

The requested report and its evidence are retained in this worktree. The
artifact viewer rejected `show` with HTTP 403 because the worktree is outside
its configured readable roots. Displaying it in that pane requires the viewer
to recognize this worktree as an agent-thread root or an explicitly allowed root.
