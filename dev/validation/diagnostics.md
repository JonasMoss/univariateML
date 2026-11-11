# Core fit diagnostics (TASK-132, 2026-10-31)

A successful result describes finite coefficients in the dossier's parameter
domain and a finite log likelihood. Empty samples and observations outside the
support must carry a non-success status. IEEE NaN is the hold API's free-coordinate
sentinel, not a valid observation. An integer span cannot represent fractional,
NaN or infinite observations; conversion validation belongs to the bindings.

Finite proper discrete endpoint members remain successes (for example Poisson
lambda = 0 and geometric prob = 1). An open continuous scale endpoint is not a
proper member: a constant normal sample has unbounded likelihood as scale tends
to zero. Under a one-to-one transform, this status is preserved together with
the likelihood Jacobian. In particular, constant positive lognormal and constant
interior logit-normal samples inherit normal's `Unbounded` status. Fixing scale
can make those same samples estimable, so no unconditional constant-data guard
belongs in these fitters.

Gamma/beta `InvalidInput` distinguishes invalid observations from `Degenerate`
(valid constant data with gamma shape/scale free or both beta shapes free) and `IterationFailure`. Their delegate
results preserve non-success. `GammaStatus::TransformFailure` identifies lost
reciprocal/squared-data representation in inverse-gamma/Nakagami delegates;
`InvExpStatus::IterationFailure` identifies unrepresentable reciprocal moments.
Valid original observations are not relabelled invalid merely because their
transformation exceeds the working range. Joint free-shape/scale constant
positive samples retain analytic `Degenerate` status before transformation;
nonconstant samples collapsed by rounding carry `TransformFailure`. Existing numerical stopping criteria and successful
fitting arithmetic are unchanged. The audit does not certify existence or global
optimality of an optimizer's finite result: it checks the narrower status contract.
A failed numerical calculation must never acquire `Ok` through aggregate defaults
or a transformation.

`diagnostics.py` generates a standalone C++ sweep from canonical dossier/golden
metadata and shared binding call routing. It removes binding guards in the
validation-only routes so invalid data reach the actual core, and observes the
result before binding failure handling. Supported hold subsets use the exact structural exceptions enumerated by the
dossiers; fixing extra coefficients can make a previously unsupported fit valid. Each held value
comes from the frozen golden. Output records family, held set, input class,
status (existing enum integer, -1 means no status), estimate/log-likelihood
finiteness, parameter-domain membership and the defect flag. Finite closed
parameter endpoints are accepted; excluded endpoints are defects.

Regenerate with `python3 dev/validation/diagnostics.py --generate build/diagnostics`.
The generated `audit.cpp` is a standalone translation unit, requiring only core
headers and Boost. The checked-in generated sweep runs in the monorepo C++ dev
tier; generation itself is not a dependency of standalone core consumers.
See the [diagnostics report](../research/evaluations/2026-10-31-diagnostics-report.md)
for tested revision, commands, timings, defects and coverage limits.

## Wrapper failure handling (rule R6)

Iterative failure is an error ([parameter rules](../roadmap/2026-10-29-parameter-rules.md), R6).

- Preserve failure statuses through transformations and delegates, and stop in
  the adapter with the status and a clear diagnostic.
- Keep a failed solve only when its finite free-coordinate score has Euclidean
  norm below 1e-3 in fitter coordinates, or passes the family's documented
  score gate. Use the total likelihood score, with any objective normalization
  undone.
- Invalid starts and nonfinite results stop.
- A rescue solve must meet its own convergence or score criterion; never return
  an unchecked last iterate.
- Every loop cap is a multiple of `min(iterlim, 100000)` by work unit;
  exhausted search or enumeration work without a successful candidate stops
  ([work budgets](../design/work-budgets.md)).

A certificate that cannot close a global comparison or coefficient inclusion
returns the successful search result with `certified = FALSE`, including when
its certificate allowance expires. The flag withdraws only the global-optimality
claim; the [accuracy contract](../design/accuracy-contract.md) holds for every
`Ok` fit, certified or not. Stop when a separated feasible likelihood bound refutes the
candidate (a reachable face can supply that bound), the search fails, inputs are invalid, or mathematics proves
nonexistence, a strictly winning limit, or distinct tied maxima. The C++ result's
`bool certified` travels through the common binding's `extra {"certified",0/1}`.
Shared R `common_result` supplies a logical element and the estimates attribute;
JS carries 0/1. No per-family R conversion is needed.

This replaced the nlminb-style rule that returned whatever BFGS landed on, which
let `mlstd` return its start values (TASK-77). The score gate checks local
stationarity, not existence or global optimality.
