# Structured fit stops (TASK-141, 2026-11-04)

`core/stop_detail.hpp` defines the header-only, string-free `FitCandidate` and
`StopDetail`. Append `StopDetail stop_detail` to a family result, preserving the
positions of existing aggregate fields. Existing statuses and success decisions
remain authoritative. Default payloads are empty with NaN scalars.

A finite candidate's coefficients follow the stopped family's public order.
A face candidate uses `StopModel` and that model's public coefficient order.
This extra enum is needed because a tie or budget stop can compare different
models (for example Pareto, Weibull and a finite Burr witness). The binding
translates enums to model ids and coefficients to public names. No binding
refits a limit or reconstructs a search.

`loglik_lower` and `loglik_upper` on a candidate enclose its evaluated total
log likelihood (or the fitted face's supremum enclosure), in original data
units, including Jacobians. They are not global bounds. The same fields on
`StopDetail` bound the global supremum when available. NaN means unavailable;
positive infinity means no finite upper bound has yet been certified. Do not
substitute a profile's local upper bound for the global upper bound.
`segment_loglik_upper` preserves Burr's last unresolved interior segment bound
when available; it does not bound unclosed tails or other segments.

`limit_coef` and `limit_loglik` describe the fitted face selected by the status.
They are separate from the stopped family's coefficients. Cauchy NoMLE and
unbounded constant samples have no candidate estimate. A budget of zero can
stop before any candidates exist. Candidates are diagnostic witnesses, never
successful estimates or a guarantee that all maxima have been enumerated.
A `tie` includes numerically unresolved comparisons and sign-uncertain seams;
it does not assert an exact mathematical tie.

## Adoption and numerical contract

Cauchy's held-scale tie retains all isolated candidates whose likelihood upper
bounds overlap the best lower witness, with the existing root-width error and
likelihood margin. Burr retains its two fitted faces, isolated root witnesses and its best evaluated
finite witness, including at certificate exhaustion; its global upper bound is
conservatively infinite because early stops can leave tails unclosed. Reciprocal
Burr transports coefficients and every likelihood bound with the Jacobian.
Lomax retains the exponential fit on a limit, and finite/face witnesses plus a
global bound on budget exhaustion. Its upper bound includes the popped segment,
remaining queue, root enclosure and the existing tail margin. Held-power Burr
transports the Lomax exponential face to Weibull in original units.

These changes report existing mathematics; they do not strengthen existence or
uniqueness claims. Bounds use the fitters' existing practical floating-point
margins, conditional on computed comparisons, rather than outward-rounded
interval arithmetic. See the family dossiers for the exact-arithmetic
certificates, numerical margins and unresolved guarantees.

## Bindings

`bindings/common/fit.hpp` carries `error_kind` (`tie`, `limit`, `budget`,
`nomle`), a limit `model` id, named candidates, named limit estimates and bounds.
Unclassified input and numerical failures keep their existing ordinary errors.
Successful fits and all printed messages retain their existing contract.

The R adapter returns a condition to the decorator, which signals it with base
R `stop`. Classes are `univariateML_<kind>`, `univariateML_stop`, `error`,
`condition`. Fields are `message`, `call`, `kind`, `candidates`, `model`,
`estimates`, `limit_loglik`, `loglik_lower`, `loglik_upper`, `segment_loglik_upper`. `candidates` is a
data frame with `model`, the union of named coefficient columns (NA for other
models), and likelihood bounds. Empty model denotes the stopped family.
`estimates` is the named limit coefficient vector. Internal Rcpp entry points
return the condition; public fitters signal it. No package dependencies are
needed for catching these errors.

The JS facade throws `Error` with the same fields (apart from R's `call`);
`candidates` is an array of row objects and `estimates` an object. Missing
coefficient keys replace R's NA entries. `warnings` retains its existing role.
NaN and infinity are native numeric values, not JSON-safe serialization.

For a new family: fill the payload where the evidence is computed; preserve it
through delegates and transformations; classify the existing stop in its common
fit header; add core and public-binding tests; retain existing message text.
