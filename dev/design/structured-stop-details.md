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
`nomle`, and for delegates `numerical` or `unresolved`), a limit `model` id, named candidates, named limit estimates and bounds.
Unclassified input and numerical failures keep their existing ordinary errors.
Successful fits retain their existing contract.

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

## Certified delegates (TASK-116.1, 2026-11-05)

Pareto II and III use Lomax and log-logistic on excesses, respectively.
Pareto IV transports Burr interior witnesses by appending the held threshold;
its face coefficients remain those of the model fitted to excesses, as named
in the diagnostic (`mlweibull(x - min)` or `mlpareto(x - min)`). Translation
has unit Jacobian and preserves all likelihood bounds.

Held-power Burr uses `y=exp(c*(log(x)-M))`, `M=max(log(x))`. The total
log Jacobian is `n*log(c)+(c-1)*sum(log(x))-c*n*M`. A Lomax witness
`(lambda,kappa)` becomes `(kappa,c,exp(M-log(lambda)/c))`; an exponential
rate becomes a Weibull scale `exp(M-log(rate)/c)`. The same additive shift
applies to candidate intervals, global bounds, selected-face likelihood and
unresolved-segment bound. A transformed positive observation that underflows
to zero stops with NumericalFailure. Inverse Burr applies its reciprocal
transport after this map, retaining certificate failure categories.

Inverse Pareto swaps Lomax `(lambda,kappa)` to `(shape,scale)=(kappa,lambda)`
and adds `-2*sum(log(x))` to every likelihood field. The exponential face
becomes `StopModel::InvExp`, with public inverse-exponential `scale=rate`.
Unit held beta shapes in beta-prime and GB2 transfer these certificates,
with witnesses reordered into each public coefficient order. Feller–Pareto
appends its held threshold to GB2 interior witnesses. A free beta shape that
happens to fit near one does not qualify for this reduction.

These transports preserve the parent certificate conditional on computed
comparisons and its practical floating-point margin. They do not certify the
held-scale two-free-shape Burr profile, constrained equal-shape paralogistic
profiles, free-location Fréchet profiles, or general GB2/beta-prime profiles.

## Discrete sweep (TASK-141.2, 2026-11-06)

`StopDetail::limit_model` records the selected face's model in the core.
Existing callers can still supply an explicit model id to `stop_failure`.
Discrete laws without a package family are labeled `unavailable:point-mass`,
`unavailable:two-point`, or `unavailable:translated-negative-binomial`, following
the `unavailable:` convention below. Their coordinates are respectively `(point)`,
`(lower, upper, prob)` (probability of upper), and `(size, prob, shift)`.
The latter adds shift to a negative-binomial failure count. These descriptive
labels are not invented package ids and cannot be dispatched as fitted models.
A two-point law on {0,1} uses the package binomial family `(size=1, prob)`.

Core face coefficients use each target model's public order. Unresolved
comparisons retain diagnostic face and finite witnesses and an infinite
global upper bound where the tail is not excluded. `unresolved` does not
assert that the face wins. `budget_exhausted` distinguishes explicit work-budget guards from numerical
failures inside an otherwise shared legacy status. Such guards become budget
conditions without changing the existing status or message. Mixed numerical
or iteration failures whose cause is not distinguished remain ordinary errors.
No binding refits a face. The settled shared CMP implementation is unchanged;
its dossier-level entry point adds payloads to its existing stop statuses.

## Location-scale t certificate (TASK-140, 2026-11-06)

All-free lst/std certificate stops distinguish `budget`, `tie`, `numerical`,
normal and ordinary-scale t2 `limit`, and unbounded/collapse `nomle`.
Certificate failures are terminal and never use an optimizer-score exception.
`StopModel::Norm` candidates have `(mean,sd)`; `StopModel::Lst` candidates
have ordinary-scale `(mu,sigma,df)`, including a positive-scale t2 face.
This avoids reporting the t2 face as a finite standard deviation.

A selected bounded scale-collapse path uses the descriptive label `unavailable:t-scale-collapse`
with `(mu,df)` and its limiting likelihood. This id denotes a path, not a
public density family or a continuous density assigned to an atomic weak
limit. There is no fitted zero scale among its diagnostic coefficients.
Unbounded collapse has no finite face estimate. Original-unit likelihood
bounds include the normalization Jacobian. Missing global upper bounds stay
infinite or unavailable; a last segment bound is not a global bound.

## Continuous-face transport (TASK-141.1, 2026-11-06)

`limit_detail` retains a selected face and its evaluated likelihood as a
candidate. It does not supply a global supremum bound. A comparison payload
marks numerical ambiguity, retains the compared witnesses and uses an infinite
global upper bound when a closed global certificate is unavailable. Exhausted
half-t, skew-normal and slash subdivision certificates likewise retain their
computed witnesses with an explicitly unclosed upper bound; arithmetic and
iteration failures do not become budget stops.

Face construction checks representable model domains, including strictly positive
scales/shapes and ordered uniform endpoints. A scale that underflows to zero is
not a fitted law, even if its log-coordinate likelihood is finite. Binding
classification checks finite input, valid controls and finite evaluated witnesses. For families whose only unbounded concentration is a constant sample,
`nomle` additionally requires equality in the original observations. Rounded
logarithms or reciprocal transforms of distinct observations must not fabricate
a non-existence claim. Exponential non-existence specifically requires all zeros.
Genuine nonconstant scale-collapse cases in the heavy-tail families keep their
existing multiplicity conditions.

The new model tags cover normal, uniform, half-normal, Laplace, lognormal,
Azzalini skew-normal and half-t. Binding names follow each model's public
order (in particular uniform `min, max`, Power `alpha` endpoint then `beta` shape,
and half-t `mu, nu, sigma`). A model
string beginning `unavailable:` describes a law without a matching package
family; it is not an id that can be passed to a package fitter. These tags
include translated/reflected exponential, truncated exponential, reflected
half-normal/half-t, unit-gamma and half-GED. Unit-gamma means
`X = upper * exp(-Y)` with `Y ~ Gamma(shape, scale)`. Half-GED means
`X = mu + side*Y`, with density
`nu/(scale*Gamma(1/nu))*exp(-(Y/scale)^nu)` on positive distances. Reflected
models report their endpoint in the original coordinates, not on negated data.

EMG and Bhattacharjee retain the face parameters computed by their standardized
searches and restore the scale Jacobian. The original face values include any
held-endpoint penalties already used by the fitter; such a value need not equal
the closed-support limit density evaluated at an observation on its endpoint.
Gompertz's exponential rate is its held `a`, or reciprocal sample mean.
Huber's normal/Laplace faces retain their own conditional location and scale,
rather than the last finite Huber iterate. Transformed gamma's lognormal face
uses the mean and population standard deviation of log data; its power shape
is the reciprocal mean log distance from the endpoint. Inverse transformed
gamma negates the lognormal mean and maps power to Pareto, including the
reciprocal Jacobian. Generalized beta retains the gamma fit on
`log(scale/x)` and includes `-sum(log(x))` in its face likelihood.

SGED's free search now retains the half-GED scale while computing its profile.
For held moments, `sged/face_payload.hpp` recovers the selected base coordinates
without changing the original shape search or a successful fit. With mean held
and scale free, put `u=1/scale` and
`m=Gamma(2/nu)/Gamma(1/nu)`. Apart from constants the negative log likelihood is
`-n*log(u)+sum((m+side*(x-mean)*u)^nu)`, on the interval imposed by nonnegative
base distances. For `nu >= 1` its second derivative is strictly positive:
`n/u^2 + nu*(nu-1)*sum((x-mean)^2*(m+side*(x-mean)*u)^(nu-2))`.
A bracketed score root, or the support endpoint when the score stays
nonpositive, therefore determines the global scale minimum in exact arithmetic.
For `0 < nu < 1`, let `d_i=side*(x_i-mean)` and
`H(u)=u^2*F''(u)=n-nu*(1-nu)*sum((d_i*u)^2*(m+d_i*u)^(nu-2))`.
Every summand is strictly increasing on the support: its derivative has the sign
of `2*m+nu*d_i*u`, which is positive because `d_i*u >= -m` and `nu < 1`.
Thus `H` is strictly decreasing and the score has a single turning point.
At a finite support endpoint the score tends to negative infinity; the global
minimum is therefore either its first root or that endpoint. With no finite
endpoint, the cost tends to positive infinity and the score approaches zero
from above, so its first root is the unique global minimum. This settles the
previously unresolved subunit-shape scale profile without changing the legacy
shape search or a successful estimate.

The implementation uses 100 bisections for each monotone solve, a
`128*epsilon` score-comparison margin at the turning point, and `1e-7*n`
likelihood agreement with the existing face value. Numerically unresolved
root-versus-endpoint likelihood comparisons within `1e-7*n` retain both
candidates as a `tie`. Arithmetic failures retain ordinary errors. Candidate
bounds enclose both the original comparison value and the independently
evaluated likelihood at recovered coordinates. These are exact-arithmetic
profile certificates conditional on computed comparisons with the stated
practical floating-point margins, not outward-rounded interval arithmetic.
Analytic shape-1/2 cases and a mixed-sign two-root case are appended to SGED's
C++ tests; R and JS also check the public held-mean subunit-shape face.
Existing *shape* searches and their numerical global-search qualifications are
unchanged.

The seed-7, n=80 skew-t equivariance workload was independently rebuilt against
base `164d4178` and the new headers on simbox with GCC 15. Both versions produce
bit-identical fits: df is 1272.4538444838495 in original units and
1272.1797791970282 after multiplying observations by 10000 and translating them.
The relative df discrepancy is about `2.154e-4`, while the restored total
likelihoods differ by less than `1.1e-11`. The R equivariance check therefore
uses `3e-4` for this near-normal df coordinate, retaining its independent
`2e-8` per-observation likelihood check and the existing tolerances on the other
coordinates. The sample, comparison program, baseline reconstruction and output
are retained under `dev/research/evaluations/2026-11-06-stops-cont-data/`.

Conditional two-piece normal arithmetic ties retain the finite standardized
fit and the compared half-normal face. Skew-t retains the actual finite profile
witness used as its reference (including the location-scale profile witness
when it exceeds the joint search), alongside a skew-normal or half-t/half-normal
face. A failed limiting-family subdivision search in skew-t transports its tie
or budget witnesses and bounds through the outer affine Jacobian and any held
endpoint penalty. A simultaneous plain numerical failure stays ordinary. The
large-held-magnitude snorm and skew-t tie checks independently evaluate the
reported half-normal and half-t densities in original coordinates.

## Accuracy stops

Accuracy is independent of the family status enum: `StopDetail::accuracy_stop`
distinguishes an explicitly unrepresentable tolerance from measured conditioning.
Both retain the finite candidate in public coordinates. Historical family status
names and message text do not classify the cause; consumers inspect the kind. Bindings use `tolerance`
and `conditioning`; R additionally inherits `univariateML_numerical`, preserving
existing numerical-error handlers. `attainable_tol` travels as a numeric field;
a tolerance message also gives it in round-trip decimal precision.

A failed iteration or nonpositive information matrix alone does not establish
conditioning. Those search failures remain numerical unless a propagated score
noise bound establishes the accuracy limitation. Nonfinite evaluations do not
become tolerance errors. Held coordinates are excluded from resolution checks.
The numerical margins are practical floating-point estimates, not interval
arithmetic certificates. A successful search whose global certificate cannot
close remains uncertified under the existing fallback rule.
