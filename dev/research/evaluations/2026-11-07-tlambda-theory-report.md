# Certifiable structure for Tukey lambda — TASK-116.17

Source revision: `681d7ac5ce6965db4066b4117c12eff69e75cd0c`, branch
`codex/tlambda-theory`. This research adds only this report and its evidence
directory. No fitting code, tests, goldens, bindings, dossier, roadmap, board
or existing tracked file was changed. No commit, installation or publication
was made.

Tukey lambda has sufficient structure for global likelihood-value certificates
on every free subset in the strict existence regimes, without assuming a
unimodal shape profile. Fixed nonnegative shape gives concavity; explicit
inverse-quantile brackets and uniform density envelopes give finite search
regions for the other cases. Some collapse equality decisions are also
settled exactly. The remaining equality questions are identified below;
they must not be replaced by a numerical tolerance tie.

The [proofs, equations and algorithms](2026-11-07-tlambda-theory-data/proofs.md)
and [complete eight-subset inventory](2026-11-07-tlambda-theory-data/subsets.md)
are part of this deliverable. The [reproducer](2026-11-07-tlambda-theory-data/check.py),
[computed evidence and source fingerprints](2026-11-07-tlambda-theory-data/checks.json),
and [finished validation record](2026-11-07-tlambda-theory-data/validation.json)
are retained alongside them. Proofs establish the mathematical claims;
finite grids and optimizer proposals are supporting numerical checks only.

## Proved reductions

Write

\[
 Q_l(u)=\frac{u^l-(1-u)^l}{l},\qquad
 D_l(u)=u^{l-1}+(1-u)^{l-1},\qquad
 g_l(Q_l(u))=D_l(u)^{-1},
\]

with the logistic limit at l=0. Quantile inversion is single-valued because
D is positive. For the smaller tail p, the magnitude h_l(p)=-Q_l(p)
is strictly decreasing in both p and l. Explicit finite brackets cover
negative, zero and positive shapes, including support deficits; they permit
likelihood enclosures rather than unchecked inversions. Proof section 1
gives the brackets and how to propagate parameter boxes through them.

For **fixed 0<=l<1**, the standardized density is strictly log concave.
Consequently the log likelihood is concave in
`A=1/sigma, B=-mu/sigma`. A held location imposes B=-mu*A, an affine
constraint; a held scale fixes A. Conditional location, scale and joint
location-scale fits therefore have unique finite maxima whenever the
existence conditions hold. This remains true above the DQM threshold;
it is a likelihood result, not an inference regularity claim.

For **fixed 1<l<2**, log density is strictly convex in standardized
location and increases with absolute residual. Held-scale location fitting
reduces to comparing the two feasible support-contact endpoints. Held-location
scale fitting has its exact optimum at `sigma=l*max|x_i-mu|`. Joint fitting
reduces to the two extreme-observation contact curves. Shapes 1 and 2
are exact uniform problems, with halfwidth sigma/l. Held shapes above 2
remain covered by finite box searches with explicit contact strata.

For **negative shapes**, tail log convexity invalidates the concavity
argument. Separate concavity of the latent objective does not rescue it:
the equations x_i=mu+sigma*Q_l(u_i) impose nonlinear constraints. Neither
global joint concavity nor shape-profile unimodality is claimed.

## Uniform exclusion of escapes

On the fitted branch [-1,1] the proved bounds are

\[
 g_l(z)\le\tfrac12,\qquad z^2g_l(z)\le1,\qquad
 f(x)\le\min\{(2\sigma)^{-1},\sigma/(x-\mu)^2\}.
\]

Holder interpolation between shapes -1 and 1 proves these uniformly;
the constants are not numerical envelope maxima. They exclude diverging
scale and location using an incumbent likelihood, and exclude small scale
under the strict tie condition. The distinct-data spacing gives an explicit
lower scale bound through powers sigma^(n-2k), not a guessed cutoff.
Held negative shapes have the corresponding exact power
sigma^((n-k)/|l|-k). Proof sections 3–4 give every constant and finite
superlevel bound.

The unbounded negative-shape domain when **mu is held** requires an extra
argument. Put l=-a. With sigma held, the modal density bound
`2^(-a-2)/sigma` excludes a->infinity uniformly. With sigma free and no
observation equal to mu,

\[
 |x-\mu|f(x)<\frac1{2a},\qquad
 L<(2a)^{-n}\prod_i|x_i-\mu|^{-1}.
\]

This excludes the joint shape/scale escape, gives an explicit data-dependent
lower shape cutoff, and proves attainment after ordinary nuisance exclusion.
If any observation equals held mu and both scale and shape are free,
a sufficiently negative fixed shape gives unbounded scale collapse.
These two cases exhaust this negative-shape end. A fixed start grid ending
at -16 does not establish either result.

For 0<l<1, an observation approaching a support endpoint at distance delta
has density asymptotic to
`sigma^-1*(l*delta/sigma)^((1-l)/l)`, hence zero endpoint likelihood.
For held l>1 the endpoint density is 1/sigma and contacts must be enumerated.
At l=1 it is 1/(2sigma). Joint approach from below cannot exceed that
uniform value, even when `(1-l)*|log p|` has a nonzero limit. This proves
upper semicontinuity at the uniform contact corner; it does not assert
continuity or a uniform fixed-shape endpoint expansion. See proof section 7.

## All free subsets

The detailed inventory states arbitrary held-shape exceptions, support
feasibility, constant samples and exact tie thresholds. Its main routes are:

| Free coefficients | Route |
|---|---|
| none | Closed-support likelihood evaluation, with bracketed inverses. |
| mu | Concavity for nonnegative principal shape; exact uniforms; two contact endpoints for 1<l<2; finite scalar boxes otherwise. |
| sigma | Concavity where applicable, exact support reductions, or finite scalar boxes; held-location negative-shape collapse equality is settled analytically. |
| lambda | With mu and sigma held, explicit negative-shape cutoff and scalar boxes, including exact uniform face. An attained maximum exists. |
| mu, sigma | Concavity at fixed 0<=l<1; exact uniforms; contact searches for held l>1; negative-shape boxes with collapse-face decisions. |
| mu, lambda | Held scale, compact fitted shape range [-1,1], uniform location exclusion and two-dimensional boxes. An attained maximum exists. |
| sigma, lambda | Held mu: ties there imply unboundedness; otherwise joint shape/scale exclusion and two-dimensional boxes prove attainment. |
| mu, sigma, lambda | Three-dimensional boxes on [-1,1] under strict ties, with both shape faces and exact uniform candidate; equality faces as below. |

The mathematical sample scope exceeds the current sufficient tie guard.
For example, a constant sample away from held mu has an attained fit with
sigma and lambda free. This report does not change that guard or authorize
altering default results.

## Collapse equalities and nonuniqueness

At held l=-a<0 and a collapsing observed value t of multiplicity k, define
r=1+1/a. When `(n-k)/a-k=0`, the face likelihood is

\[
 F_t(a)=2^{-(a+2)k}a^{-r(n-k)}
                 \prod_{x_i\ne t}|x_i-t|^{-r}.
\]

With mu=t held, a strict tail inequality proves every positive-scale
likelihood is below F_t(a). The supremum is unattained; no numerical
interior comparison is needed. Negative and positive collapse exponents
give unboundedness and vanishing likelihood, respectively.

With **l=-1 held and mu free**, the sharper identity
`D_-1-Q_-1^2=2/[u(1-u)]>=8` gives
`f(x)<=sigma/[(x-mu)^2+8sigma^2]`. A two-point product identity then
proves that any half-tie equality sample has likelihood strictly below
its collapse face at every finite mu,sigma. This closes the entire
held-l=-1 equality case.

With **shape and location free**, half-tie faces occur only at l=-1 and
have values F_t(1). Explicit neighborhood bounds exhaust these faces,
including shape approaching -1 at rate 1/|log sigma|. A finite likelihood
strictly above all face values establishes attainment and finite search
bounds. Balanced two-cluster samples always have such an improvement:
the uniform likelihood exceeds either face by a factor 8^k. The general
exact decision when there is no established strict improvement remains open.

Uniqueness cannot be assumed across hold subsets. Three exact counterexamples
are proved in section 10:

* l=1, sigma=1, data (-1/4,1/4): an entire interval of optimal locations.
* l=3/2, sigma=1, the same data: exactly two optimal locations, -5/12 and 5/12.
* l=-1, sigma=1, data (-80/9,80/9): the midpoint is a strict local minimum;
  symmetry and vanishing tails imply at least two distinct global maxima.

The third counterexample lies inside the fitted branch and involves no
uniform alias or plateau.

## Certificate implementation contract

Start with a positive feasible incumbent. Derive shape, scale and location
bounds from the proved envelopes; intersect them with holds and support.
Evaluate exact uniform and eligible contact/restriction strata separately.
For each remaining box, enclose inverse tails monotonically and lower-bound
D to upper-bound every density. Local optimizers supply feasible proposals
only. Fair subdivision and relative-to-stratum dense proposals produce a
finite likelihood-gap certificate for any positive value tolerance on the
compact strict-existence region. This needs no finite-simple-root assumption.
The proof, pseudocode, interior scores and implementation order are in
sections 8–11.

Success certifies an incumbent/global-upper-bound gap, not unique coordinates
or an exact root list. Excluded collapse faces need the additional existence
decision above. A tolerance tie with an excluded face is not proof of
nonattainment. Unresolved comparisons must stop with their witnesses.
Use the shared `iterlim` budgets, with 64-bit counts, and report budget
exhaustion distinctly from numerical failure. There is no fixed numerical
shape or scale cap in the proposed algorithms.

The intended standard is exact-arithmetic certification conditional on the
computed comparisons, with a declared practical floating-point margin.
The proofs propose a conservative epsilon-scaled margin as an engineering
starting point; its adequacy has not been validated for a C++ implementation.
Unresolvable signs, inversion brackets or overflow must stop. Outward-rounded
interval arithmetic is not required by this route. Certification-specific
enclosures can be added without changing the settled d/p/q/r; no change to
`dpqr.hpp`, `special/`, truth files or code generation is required here.

## Precise open entries

1. **Free shape and location, half-tie equality:** if no finite value strictly
   exceeds max F_t(1), prove nonattainment or decide whether a finite tie
   exists. The missing ingredient is an analytic global comparison or an
   effective exact critical-value decision with free real exponents.
   The envelope and face bounds alone do not decide this. Retain a numerical
   comparison failure rather than mislabeling it CollapseLimit or success.
2. **Other held negative shapes with free location and equality:** a practical
   exact comparison engine is missing. For rational nonzero held shape,
   quantile constraints, scores and likelihood become algebraic after
   adjoining positive roots and clearing positive denominators. This gives
   a finite exact decision route for rational data/holds, including binary64
   inputs, but degree can be enormous; no practical header-only solver is
   supplied. General real held shapes lack that algebraic reduction.
   Held mu equality and held l=-1 equality are already settled above.
3. **Numerical implementation and cost:** the box certificate is specified
   and its positive-tolerance termination proved, but its runtime and
   floating-point margin are unmeasured. Implement and validate inverse
   enclosures, contact boxes and shallow comparisons before changing the
   dossier's certification tag. No bound on shape-profile mode counts or
   general uniqueness is proved; neither is needed by the box route.

Exploration of nine equality samples found converged feasible witnesses
below the corresponding face values. Those searches used finite proposal
boxes and are explicitly not global upper certificates or evidence deciding
the open entries. The exact l=-1 inequality, not those searches, settles
the l=-1 cases.

## Finished validation

All runs below finished with exit code zero before this report was written.
They used the workstation, the required slot wrappers, nice +10, and one
BLAS/OpenMP thread. Builds/dev suites reserved four slots; research checks
reserved one. Shared-slot waits were allowed to finish. No simbox run or
fallback was used, and no command remains running.

| Final command | Result |
|---|---|
| `/tmp/umvl-cpu/run4 just cpp-test` | 106/106 dev shards passed; ctest wall time 63.10 s. |
| `/tmp/umvl-cpu/run4 just test` | FAIL 0, WARN 0, SKIP 154, PASS 20983; R elapsed 110.173 s. |
| `/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-07-tlambda-theory-data/check.py` | All exact-sign, 120-digit envelope/inverse/limit checks passed; nine exploratory proposal searches converged. |
| Final fingerprint/log-summary verification through `run` | All research and family-source fingerprints matched; both dev summaries verified. |

The numerical workloads include 80 principal-branch points, 160 extended-shape
envelope points, 70 inverse brackets/joint-escape checks, a held-location
equality limit, and a noncommuting uniform-contact limit. Exact rational
checks include the negative-curvature counterexample and the sharper l=-1
envelope. `checks.json` records seeds, proposal boxes, optimizer outcomes,
evaluation counts, environment and SHA-256 fingerprints. The final environment
used GNU C++ 15.2.0, R 4.6.1, Python 3.14.5, mpmath 1.3.0, NumPy 2.4.6
and SciPy 1.18.0 on Ubuntu 26.04.1. Temporary/runtime files were confined
to `build/tmp`; scratch logs are in `build/scratch`.

An earlier 70-digit curvature identity check lost relative accuracy through
cancellation at extreme tails. Raising precision to 120 digits resolved it
without weakening the 1e-55 relative tolerance. This is recorded in
`validation.json`. Nonfatal baseline build notices included CMake CMP0167
and shell startup references to a missing cargo environment; they did not
affect any test outcome. R skips are dev-tier exclusions.

Full R, no-long-double, JS and site runs were not required: there were no
changes to fitting, d/p/q/r, test infrastructure, bindings, public signatures
or family dossier text. The dev suites validate the unchanged source baseline;
the research checks validate the new evidence, not the current fitter's
global certification.

## Sources and continuity

The derivations here are self-contained. The parameterization was
cross-checked against the primary
[extraDistr Tukey lambda manual](https://search.r-project.org/CRAN/refmans/extraDistr/html/TukeyLambda.html);
no external optimizer or generalised-lambda result is used as a global
certificate. Earlier repository evidence remains relevant:
[family dossier](../../../univariateML-cpp/include/cppunivariate/distributions/tlambda/README.md),
[conditional existence inventory](2026-10-29-p0-locscale.md#tlambda),
[uniform-junction analysis](2026-10-31-open-existence-report.md), and the
[skew-family certificate research](2026-11-07-skew-theory-report.md).
The applicable [parameter rules](../../roadmap/2026-10-29-parameter-rules.md)
and [work budgets](../../design/work-budgets.md) remain canonical. No
inference procedure or regularity classification is changed by this report.

## Artifact pane

The required `show` command was attempted after completion. The viewer
returned HTTP 403 because this worktree is outside its configured readable
roots. No files or viewer configuration outside the authorized worktree
were changed. The report and evidence are available at the linked paths;
pane display requires registering this worktree with the viewer.
