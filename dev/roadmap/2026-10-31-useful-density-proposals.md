# Useful density proposals beyond actuar and extraDistr

**Status:** banked proposals, 2026-10-31; no implementation authorized by this entry.
Priority is a suggested study order, balancing application value, interpretability
and overlap. It is not a promised implementation order. TASK-131 records this
proposal capture; future implementation needs its own task.

## Shortlist

Domains below describe model interiors, not a complete boundary contract.
All positive-support models here have a fixed zero endpoint; no free translation
is proposed.

| Name | Parameters and interpretation | Support | Priority | Typical field | Reference |
|---|---|---|---|---|---|
| Exponentiated Weibull | Scale a > 0; Weibull shape b > 0; CDF exponent c > 0 | x > 0 | 1 — High | Reliability, engineering lifetimes, survival | [Mudholkar and Srivastava (1993)](https://doi.org/10.1109/24.229504) |
| Generalized inverse Gaussian (GIG) | Power index lambda real; chi > 0 suppresses density near zero; psi > 0 controls upper-tail decay | x > 0 | 2 — High | Positive measurements, stochastic volatility, latent heterogeneity | [Peña and Jauch, properties and computation](https://arxiv.org/abs/2401.00749) |
| Power generalized Weibull (PGW); compare adapted PGW | Scale a > 0; shapes b,c > 0; b and bc determine early- and late-life hazard powers in the convention below | x > 0 | 3 — High–medium | Reliability and survival with nonmonotone hazards | [Burke, Jones and Noufaily, modelling framework](https://arxiv.org/abs/1901.03212) |
| Sinh–arcsinh normal | Location mu real; scale sigma > 0; asymmetry epsilon real; tail parameter delta > 0 | x real | 4 — Medium | Distributional regression, asymmetric measurements, residual models | [Jones and Pewsey (2009)](https://doi.org/10.1093/biomet/asp053) |
| Prentice generalized gamma | Log-location mu real; log-scale sigma > 0; signed shape Q real | x > 0 | 5 — Medium; unification | Parametric survival and duration data | [flexsurv definition and mappings](https://chjackson.github.io/flexsurv/reference/GenGamma.html) |

The first four add families absent at capture time. Prentice generalized gamma
unifies `trgamma`, `invtrgamma` and `lnorm`; it is already mentioned in the
[classical failure-family roadmap](2026-10-29-classical-failure-families.md).
This entry owns its application motivation and preliminary numerical assessment.

## Preliminary ML and d/p/q/r assessment

These are analytical planning observations, not completed existence analyses,
benchmarks or validated algorithms. Difficulty refers to a production-quality
implementation over extreme parameters and all supported held subsets.

### Exponentiated Weibull — ML medium–high; d/p/q/r low–medium

Convention: F(x) = [1 - exp(-(x/a)^b)]^c. The density behaves as
x^(bc-1) near zero; b determines the stretched-exponential upper tail. For
integer c, this is the maximum of c independent Weibull variables.

For fixed a,b, c has the conditional MLE
c_hat = -n / sum(log(1-exp(-(x_i/a)^b))). This removes one dimension
from a fully free fit, but does not establish global optimality or existence.
Expect correlated shapes, flat profiles and extreme-shape limiting paths;
classify these before selecting a search interval. Constant positive data have
unbounded likelihood through the Weibull subfamily (c=1, b tending to infinity,
a at the common value). Held subsets require their own analysis.

CDF, quantile and inverse-transform sampling are explicit. Use log1mexp,
expm1 and log1p rather than subtracting nearly equal probabilities. Near zero,
underflow of (x/a)^b must not destroy its log contribution. Quantiles must accept
log probabilities and both tails without first rounding a probability to one.
Independent reference: [SciPy exponweib](https://docs.scipy.org/doc/scipy/reference/generated/scipy.stats.exponweib.html),
with SciPy shape a = our c, SciPy shape c = our b, loc fixed to zero.

### GIG — ML high; d/p/q/r high

Convention: f(x) proportional to x^(lambda-1) exp(-(psi*x+chi/x)/2).
The normalizer uses K_lambda(sqrt(chi*psi)). Gamma is the chi=0,
lambda>0 face; inverse gamma is the psi=0, lambda<0 face; lambda=-1/2
is inverse Gaussian in suitable coordinates. Those zero-coefficient faces
are outside the table's interior and need explicit contracts.

The log likelihood uses sufficient statistics sum(log x), sum(x), sum(1/x).
It is concave in natural exponential-family coordinates on the interior;
this helps optimization but does not prove interior attainment. Boundary
competition with gamma/inverse gamma and concentration on constant samples
still need classification. Stable log-Bessel K values and order derivatives
(or independently checked alternatives) are central risks. The current
special-function seam has no Bessel wrapper; backend range must be assessed.

Density evaluation requires a stable normalizer; general CDF and quantile
need more than an elementary transform. Investigate direct tail integration,
specialized algorithms and a bracketed quantile solve. Sampling needs a
validated independent-draw algorithm; an MCMC chain is not a replacement for
rml's independent sampling contract. Published half-integer CDF algorithms
do not establish arbitrary-order coverage. Reciprocal symmetry exchanges
chi/psi and negates lambda, providing a useful independent identity.

### PGW / adapted PGW — ML medium–high; d/p/q/r low–medium

Provisional PGW convention: H(x) = [1+(x/a)^b]^c - 1,
S(x)=exp(-H(x)). Hazard powers are b-1 near zero and bc-1 at infinity;
c=1 is Weibull. Density is H'(x)*S(x), and quantiles follow by inverting H.
The reference's adapted family uses different coordinates: settle its exact
parameterization and proper-distribution domain before implementation.

Expect shape/scale correlation, concentration paths and poorly identified
limiting regimes. Compare ordinary and adapted PGW on representative profiles;
do not add two names without establishing their distinct contracts. A freely
estimated PH multiplier in addition to a time scale introduces another
parameter and potential redundancy on the Weibull submodel; do not add it
implicitly. Constant samples have the Weibull concentration failure.

Evaluate H with expm1(c*log1p((x/a)^b)); invert with expm1/log1p and
log-domain power handling. This is elementary algebra but still needs stable
extreme-tail and endpoint branches. For adapted PGW, check any proposed
Gompertz/log-logistic/Burr limits separately and exclude defective regimes
unless mass at infinity is explicitly supported.

### Sinh–arcsinh normal — ML high; d/p/q/r medium

Fix the convention through Z ~ N(0,1):
X = mu + sigma*sinh((asinh(Z)+epsilon)/delta).
Then F(x)=Phi(sinh(delta*asinh((x-mu)/sigma)-epsilon)).
The normal occurs at epsilon=0, delta=1; delta<1 gives heavier tails
and delta>1 lighter tails than normal. Location and scale here are not
necessarily the mean and standard deviation.

Four coupled parameters require careful initialization and profile/multistart
comparisons. Study tail/scale degeneracies, limiting families, ties, constant
samples and every supported held subset; returning a stationary point alone
would not establish an unrestricted MLE. Constant samples can collapse via
the normal subfamily. Existence and useful search bounds remain open here.

CDF/quantile/sampling are normal transforms, and density adds an analytic
Jacobian. Hyperbolic overflow, cancellation under strong asymmetry and
log-tail normal transforms are the numerical risks. Agree the sign convention
before comparing another package; transformed-normal identities give checks.

### Prentice generalized gamma — ML high; d/p/q/r medium–high

For Q != 0, let G ~ Gamma(Q^(-2),1) and
X = exp(mu + sigma*log(Q^2*G)/Q). At Q=0 use lognormal(mu,sigma).
Positive Q is `trgamma`, negative Q is `invtrgamma`. A unified fit can cross
the lognormal surface instead of reporting a branch-specific escape.

The difficult region is small |Q|: gamma shape diverges and direct log-density
terms cancel. A small-Q expansion or a demonstrably accurate uniform method
is needed, including smooth derivatives if used in optimization. An arbitrary
cutoff that simply substitutes lognormal may distort the score. Existing
branch fitters provide starts and comparison evidence, not a proof of a
unified global fit. Large-|Q| limits, held sets and concentration failures
still require analysis. Constant data collapse through Q=0, sigma tending to zero.

Reuse gamma tails with correct tail reversal for Q<0, explicit lognormal at
zero, and stable near-zero evaluation of density/CDF/quantile/random transforms.
Sampling also suffers cancellation in log(Q^2*G)/Q near zero. Smooth coordinates
alone establish neither regularity nor valid Wald inference; consult the
[inference plan](2026-10-30-inference-and-dqm.md) before inference work.

## flexsurv comparison and scope

The [documented flexsurv catalogue](https://chjackson.github.io/flexsurv/reference/index.html)
was checked on 2026-10-31. Original generalized gamma is `trgamma`; original
and Prentice generalized F have the same interior family as GB2. Prentice
signed Q for generalized F does not create an extra family beyond GB2.
Its P=0 generalized-gamma face is separate in our current fitters.
Weibull PH is another parameterization, not another density family.

Additional flexsurv scope includes negative-shape Gompertz with mass at infinity
and Royston–Parmar spline distributions. Neither is accepted by this proposal:
the former needs a defective-distribution contract; the general spline family
needs user-chosen parameter-vector length. Exponentiated Weibull, PGW, GIG and
sinh–arcsinh are broader proposals, not flexsurv built-ins.

## Trigger to revisit

When choosing the next useful family beyond the current catalogue, start with
an exponentiated-Weibull existence/profile study. GIG is the next distinct
positive-family candidate, but budget separately for its special functions,
CDF/quantile and sampler. Priorities may change after those studies.

## Verification

Before implementing any family, follow the [validation guide](../validation/README.md):
record the model, limits, held subsets, likelihood existence and unresolved
proof gaps; then choose the fitter and document its failure behavior.
Validate density/CDF/quantiles with high-precision independent calculations,
both tails and log probabilities, boundary cases and nested-family identities.
Use seeded simulation/recovery and independent likelihood searches for fitting;
distinguish stationarity, numerical comparisons and existence proofs. Sampling
checks should include distributional checks, not only q/p round trips.

This proposal was assessed against source revision
`85e774cd15614233dedd3f67226091f0a77cd064` on 2026-10-31, using dossier and
special-function-seam inspection and the linked primary documentation.
No new numerical experiments, likelihood-existence proofs, tolerance claims or
implementation tests were performed. The difficulty ratings are provisional.
