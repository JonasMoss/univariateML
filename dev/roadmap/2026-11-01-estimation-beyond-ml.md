# Estimation beyond ML: closure, intrinsic Bayes actions and spacings

**Status:** banked research direction, 2026-11-01; preliminary analytical,
literature and numerical exploration under TASK-135. No public estimator API,
change to ML restrictions, or general Bayesian infrastructure is authorized
by this entry. The current ML-first scope remains as documented in CLAUDE.md.

## Motivation and relationship to existing work

Investigate estimators that return a density in a declared model class or its
relevant closure, especially where ML has an unbounded likelihood, an unattained
supremum, singular information or a limiting density at infinite parameters.
Keep point estimation, finite-sample existence, consistency, optimization and
valid uncertainty quantification separate.

Coordinate this with the [inference plan](2026-10-30-inference-and-dqm.md),
[failure-family plan](2026-10-29-classical-failure-families.md), and
[package/documentation plan](2026-10-30-package-and-documentation.md).
Do not duplicate or supersede their classification and validation requirements.

## Closure-compatible coordinates

For Lomax shape alpha and scale lambda, use xi=1/alpha and sigma=lambda/alpha:

    f(x) = sigma^(-1) (1 + xi*x/sigma)^(-1/xi-1), x >= 0.

The exponential limit is the finite face xi=0, sigma>0. In the original
coordinates both alpha and lambda diverge. This is the nonnegative-shape GPD
chart, not a new density family. Study analogous coordinates for other families,
allowing multiple charts if necessary. Identify limits in Hellinger distance;
weak limits containing atoms or escaped mass are separate objects and need a
declared dominating measure and model contract. A proper density limit is not
the same as parameter divergence with loss of probability mass.

Finite closure coordinates are useful but do not uniquely define intrinsic
coordinates or coordinate-invariant averaging. Posterior means are natural Bayes
actions for squared parameter error when the chosen coordinates and required
moments are appropriate. Changing coordinates changes this decision problem.

## Bayesian estimation in distribution space

Given a proper posterior, consider the model-restricted Bayes action

    Q_hat in argmin_{Q in A} E[h^2(P_theta,Q) | data],

where A is the original family or an explicitly specified Hellinger closure,
and h^2(p,q)=1-integral sqrt(p*q). This is a restricted posterior Frechet mean.
It is invariant under one-to-one parameter changes, with the posterior measure
transported consistently. It is not uniquely objective: prior, loss and action
class remain choices.

Let m(x)=E[sqrt(p_theta(x)) | data]. The unrestricted Hellinger action is
q_H=m^2/integral(m^2); the restricted action is its Hellinger projection onto A.
It differs from the posterior predictive density E[p_theta]. The derivation
uses Tonelli and Cauchy-Schwarz; 0<integral(m^2)<=1 for a proper posterior of
dominated probability densities. No implementation has been checked here.

Study forward KL, reverse KL, and Bernardo's intrinsic discrepancy
min{KL(P,Q),KL(Q,P)} as distinct losses. Forward expected KL projects the
posterior predictive distribution where the relevant integrals are finite;
reverse KL can impose common-support restrictions. Intrinsic discrepancy is
symmetric but need not be a metric. See [Bernardo and Juarez (2003), Intrinsic
Estimation](https://www.uv.es/~bernardo/BernardoJuarez.pdf).

Three independent gates must remain explicit:

1. Prior construction is mathematically defined for the particular free subset
   and regime. Full Jeffreys, independence Jeffreys and reference priors differ.
2. Posterior normalization is finite and positive. Bounded Hellinger loss does
   not repair an improper posterior. Truncation limits need their own argument
   and may depend on the exhaustion used.
3. Expected loss attains its minimum in the chosen action class. Boundedness
   and closure alone do not establish attainment or uniqueness.

Low-dimensional quadrature is a plausible computational route. First establish
propriety and loss existence, analytically marginalize where possible, and
validate boundary/tail integration. Do not infer convergence from a stable
finite integration box. Jeffreys remains undefined as a full regular-volume
construction in some nonregular regimes; a proper posterior under another
prior is a different question.

## Maximum product of spacings (MPS)

For distinct ordered continuous observations x_1<...<x_n, define

    D_1 = F_theta(x_1),
    D_i = F_theta(x_i)-F_theta(x_{i-1}), i=2,...,n,
    D_{n+1} = 1-F_theta(x_n).
    S(theta) = sum_{i=1}^{n+1} log(D_i).

Maximize S, or its mean. Both tail spacings are part of the definition.
The D_i sum to one, hence S <= -(n+1)*log(n+1) by AM-GM. Equivalently this
minimizes KL(U || D) from uniform cell weights U_i=1/(n+1) to the spacing
vector. This is a data-dependent partition criterion, not the posterior
expected distributional KL loss above.

A bounded criterion cannot diverge upward like a spiking likelihood, but its
supremum can still be unattained along a family limit. Check identifiability,
all boundary paths and held subsets separately. MPS is invariant under
parameter relabeling and fixed strictly increasing data transformations when
the transformed model is transported too. It does not require a Fisher
information matrix just to define its objective.

Endpoint contrast: if a fitted upper endpoint approaches x_n and F(x_n)->1,
the last spacing tends to zero and S->-infinity, even if f(x_n)->infinity.
The analogous statement holds at the lower endpoint. This excludes those
specific endpoint-spike paths, not every possible escape.

[Wong and Li (2006)](https://hub.hku.hk/bitstream/10722/82965/1/content.pdf)
study GEV/GPD spacings estimators beyond ordinary ML regularity regimes.
Their gamma equals our -xi. Their asymptotics distinguish an endpoint direction
with a nonstandard rate from other coordinates; do not read their efficiency
claims as joint regular Wald inference or as a proof of unique finite-sample
attainment over every parameter domain. General consistency results also have
conditions: [Shao and Hahn (1999)](https://www.ism.ac.jp/editsec/aism/51/3.html).

### Exact endpoint example and independent pilot

For Uniform(a,b), n>=2 distinct observations and range R=x_n-x_1, the MPS
solution is

    a_hat = x_1 - R/(n-1),  b_hat = x_n + R/(n-1).

Writing left and right margins as l,r gives product proportional to
l*r/(R+l+r)^(n+1). For fixed l+r it is maximal at l=r; differentiating in
their sum gives l=r=R/(n-1), with vanishing product at the outer boundaries.
Unlike the uniform MLE, the MPS endpoints lie outside the observations.

The [pilot script](../research/evaluations/2026-11-01-spacings-pilot.py) and
[recorded output](../research/evaluations/2026-11-01-spacings-pilot.json) check
this formula from three numerical starts. They also examine GPD xi=-1.5,
threshold zero, upper endpoint 1+epsilon approaching sample maximum 1:
likelihood increases while the spacing product decreases. This illustrates
the endpoint mechanism in a regime with unbounded unrestricted likelihood.
It is not a global GPD fit or an existence proof.

### Catalogue pilot and implementation risks

| Family/submodel | Reason to investigate | Unresolved work |
|---|---|---|
| GPD, threshold known | Endpoint singularities and non-DQM regimes | Shape domain, all escapes, endpoint inference |
| GEV, all free | Upper/lower endpoint competition; ML restrictions are method-specific | Full finite-sample existence analysis and closure comparisons |
| Fatigue, threshold free | ML can spike at the minimum | Test whether remaining escapes prevent MPS attainment; literature check |
| Lomax / Pareto II | Exponential closure and free-threshold submodels | Separate fixed and free threshold, compare exponential face |
| Triangular, endpoints free | CDF criterion avoids needing infinite endpoint Fisher information | Mode kinks, all boundary/collapse paths |
| Skew-normal, all free | Half-normal limiting densities | Finite optimum versus half-normal closure; singular symmetry |
| GB2, four free | Rich limiting-family structure | CDF expense, cancellation, weak identification and multiple escapes |
| Discrete Weibull / CMP | Contrast with continuous recipe | Ordinary continuous spacings do not transfer directly |

Ties make ordinary continuous MPS identically zero. Distinguish measurement
rounding, true discrete observations and numerical duplication. Grouped-data
likelihood with declared measurement bins is coherent but is not ordinary MPS.
Density replacements for zero spacings define a modified criterion and need
separate dimensional, invariance, existence and inference checks. Do not jitter
silently or apply a continuous theorem to discrete data.

Compute log spacings using log-CDF or log-survival differences and log1mexp,
choosing a stable tail. Never floor zero spacings to an arbitrary epsilon:
that changes boundary penalties and the estimator. Existing CDF truth tables
do not certify differences of extremely close CDF values. Sorting costs
O(n log n) once; subsequent objective evaluations use O(n) CDF evaluations,
potentially much more costly than density evaluation. Use density/ML starts as
candidates, not evidence of the MPS global optimum.

## Other recipes to compare

| Method | Appeal | Main qualification |
|---|---|---|
| Cramer-von Mises minimum distance | Simple CDF-only objective; usual continuous-model statistic is 1/(12n)+sum_i [F(x_i)-(2i-1)/(2n)]^2 | Ties computable but continuous-null calibration does not transfer; existence and identification remain |
| Anderson-Darling minimum distance | More tail weight; log-CDF/log-survival machinery reusable | Strong sensitivity to extremes and endpoint zeros; not automatically robust |
| Minimum CRPS | Proper predictive scoring criterion; handles empirical ties without zero-spacing failure | Data-scale dependent; tail integrability must be checked |
| Quantile matching / integrated quantile losses | Tail emphasis explicit; useful for quantile-defined Tukey lambda | Choice of weights/levels, identification, quantile cost and moment requirements |
| L-moments / trimmed L-moments | Useful competing fits and starts for extreme-value models | Ordinary L-moments require a finite absolute first moment; trimming is a different procedure |
| Minimum Hellinger to a smoothed empirical density | Distribution-space frequentist alternative | Empirical atomic measure versus a continuous model has maximal Hellinger distance; smoothing and bandwidth are substantive choices |

For CRPS, minimizing mean score equals minimizing integral(F_theta-F_n)^2 dx
up to a data-only constant when integrals exist. This differs from CvM's
model-CDF weighting, and CRPS is not invariant under general monotone changes
of measurement scale. See [Gneiting and Raftery (2007)](https://sites.stat.washington.edu/people/raftery/Research/PDF/Gneiting2007jasa.pdf).
For ordinary L-moments see [Hosking (1990)](https://doi.org/10.1111/j.2517-6161.1990.tb01775.x).
These entries are candidate methods, not blanket robustness or existence claims.

## Trigger to revisit

When expanding beyond ML, first run a dedicated GPD/GEV/threshold-family MPS
study with CvM and AD comparators, then study closure-aware Bayes actions as a
separate track. Decide action classes, ties/rounding contracts and estimator
metadata before designing public interfaces. Keep log likelihood as a density
evaluation separate from the fitting criterion; ML-specific uncertainty and
model-selection interpretations must not be inherited automatically.

## Verification

Initial check: base source revision 90ec2153ea2a4272bb47259bb6091d2602a176e6,
plus the new pilot script; exact dependency versions in its JSON output.
Deterministic sample [0.1,0.2,0.4,0.7,1.0], no random seed. Command:

    OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 nice -n 10 python3 dev/research/evaluations/2026-11-01-spacings-pilot.py

Three uniform fits passed endpoint error <1e-7 with optimizer success; GPD
path monotonicity and exact-tie rejection passed. No permitted failures or
skips. No production CDF/fitter tests, global nonuniform optimization,
large-sample simulation, coverage, runtime benchmark or posterior quadrature
were performed. Results are illustrations, not certification.

For future work follow the [validation guide](../validation/README.md):
record method-specific existence proofs/gaps, independent objective checks,
tail-spacing accuracy, held subsets, ties, affine and monotone transformations,
closure faces, multiple starts and failure accounting. Simulations should
separate nonregular correctness, contamination sensitivity and finite-sample
performance. Uncertainty needs method/regime-specific calibration; do not use
an MPS objective Hessian as likelihood Fisher information without justification.
