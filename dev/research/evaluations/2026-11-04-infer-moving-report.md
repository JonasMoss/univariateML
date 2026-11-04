# Moving features, local experiments and information estimates

TASK-102.11, 2026-11-04. Research at source revision
`a3fbb01383ab1ec6065c43f7fade40b1bd2751a4`, branch `codex/infer-moving`.
Only this report and its [evidence directory](2026-11-04-infer-moving-data/)
were added. No library, binding, dossier, golden, generated d/p/q/r, roadmap,
board or Git history was changed. All computation used the workstation slot
locks; no simbox, installation or sub-agent was used.

The moving second-derivative joins in **snorm and Huber do not prevent regular
inference**. Both models have DQM, finite positive definite Fisher information
and a valid ordinary, piecewise observed Hessian at every finite interior
parameter, including snorm symmetry with skewness free. GED location is DQM
for **ν > 1/2**, whereas its ordinary location Hessian has the right finite
expectation only for **ν > 1**. These are different thresholds.

For bounded Tukey lambda and negative-shape GPD/GEV, rates belong to
**endpoint coordinates and their complementary directions**. They do not
usually belong to each named coefficient separately. At the critical exponent
the endpoint experiment is Gaussian at rate √(n log n); below that exponent it
is a Poisson endpoint experiment. Directions preserving endpoints can remain
root-n. For example, all-free GEV at ξ = −3/4 has one fast endpoint coordinate
and two root-n coordinates, rather than three coordinates at rate n^(3/4).

The six families all receive a regime classification below. What remains open
is stated by family and procedure in [Open results](#open-results), especially
GED's fitted joint Hessian near its cusp and transferring ideal likelihood
limits to the actual numerical fitters. A numerical convergence flag does not
establish that transfer.

## Scope, coordinates and grades

The question is i.i.d. sampling from the stated model unless a paragraph
explicitly discusses misspecification. A rate r_n means the multiplier in
r_n(estimate − truth), not a coverage-error rate. Define

$$
 H^2(p,q)=\int(\sqrt p-\sqrt q)^2\,dx.
$$

There is no factor 1/2 in this definition. In a regular chart,
H²(p_θ,p_(θ+h)) = hᵀIh/4 + o(‖h‖²), where I is **per observation**.
Write s = ∂ log p/∂θ, J = Σ s_i s_iᵀ, H = −Σ ∂² log p_i/∂θ∂θᵀ.
Ordinary regular limits are J/n → I and H/n → I when their separate
conditions hold. A model covariance is (nI)⁻¹ or a justified H⁻¹; sandwich
covariance is H⁻¹ J H⁻ᵀ, without another factor 1/n. The outer product of the
total score is neither J nor an information estimator.

“Proved here” means the density calculation or argument supplied below in
real arithmetic. “Derived local experiment” means a likelihood-limit argument,
separate from an assertion about a numerical maximizer. Numerical evidence is
finite, with no interval-arithmetic certificate. Literature checks distinguish
read theorems, abstracts and inaccessible originals; the retained
[source ledger](2026-11-04-infer-moving-data/sources.json) records this explicitly.

The fitter scope was read from the six `README.md` and `mle.hpp` files:

| Family | Estimable coordinates and restrictions relevant here |
|---|---|
| tlambda | μ, σ > 0, λ, every held subset. Free λ ≤ 1; also λ ≥ −1 when μ is free. Holding μ removes the lower restriction. Held λ can exceed 1. |
| ged | mean μ, standard deviation σ > 0, ν. Free ν ≥ 1, including the Laplace face. Held ν can be any positive value. |
| gpd | μ is a **known threshold**; free σ and/or ξ. Free ξ ≥ −1. With free σ, held ξ < −1 is rejected. |
| gev | μ, σ, ξ, every held subset. Free −1 ≤ ξ ≤ 1. Held ξ > 1 can be accepted. Nontrivial held ξ < −1 fits are restricted as discussed below. |
| snorm | mean μ, standard deviation σ, positive skew ξ, every held subset. This is the Fernández–Steel two-piece normal. |
| huber | μ, raw scale σ, positive ε, every held subset; ε can be held or estimated. σ is not its standard deviation. |

All-held evaluation is not an estimation problem. A fitted coefficient on a
restriction is not an interior MLE simply because the underlying density is
regular there. The historical parameter-plan table contains “ξ ≥ −1 (now
−1/2)”; the inspected fitters actually use **−1**, which is the scope here.
The [canonical inference plan](../../roadmap/2026-10-30-inference-and-dqm.md)
remains the coordination reference.

## Two local calculations

### An endpoint

Suppose distance d inside a moving endpoint has density

$$
 f(d)=c d^{\alpha-1}(1+o(1)),\qquad c>0,\quad\alpha>0,
$$

with controlled differentiated remainders, no other worse moving feature, and
smooth integrable contributions away from the endpoint. Put κ = α − 1.
Moving the endpoint by h gives an edge contribution of order |h|^α.
On d ≫ |h| the squared root-density increment is

$$
 \frac{c\kappa^2}{4}h^2 d^{\alpha-3}(1+o(1)).
$$

Split the integral at M|h| and a fixed small δ. Substitute d = |h|y in
the edge integral. The result, proved by these two integrals, is

$$
 H^2\asymp
 \begin{cases}
 |h|^\alpha,&0<\alpha<2,\\
 h^2\log(1/|h|),&\alpha=2,\\
 h^2,&\alpha>2.
 \end{cases}
$$

At α = 2 the logarithmic coefficient is c/4 for a unit endpoint displacement.
At α = 1 the leading interior derivative above vanishes, but the **support
jump still gives order |h|**. Thus finite or even zero pointwise scores at a
uniform member do not restore DQM. The formula requires its stated local
conditions; it is not a theorem for arbitrary densities specified only by an
undifferentiated asymptotic equivalence.

For α > 2, the square-root endpoint derivative is square-integrable, the
lost support mass is o(h²), and the split argument gives DQM. For α ≤ 2,
lost mass is not o(h²) in an endpoint-moving direction, excluding ordinary
DQM. At α = 2 the score second moment also diverges logarithmically.

Let e denote the endpoint. For α < 2 the point measure of n^(1/α)(e−X_i)
converges to a Poisson process with intensity c y_+^(α−1)dy; a local endpoint
shift translates that intensity. This is an experiment allowing singular
support components, not an ordinary Gaussian shift or a likelihood ratio
that must have expectation one under the old support. Truncating the scaled
distance at M leaves a squared-root likelihood contribution bounded by a
constant times M^(α−2), which tends to zero. This supplies the connection
from the rare-event point-process limit to the local endpoint experiment.
For α = 1 it is a homogeneous boundary Poisson experiment. Distinct endpoints
give independent limiting edge processes.

At α = 2, use h = u/√(n log n). The probability of any observation in a
new or lost edge is O(1/log n). Truncated Taylor increments satisfy Lindeberg;
their variance is supplied by ∫ d⁻¹ dd. The limiting endpoint experiment is
Gaussian, with information c/2 in this normalization. For several endpoints,
with velocities v_j, the information is Σ c_j v_j v_jᵀ/2. Smooth tangential
directions use √n and their cross information with the critical coordinates
vanishes after normalization.

The regular endpoint score and Hessian have finite means at α > 2. Their
variances generally require α > 4, since their squared leading terms
integrate d^(α−5). Infinite variance of an information estimate is not
inconsistency of its average. At α = 2, exact endpoint scores satisfy
J/(n log n) → c/2; the leading observed Hessian has the same limit. Neither
is estimating finite ordinary Fisher information. These are possible critical
information estimates in the **mixed rate chart**, not approvals of a generic
finite-difference stencil.

This partition agrees with the endpoint α in
[Smith (1985), publisher abstract](https://academic.oup.com/biomet/article-abstract/72/1/67/242523).
The original full proof was not accessible. Its regular MLE transfer is not
used without qualification: the uniformity problem identified by Bücher and
Segers is material.

### A positive-density cusp

For an even cusp f(x) = f₀{1 − A|x|^ν + o(|x|^ν)}, f₀,A > 0, a location
increment has, near zero,

$$
 \sqrt{f(x-h)}-\sqrt{f(x)}
 =-\frac{\sqrt{f_0}A}{2}
   (|x-h|^\nu-|x|^\nu)+\text{remainder}.
$$

For 0 < ν < 1/2, substitute x = hy. The squared difference integrates to

$$
 H^2\sim K_\nu |h|^{1+2\nu},\qquad
 K_\nu=\frac{f_0 A^2}{4}
 \int_{\mathbb R}(|y-1|^\nu-|y|^\nu)^2dy>0.
$$

The tail of this integral is proportional to |y|^(2ν−2), so it is finite
precisely below 1/2. At ν = 1/2, integrate the two derivative tails to get

$$
 H^2\sim \frac{f_0 A^2}{8}h^2\log(1/|h|).
$$

Above 1/2 the derivative is in L² and the ordinary quadratic term replaces
this expression. Consequently the location multipliers are n^(1/(1+2ν)),
√(n log n), and √n respectively. The critical multiplier includes the log
factor; writing n^(1/γ) alone omits it.

For ν < 1/2 the log likelihood process at the first multiplier has limit

$$
 2\sqrt{K_\nu}\,W_{\nu+1/2}(u)
       -2K_\nu|u|^{1+2\nu},
$$

where W is two-sided fractional Brownian motion of unit variance at u = 1.
Here is an independent derivation for the **even** GED cusp: the increment
kernels |y−u|^ν−|y|^ν have squared norm proportional to |u|^(1+2ν), and
polarization gives stationary-increment fBm covariance. Individual log
increments are bounded by a constant times |h|^ν, hence tend uniformly to
zero. A triangular-array CLT gives the Gaussian term and normalization gives
the displayed drift. The product Hellinger increment bound is C|u−v|^(1+2ν),
whose exponent exceeds one, giving compact-interval tightness of square-root
likelihoods. This proves the local process limit; transferring its argmax to
a specific algorithm additionally needs localization, uniqueness of the limit
argmax and a vanishing likelihood optimization error.

At ν = 1/2 the same truncation/CLT argument gives a linear Gaussian
likelihood experiment with information 2K = f₀A²/4 at rate √(n log n).
Compact tightness follows by bounding increments by |u−v|^(2−η), η > 0.
This critical constant will matter for OPG below.

The i.i.d. section and Remark 1 of the
[Dachian et al. survey](https://arxiv.org/html/1711.03740) identify generalized
Gaussian cusps and their fBm limits. Its principal displayed example has an
odd cusp and its likelihood calculation is explicitly heuristic; the even
kernel and critical coefficient above were derived here. It points to
Prakasa Rao (1968), *Annals of Mathematical Statistics* 39, 76–87,
DOI [10.1214/aoms/1177698506](https://doi.org/10.1214/aoms/1177698506).
That original was not accessible, so no unchecked original theorem is claimed.

## Tukey lambda

### Endpoint exponent, regularity and rank

The quantile is Q(u) = μ + σ{u^λ − (1−u)^λ}/λ, with its continuous
logistic extension at zero. For 0 < λ < 1, near the lower endpoint,

$$
 d=\frac{\sigma}{\lambda}
       \{u^\lambda+\lambda u+O(u^2)\}
   \sim\frac{\sigma}{\lambda}u^\lambda,
 \qquad
 f\sim\lambda^{1/\lambda-1}\sigma^{-1/\lambda}
                 d^{1/\lambda-1}.
$$

Thus **α = 1/λ only for 0 < λ < 1**. At λ = 1, Q′ = 2σ and the
density is 1/(2σ). For every fixed λ > 1, Q′ tends to σ at either
endpoint and density tends to **1/σ**, not a negative-power singularity.
The background extrapolation above 1 is false. Held λ = 2 is another
uniform law, on [μ−σ/2, μ+σ/2].

For λ ≤ 0 the support is all real. Quantile-coordinate scores have at most
logarithmic powers in u and 1−u (quadratic logarithms at λ = 0). These have
all finite moments under uniform u. A compact-neighborhood envelope proves
DQM. For 0 < λ < 1/2, endpoint scores are O(u^(−λ)) plus logarithms;
their squares are integrable and support loss is o(‖h‖²), giving joint DQM.

Rank can also be proved without eigenvalue spot checks. A zero linear
combination of DQM scores implies a zero quantile variation
δμ + δσ q_λ(u) + σδλ ∂_λq_λ(u) for every u. Symmetry sets δμ = 0.
For λ < 0, (∂_λq)/q has a divergent log u term; at zero it again diverges.
For 0 < λ < 1, cancelling endpoint displacement forces
δσ = σδλ/λ, but leaves a nonzero u^λ log u term unless δλ = 0.
All coefficients therefore vanish. Information has full rank in every
regular free subset. This is a local identifiability result; the λ = 1/2
threshold and the globally duplicated uniform members are different issues.

For numerical reproduction, put A = u^(λ−1)+(1−u)^(λ−1), B = ∂_u A,
q = (u^λ−(1−u)^λ)/λ. At μ = 0, σ = 1, in coordinates (μ, log σ, λ),

$$
 s_\mu=B/A^2,\quad
 s_{\log\sigma}=-1+Bq/A^2,\quad
 s_\lambda=-A_\lambda/A+Bq_\lambda/A^2.
$$

The retained checker integrates these independently over u and checks their
means and rank at λ = −1, −0.3, 0, 0.25, 0.4.

### Free subsets and local rates

Use **a = σ/λ**, so endpoints are e₋ = μ−a, e₊ = μ+a. At positive
λ the endpoint differential, in the actual free coordinates, is

$$
 \delta e_\pm=\delta\mu\pm
       (\delta\sigma/\lambda-\sigma\delta\lambda/\lambda^2).
$$

Let d be the number of free coefficients, r the rank of these two endpoint
rows restricted to them. The endpoint-preserving kernel is nonzero exactly
when **σ and λ are both free**; it has δμ = 0,
δσ = σδλ/λ. Its shape likelihood has logarithmic endpoint scores and
positive finite information, so its local rate is √n. Use exact coordinates
(μ,a,λ) to follow this direction; a merely linear constraint can introduce
second-order endpoint drift.

| True shape | Endpoint directions (rank r) | Complement (dimension d−r) |
|---|---|---|
| λ ≤ 0 or 0 < λ < 1/2 | Joint ordinary DQM, full rank, √n throughout | Included in the same ordinary model |
| λ = 1/2 | Gaussian at √(n log n), no ordinary DQM | Regular Gaussian at √n |
| 1/2 < λ < 1 | Two power-intensity edge processes at n^λ, restricted to the endpoint image | Regular Gaussian at √n |
| λ = 1 | Uniform boundary Poisson processes at n | If shape is free and scale is free: one-sided regular shape direction at √n |
| Fixed λ > 1 | Positive-density endpoint processes at n | No shape tangent: shape is held in every supported fit here |

For d = 3, r = 2. For free {σ,λ}, r = 1. Free {μ,σ} or {μ,λ}
has r = 2. Every singleton has r = 1. This enumerates the supported positive
shape free subsets. In particular, in the all-free power-endpoint regime,
μ and a are fast while λ is root-n; **σ itself is then root-n**, inherited
from λ. A blanket claim that all estimates have rate n^λ is incorrect.

At λ = 1 with endpoints fixed, the score for shape is explicitly

$$
 -\tfrac12\{\log u+\log(1-u)+2\}.
$$

It has mean zero and positive finite variance under uniform u. Shape may
approach 1 only from below in the fitted branch, so this Gaussian tangent is
constrained. The joint local experiment combines it with the two endpoint
processes. If scale is held, shape moves endpoints and has the fast rate;
there is no extra shape tangent in that free subset.

At λ = −1 with free μ and free λ, the underlying model is regular but the
fitted shape constraint gives a Gaussian experiment restricted to a half-space.
An ideal constrained MLE has the information-metric projection limit. Holding
λ or holding μ removes this particular active constraint. Ordinary joint
Wald/χ² calibration must not be assigned to the constrained fit automatically.

In λ < 1/2, model Fisher, exact OPG/n and exact H/n have the usual
information interpretation; endpoint estimates have adequate support clearance
for a root-n plug-in. Information-estimator variances generally fail for
1/4 ≤ λ < 1/2. At λ = 1/2, exact endpoint J and H need the critical
normalization above; the tangent block retains n normalization. For λ > 1/2
there is no joint ordinary information covariance. At uniform members formal
location scores can even be zero: support information is not in those scores.

## GED

### Model experiment and supported subsets

Use the auxiliary power scale a = σ√{Γ(1/ν)/Γ(3/ν)}. This is a smooth
invertible transformation of the package standard deviation for ν > 0:

$$
 f(x)=\frac{\nu}{2a\Gamma(1/\nu)}
          \exp\{-|(x-\mu)/a|^\nu\}.
$$

Holding **package σ** while estimating ν does not hold a; derivative
calculations must include this transformation. At fixed ν the location score
and its information are

$$
 s_\mu=\nu a^{-\nu}\operatorname{sgn}(r)|r|^{\nu-1},\qquad
 I_{\mu\mu}=\frac{\nu^2}{a^2}
       \frac{\Gamma(2-1/\nu)}{\Gamma(1/\nu)},\quad \nu>1/2.
$$

If μ is held, scale and shape scores involve constants, T = |r/a|^ν and
T log T. All their squared moments are finite for every ν > 0. If μ is
free, its odd score is square-integrable iff ν > 1/2; the other scores are
even. In the even block, 1,T,T log T are linearly independent. These facts,
the square-root cusp calculation and smooth scale/shape transformations prove
DQM and full rank for every free subset in the claimed regimes.

| Free/held scope | Local experiment and rate |
|---|---|
| μ held, any positive held ν | Scale is regular at √n |
| μ held, ν > 1 free, scale held or fitted | Regular scale/shape experiment at √n |
| μ free, ν > 1/2 held | Regular location and optional scale experiment at √n |
| μ free, ν > 1 free | Joint regular model at √n, including 1 < ν ≤ 2 |
| μ free, ν = 1 free subject to ν ≥ 1 | Regular underlying model with a constrained shape tangent; projection limit for shape and correlated even nuisance |
| μ free, ν = 1/2 held | Gaussian location experiment at √(n log n); optional scale Gaussian at √n |
| μ free, 0 < ν < 1/2 held | fBm likelihood for location at n^(1/(1+2ν)); optional scale Gaussian at √n |

With ν ≤ 1 held, shape is not an estimated nuisance in these fitted
submodels. The even scale score is asymptotically orthogonal to the singular
location experiment: splitting off a shrinking cusp neighborhood makes its
variance contribution vanish; covariance with the location likelihood
increments vanishes by symmetry and the integrable residual bound. Thus the
mixed limits separate. The free-shape restriction at ν = 1 affects the even
nuisance block but not the first-order odd location contrast.

For ν > 1, consistent interior exact likelihood solutions have the regular
score expansion: the local location-score L¹ modulus is O(δ), and its L²
increment is O(δ^(2ν−1)) below 3/2, O(δ² log(1/δ)) at 3/2, and O(δ²)
above; the scale/shape terms are smooth with integrable envelopes. These give
the usual nonsmooth estimating-equation conditions on compact interior sets.
For held ν = 1, the median expansion supplies the location result. Below 1,
DQM and the local likelihood calculation do not by themselves certify the
implemented observation-knot search or its asymptotic optimization accuracy.

The accessible [Zhu–Zinde-Walsh working paper](https://www.mcgill.ca/files/economics/propertiesandestimation.pdf),
Proposition 5 and Appendix C, states its unknown-location asymptotic normality
result for both power exponents > 1, and treats known location separately.
It supports a nonsmooth-score route rather than a C²-only rule. Its 2009
journal version is bibliographically verified, DOI
[10.1016/j.jeconom.2008.09.038](https://www.sciencedirect.com/science/article/pii/S0304407608001668);
that final proof was not accessible. The working paper's nearby information
equality wording is not used to justify an ordinary Hessian below ν = 1.

### Which information estimates are valid

Away from r = 0, the ordinary negative location Hessian is

$$
 h_{\mu\mu}=\nu(\nu-1)a^{-\nu}|r|^{\nu-2}.
$$

For ν > 1 its expectation exists and equals I_μμ by Γ(z+1) = zΓ(z).
Its variance exists iff ν > 3/2, apart from trivial cancellations. Thus the
background assertion about **infinite Hessian variance on (1,3/2] is correct
at the true parameter**. The a.e. Hessian still has a valid LLN there.
For ν = 1 it is zero a.e.; a distributional derivative has a point mass
2δ₀/a, whose expectation supplies 2f(μ)/a = 1/a². For 1/2 < ν < 1
the ordinary Hessian is negative and its expectation is −∞. It cannot be
information or sandwich sensitivity, even though the model is DQM and its
location MLE can have a regular limit.

Exact OPG at the **truth** has a finite mean iff ν > 1/2. Its variance
generally requires ν > 3/4. The model Fisher formula can be evaluated at
consistent estimates of a and ν in any fixed ν > 1/2 regime. However:

* For held ν < 1 and μ fitted, the location MLE is at an observation,
  where s_μ is singular. **OPG evaluated at that MLE is undefined.** Deleting
  the offending observation is not a validated repair. Fisher plug-in and
  true-parameter OPG have different requirements.
* For held ν = 1, choose a finite median subgradient; the O(1) exceptional
  rows do not affect J/n. Ordinary H remains unsuitable.
* For held 1 < ν < 2, a true-parameter LLN alone is insufficient to prove
  H at a fitted μ. The next paragraph supplies the additional argument for
  the held-shape submodel. At a jointly fitted ν ≤ 2 the analogous theorem
  is left explicitly open, rather than inferred from a spot check.

For the held-shape exact location MLE, write p = ν−1 ∈ (0,1). Conditional
on the other observations its score equation gives X_i = m(r)+r, with

$$
 \frac{dX_i}{dr}=1+
  \frac{p|r|^{p-1}}{p\sum_{j\ne i}|X_j-m(r)|^{p-1}}.
$$

On the consistent localization event, a positive fraction of other residuals
lie in a fixed annulus, so the denominator is at least cn. Local boundedness
of the GED density gives a residual-density bound
C{1+n⁻¹|r|^(ν−2)}. Choose a cutoff n^(−k), k > 1 sufficiently close
to 1. A union bound for residuals below it is
O(n^(1−k)+n^(−k(ν−1))) = o(1). Above that cutoff and below δ, the
expected empirical Hessian is bounded by Cδ^(ν−1) plus
O(n⁻¹∫_(n^(−k))^δ r^(2ν−4)dr), which tends to zero, also when ν ≤ 3/2.
Away from the cusp the usual uniform LLN applies. This proves H/n consistency
at the held-shape exact location MLE. Fitting scale does not change that
location minimizer; consistency of its power-mean scale completes the same
result with scale fitted. It does **not** prove a uniform LLN over all nearby
μ: that supremum is infinite at sample knots.

At ν = 1/2 a further distinction is necessary. With a = 1, f₀ = 1/4:

$$
 K=1/32,\qquad I_{\mathrm{critical}}=2K=1/16.
$$

But s_μ² = 1/(4|r|) has tail Pr(s_μ² > t) ∼ 1/(8t), giving
**J/(n log n) → 1/8**, twice the critical likelihood information. A truncated
tail-index-one LLN proves this by cutting at n log n; the omitted maximum
probability and normalized truncated variance are O(1/log n).
Likelihood increments effectively truncate at distance
h = 1/√(n log n), whereas the closest observation has distance of order 1/n.
The inner scores cannot be linearized over a displacement h. The correctly
truncated score has

$$
 E[s_\mu^2\,1\{|r|>h\}]
     =\tfrac14 E_1(\sqrt h),
 \qquad E_1(t)=\int_t^\infty e^{-v}\,dv/v,
$$

which divided by log n tends to 1/16. Ordinary H is invalid here as well.
Consequently even an extra log normalization of raw OPG is not a correct
critical GED information procedure. This calculation is proved from the
density and independently checked numerically below.

## GPD and GEV

### Endpoint charts and regime partition

For ξ < 0 put α = −1/ξ and e = μ−σ/ξ. GPD, with μ held, has

$$
 f(x)=\frac{\alpha}{e-\mu}
       \left(\frac{e-x}{e-\mu}\right)^{\alpha-1},
       \quad\mu<x<e.
$$

GEV in coordinates (e,b,α), b = −σ/ξ, has

$$
 f(x)=\frac{\alpha}{b}
       \left(\frac{e-x}{b}\right)^{\alpha-1}
       \exp\left\{-\left(\frac{e-x}{b}\right)^\alpha\right\},
       \quad x<e.
$$

Both have endpoint exponent α, hence the same **endpoint** partition.
The GPD's lower endpoint is known, not a freely moving threshold.

| Shape and supported free subset | Local experiment |
|---|---|
| ξ > −1/2, any supported free coordinates, away from an active fitted bound | DQM, finite full-rank information; Gaussian at √n |
| ξ = −1/2 | One endpoint direction Gaussian at √(n log n); remaining directions Gaussian at √n |
| −1 < ξ < −1/2 | Endpoint Poisson experiment at n^(−ξ); remaining directions Gaussian at √n |
| ξ = −1 | Positive-density endpoint experiment at n; remaining directions Gaussian at √n, constrained if shape is free |
| ξ < −1 held | Underlying endpoint experiment at n^(−ξ), with a singular density at the endpoint; no general supported asymptotic fitting problem in this implementation |

The endpoint differential is

$$
 \delta e=\delta\mu-\delta\sigma/\xi+
                         \sigma\delta\xi/\xi^2.
$$

Every nonempty supported coordinate subset at ξ < 0 has one endpoint-moving
direction. Its endpoint-preserving kernel has dimension d−1. For GPD with
both σ,ξ free, the kernel is δσ = (σ/ξ)δξ. With e fixed the α score
is 1/α + log{(e−x)/(e−μ)} and information is 1/α². In the nonregular
regime this gives a root-n **shape** direction; σ shares that variation.
If either σ or ξ is held, the remaining coordinate moves e and there is no
regular complement. The kernel information must follow the actual held
constraints, not a block that treats an estimated endpoint as a known nuisance.

For GEV with shape held and μ,σ free, e is fast but b is root-n; both
named μ and σ therefore have root-n components. With all three free there
are two regular directions at fixed e. Their quantile derivatives are
proportional to v^(1/α) and v^(1/α)log v and are independent. Any regular
coordinate restriction inherits positive tangent information.

At ξ = −1 with shape **free**, its lower fitting bound constrains α ≥ 1.
The limit combines an endpoint process with a Gaussian cone experiment for
the complement. Holding ξ = −1 removes the shape inequality but not the
moving-endpoint experiment. At GEV ξ = 1 with ξ free, the model is ordinary
DQM but its upper fit bound gives an ordinary Gaussian cone experiment;
holding ξ = 1 removes this restriction.

For GPD held ξ < −1, free σ is rejected. For GEV the held path permits
ξ < −1 only with μ held and either σ held or every observation ≤ μ.
Under a true negative-shape GEV, Pr(max X_i ≤ μ) = exp(−n). Thus the only
nontrivial accepted free-scale exception has probability tending to zero and
does not define a usable asymptotic fitting regime. The density's mathematical
experiment and the implementation's supported estimator must be distinguished.

### Regular information and critical information

In GPD coordinates (σ,ξ), with μ known, direct integration gives

$$
 I=\begin{pmatrix}
 [\sigma^2(1+2\xi)]^{-1}
 & [\sigma(1+\xi)(1+2\xi)]^{-1}\\
 [\sigma(1+\xi)(1+2\xi)]^{-1}
 &2[(1+\xi)(1+2\xi)]^{-1}
 \end{pmatrix},\quad\xi>-1/2.
$$

Its determinant is 1/{σ²(1+ξ)²(1+2ξ)}, proving full rank. At zero this
extends to [[1/σ²,1/σ],[1/σ,2]]. Shape **free at zero** is therefore
regular too; holding it is unnecessary for model regularity. Endpoint loss
on the negative side of a small shape neighborhood is exponentially small.
This matrix agrees with the inverse asymptotic covariance in
[Grimshaw (1993), p.186](https://www.stat.cmu.edu/technometrics/90-00/vol-35-02/v3502185.pdf),
after mapping its shape k = −ξ and scale α = σ. The paper is an original
source; its scanned equations need visual care rather than blind OCR reuse.

For GEV, put v = −log p in the quantile Q = μ+σ(v^(−ξ)−1)/ξ.
For ξ ≠ 0, zero quantile variation would be a zero combination of
1,v^(−ξ),v^(−ξ)log v. These functions are independent. At ξ = 0 use
1,log v,(log v)². Together with DQM this proves full information rank.
For positive ξ its finite lower endpoint has exponential vanishing density,
so it introduces no extra power-endpoint irregularity.

[Bücher and Segers, Propositions 3.2–3.3](https://arxiv.org/html/1601.05702)
prove joint GEV DQM iff ξ > −1/2 and the regular MLE result on suitable
compact interior parameter sets. Their “role of uniformity” discussion
identifies why pointwise expected-Hessian continuity does not justify a
uniform empirical Taylor argument. This is a checked theorem with its
scope retained, not a theorem about every local maximum returned by this
package. Publication identity, *Extremes* 20(4), 839–872, DOI
10.1007/s10687-017-0292-6, is also verified on an
[author's institutional publication list](https://math.ruhr-uni-bochum.de/en/faculty/professorships/stochastics/group-buecher/research-publications/publications/).

In the regular ξ > −1/2 regime, Fisher, exact OPG/n and exact H/n are
valid model information estimates. For negative ξ their variances generally
need ξ > −1/4; at ξ = −1/4 the divergence is logarithmic. For a consistent
root-n estimator and ξ > −1/2, the minimum endpoint distance is of order
n^(−1/α), larger than its root-n endpoint displacement. With probability
tending to one, plug-in endpoint distances are uniformly relatively close;
shape errors times the largest log distance are o_p(1). This, the true-point
LLN and an endpoint truncation justify fitted H and J in a fixed regular
regime. This argument does not extend uniformly to α ↓ 2.

At ξ = −1/2, exact endpoint H and J, scaled by n log n, have the critical
information from the endpoint calculation; tangent blocks scale by n.
For −1 < ξ < −1/2, the endpoint score second moment is infinite and
the properly scaled Hessian/OPG remains driven by random extreme points,
not a deterministic joint Gaussian information matrix. At ξ = −1 the
leading score singularity cancels: GPD's uniform scale score is −1/σ
with nonzero expectation and negative ordinary observed curvature. GEV's
endpoint score at fixed b is −1/b with zero curvature. These finite formal
scores are not scores of a DQM endpoint model.

Useful exact submodels survive. GPD with ξ = −1 known has endpoint M
equal to the largest excess plus μ, and Pr((M−μ)/(e−μ) ≤ t) = t^n.
Thus [M, μ+(M−μ)α₀^(−1/n)] covers e with probability 1−α₀. For GEV
with ξ = −1 and b known, n(e−max X)/b is exactly Exp(1). Tukey's held
uniform members similarly reduce to uniform order-statistic inference.
These claims specify which nuisance quantities are known; they are not joint
Wald procedures for unknown shape or scale.

## Two-piece normal (snorm)

Use mode c, geometric side scale s, log skew b = log ξ. The two side
scales are s/ξ and sξ. For z = (x−c)/s, a = log s, and t = sign z,

$$
 \ell=\tfrac12\log(2/\pi)-a-\log(e^b+e^{-b})
                  -\tfrac12 z^2e^{-2tb},
$$
$$
 s_c=z e^{-2tb}/s,\quad
 s_a=-1+z^2e^{-2tb},\quad
 s_b=-\tanh b+t z^2e^{-2tb}.
$$

Across z = 0, log density and its first derivatives in **all three
parameters** match. Its second derivatives have finite jumps. A compact
parameter neighborhood is dominated by Gaussian tails times a polynomial;
piecewise square-root differentiation therefore gives joint DQM. When
integrating a second derivative, the interface term is the jump of a first
derivative, which is zero. Hence **Bartlett's identity holds without a missing
point-mass correction**. The ordinary a.e. piecewise H has a uniform LLN
on compact neighborhoods and is consistent at consistent fits. If a datum
is exactly at a fitted join, a bounded one-sided Hessian convention changes
the average by o_p(1); a blanket C² requirement is excessive.

Rank is proved by polynomial independence on both half-lines. In a zero
linear combination of the displayed scores, the linear coefficient forces
the c coefficient to vanish; the two quadratic coefficients then force both
scale and skew coefficients to vanish. There is no symmetry singularity.

The package's moment chart is smoothly invertible:

$$
 m=\sqrt{2/\pi},\quad
 C(\xi)^2=(1-2/\pi)(\xi^2+\xi^{-2})+4/\pi-1,
 \quad s=\sigma/C(\xi),\quad
 c=\mu-s m(\xi-\xi^{-1}).
$$

At ξ = 1, μ = 0, σ = 1, in (μ,log σ,log ξ) coordinates the information
is exactly diag(1,2,3−8/π). In particular 3−8/π > 0. The skew score is
sign(z)z²−2mz, orthogonal to both normal location and scale scores.
This is different from the singular symmetry point of Azzalini skew-normal.

Every finite interior supported free subset is regular, full rank, at √n;
Fisher, OPG/n and piecewise H/n are valid under the model. Standard regular
likelihood/estimating-equation arguments apply to consistent exact interior
maxima because scores are locally Lipschitz with a square-integrable envelope
and the population sensitivity is nonsingular. This does not certify every
conditional search. Testing ξ = 1 with μ and σ fitted is an ordinary
one-dimensional symmetry hypothesis in this model, with a χ²₁ likelihood-ratio
limit under these conditions; a fixed-location singular skew-normal result
must not be imported.

The original [John (1982) publisher record](https://www.tandfonline.com/doi/abs/10.1080/03610928208828279)
verifies the family and asymptotic-estimation literature. Only its abstract
was accessible. DQM, rank and Hessian conclusions here rest on the supplied
derivation and numerical identity checks, not an unverified reading of that
paper's proof.

## Huber

Let z = (x−μ)/σ, c = ε, ψ_c(z) = clip(z,−c,c),

$$
 Z(c)=\sqrt{2\pi}\operatorname{erf}(c/\sqrt2)
                    +2e^{-c^2/2}/c,\qquad
 B(c)=-Z'(c)/Z(c)=2e^{-c^2/2}/\{c^2Z(c)\}>0.
$$

In coordinates (μ,log σ,log c), scores are

$$
 s_\mu=\psi_c(z)/\sigma,\quad
 s_{\log\sigma}=z\psi_c(z)-1,\quad
 s_{\log c}=c\{B(c)-(|z|-c)_+\}.
$$

At z = ±c all three first derivatives match. On compact subsets of
0 < c,σ < ∞ the density has a common exponential tail envelope and the
derivatives have polynomial envelopes. This proves DQM, integrability and
the Bartlett identity by piecewise integration with zero first-derivative
jumps. The piecewise Hessian's indicator classes have a uniform LLN, so
H/n and OPG/n are consistent at consistent fitted parameters. No point mass
is missing from this Hessian. This is joint likelihood scale estimation,
not Huber Proposal 2 scale estimation.

Rank has a short proof. On the open core interval, a zero score combination
is a polynomial consisting of a linear location term, a quadratic scale term,
and a constant shape term cB(c). The polynomial is identically zero only
if all three coefficients vanish. Thus every finite c > 0 has positive
definite joint information; every held subset inherits positive information.
The conditional known-c location entry is especially simple:

$$
 I_{\mu\mu}=\Pr(|Z|<c)/\sigma^2
            =E[\psi_c(Z)^2]/\sigma^2.
$$

Consequently all finite interior Huber submodels have a √n Gaussian local
experiment. Finite shape tests with location and scale fitted are regular
under consistent exact likelihood maximization. Exponential envelopes,
continuous scores and the uniform Hessian law justify that ideal local MLE
expansion. The actual free-shape scan and its face comparisons still need
an asymptotic numerical-accuracy argument, independently of this mathematics.

## Uniformity and hypotheses

The classifications above are pointwise. The following transitions prevent
a blanket uniform inference promise.

* **Endpoint α ↓ 2** (Tukey λ ↑ 1/2; GPD/GEV ξ ↓ −1/2): ordinary
  information diverges like 1/(α−2). The endpoint-distance integral sampled
  by n observations changes when (α−2)log n is not large. Ordinary and
  critical limits cannot be interchanged. Fourth-moment thresholds at α = 4
  also change uncertainty of H and J, without changing DQM.
* **Tukey λ ↑ 1 from below:** u^(λ−1) dominates the other quantile
  derivative only when (1−λ)log(1/u) is large. With u ≍ 1/n the
  power-endpoint approximation is nonuniform when (1−λ)log n is bounded.
  The limiting uniform density is 1/(2σ), whereas the fixed-λ power
  approximation extrapolated toward 1 suggests coefficient 1/σ. The thin
  transition layer and the shape constraint must be retained. This does not
  invalidate the fixed-endpoint root-n shape tangent at λ = 1.
* **GED ν ↓ 1/2:** information diverges like 1/(2ν−1). Crossover depends
  on (2ν−1)log n. Raw critical OPG even has the wrong constant for the
  critical likelihood experiment. **GED ν ↓ 1 from above:** expected
  ordinary Hessian stays finite but concentrates arbitrarily close to the
  cusp. For a = 1, the fraction of E H supplied outside distance 1/n is
  Q(1−1/ν,n^(−ν)), the regularized upper gamma function. At ν = 1.001,
  even n = 10⁶ retains only **1.315%** of that expected curvature outside
  1/n. The controlling quantity is (ν−1)log n. This proves a concrete
  lack of uniformity of true-point empirical Hessian consistency near 1;
  at ν = 1 the ordinary Hessian is identically wrong.
  More explicitly, take ν_n = 1+1/(log n)² and cut at distance n⁻².
  With probability tending to one there is no observation inside that
  cutoff, while the expected truncated H/n tends to zero by the same gamma
  formula. Markov's inequality then gives H/n → 0 along this sequence,
  although the model location information tends to 1/a².
* **Huber c → ∞:** shape information tends to zero, since all typical
  observations enter the normal core. Joint shape estimation needs enough
  tail observations; n Pr(|Z|>c) large is necessary for normal shape
  asymptotics, not an asserted sufficient criterion. With c held, ordinary
  normal location/scale inference survives the limit.
* **Huber c → 0 with b = σ/c fixed:** use (μ,log b,log c). The core
  width is bc² and its probability is of order c². At fixed b, the log-c
  score is c²(y²−1)+O(c⁴) inside |x−μ| = bc²|y| ≤ bc², and
  2c⁴/3+O(c⁶) outside. Thus efficient log-c information is
  (8/15)c⁶(1+o(1)); removing the regular log-b nuisance changes only
  O(c⁸). Joint relative shape estimation cannot be uniform unless n c⁶
  grows. In the checker's (log σ,log c) chart the small eigenvalue is
  asymptotic to (4/15)c⁶, agreeing with 2.61×10⁻⁷ at c = 0.1.
  The limit model for μ,b is Laplace, whose ordinary location Hessian fails.
* **snorm ξ → 0 or ∞ at fixed moments:** a half-normal endpoint appears,
  and information-conditioning/regular asymptotics are not uniform. The
  probability of the shrinking side is ξ²/(1+ξ²), or its complement;
  retaining many observations on that side is necessary for a joint regular
  approximation. In contrast, **ξ → 1 has no singular regime transition**.
* **Fitting bounds:** local sequences within O(n⁻¹/²) of a regular active
  bound need Gaussian-cone rather than unconstrained-normal calibration.
  At the nonregular faces, combine the cone in the regular complement with
  its endpoint experiment. Shape-bound tests are not automatically χ²₁;
  a single regular one-sided efficient shape coordinate gives the familiar
  half χ²₀ plus half χ²₁ law. Other targets need their own projection.

For a regular contrast with nuisance fitted, use the full information and
its efficient-score/Schur-complement calculation. A principal information
block treats the omitted nuisance as **known**. For a fast endpoint with
root-n complement, the tangent information describes endpoint-known first
order inference for the regular target; it does not give a Gaussian endpoint
confidence interval. At a critical endpoint the mixed Gaussian experiment
can support suitable Gaussian tests, but it requires the mixed chart and
critical normalization. None of these statements validates the existing
generic parametric percentile bootstrap near transitions.

Under misspecification, DQM of the fitted family is insufficient. A sandwich
also needs a well-defined interior pseudo-true parameter, a valid estimator
expansion, finite score variance and nonsingular sensitivity under the actual
law. For fixed finite snorm parameters, E_g|X|⁴ suffices for score second
moments; for full Huber, E_g|X|² suffices, with local non-atomic join behavior
and a nonsingular pseudo-true sensitivity still required. Fixed-scale Huber
location alone has bounded score and weaker tail requirements. For held
GED ν > 1, the candidate sensitivity is
ν(ν−1)a^(−ν) E_g|X−μ|^(ν−2), and meat is
ν²a^(−2ν) E_g|X−μ|^(2ν−2), when these and the expansion exist. At Laplace
location sensitivity is 2g(μ)/a, not its zero ordinary Hessian. Endpoint
behavior under a different sampling density can change the experiment
itself. No robust endpoint inference is established by this report.

## Numerical evidence

All calculations are independent density calculations, not replacements for
production fitter tests. The [main checker](2026-11-04-infer-moving-data/check.py)
and [retained results](2026-11-04-infer-moving-data/results.json) contain:

* 96 GPD/GEV/GED Hellinger distances: six h values from 10⁻² to 3×10⁻⁵,
  endpoint α = 1,4/3,2,3,5 and GED ν = 0.3,0.5,0.6,1,1.2,2.
  Eighteen additional Tukey distances use λ = 0.25,0.5,0.75,1,1.5,2
  and h = 0.01,0.003,0.001. Log slopes agree with the predicted powers
  and critical log corrections within the declared absolute 0.13 tolerance.
  This is finite trend evidence, not a proof from estimated slopes.
* Analytic score/Hessian integration for snorm ξ = 0.5,1,1.5,3 and Huber
  c = 0.1,0.5,1.345,3,6. Normalization, mean-zero score and Bartlett
  equality pass; derivatives are checked separately by finite differences
  away from joins. snorm's exact moment-coordinate symmetry matrix passes.
  Huber's smallest eigenvalue at c = 6 is 4.05×10⁻⁹ in the stated log chart:
  positive rank and numerical conditioning are different claims.
* Independently symbolic GPD/GEV scores and Hessians at ξ = −0.4,−0.2,0,
  0.2,0.8, with μ = 0 and σ = 1. Integrate against a unit exponential
  variable; both Bartlett and the GPD closed Fisher formula pass. GPD
  uses (log σ,ξ); GEV uses (μ,log σ,ξ). Endpoint distance is retained
  algebraically after differentiation to avoid subtraction cancellation.
* Five Tukey full-information integrations at λ = −1,−0.3,0,0.25,0.4,
  in (μ,log σ,λ). Their smallest eigenvalues are respectively 0.0776,
  0.1956,0.3220,0.4594,0.5883. Finite integration tails are documented;
  these numbers illustrate the rank proof and do not replace it.

The main checker finishes with **152 assertions passed, zero quadrature
warnings**. Score/Hessian identities generally use absolute 2×10⁻⁸ and
relative 2×10⁻⁷ tolerances; extreme-value symbolic integrals use 3×10⁻⁶.
These tolerances describe numerical agreement, not mathematical error bounds.
The original prototype encountered an endpoint cancellation and over-tight
Tukey overlap quadrature. Retaining the endpoint distance algebraically and
integrating the two quantile tails separately resolved both. No accepted
failed optimizer or capped estimate was introduced.

The [second checker](2026-11-04-infer-moving-data/critical.py) and
[results](2026-11-04-infer-moving-data/critical-results.json) add four
50-decimal mpmath comparisons (GPD power and critical endpoints, GEV critical
endpoint, GED critical cusp). Maximum relative discrepancy against SciPy is
3.26×10⁻¹³, versus a 2×10⁻⁶ acceptance threshold. At ν = 1/2, a = 1,
2,000 likelihood-ratio replications at each n use μ = 0 versus
μ = 1/√(n log n), with scale and shape known:

| n | Mean log LR | Variance log LR | Finite-h predicted variance 4nH² | Median J/(n log n) |
|---:|---:|---:|---:|---:|
| 10,000 | −0.04613 | 0.07912 | 0.08058 | 0.12278 |
| 100,000 | −0.03648 | 0.08007 | 0.07787 | 0.12314 |

The asymptotic likelihood information is 0.0625; finite-h convergence is slow.
Raw OPG's limiting value is 0.125. This simulation supports the independently
proved distinction; it is not a calibrated critical confidence procedure.
The second checker passes all **eight** high-precision/likelihood assertions.

Calibration of actual inferential targets was kept separate from matrix
identities. Under a **fully specified three-parameter null**, the χ²₃ score
test accepts as follows, 3,000 replications per cell:

| Model point | n = 100 | n = 1000 | Monte Carlo SE, approximately |
|---|---:|---:|---:|
| snorm, raw c = 0, s = 1, ξ = 1 | 0.9457 | 0.9470 | 0.0041 |
| Huber, μ = 0, σ = 1, ε = 1.345 | 0.9513 | 0.9540 | 0.0039 |

These cells check all-coordinate score calibration with **no fitted
nuisance**, not Wald covariance at a numerical fit or a symmetry test with
nuisance estimated. For GED, the held-shape, known-a = 1 location MLE is
computed by a monotone bracketed solve for ν > 1 and a median at ν = 1.
Model-Fisher 95% location Wald coverage, 1,200 replications per cell, is:

| Held ν | n = 100 | n = 1000 | Median fitted H/(nI) at n = 1000 |
|---:|---:|---:|---:|
| 1 | 0.9233 | 0.9358 | 0 |
| 1.2 | 0.9275 | 0.9533 | 0.8896 |
| 1.5 | 0.9492 | 0.9492 | 0.9941 |
| 2 | 0.9542 | 0.9425 | 1 |

Monte Carlo SEs are 0.0060–0.0077. These results demonstrate finite-sample
error and slow curvature behavior; they do not establish uniform nominal
coverage. No failed solve is discarded. The strict convexity at held ν > 1
certifies the bracketed score root as the global location maximum, conditional
on the numerical comparisons; it is not a capped search.

Seeds are 20261104 for the main bank and 20261105 for the critical bank.
Environment: Ubuntu 26.04.1, Linux 7.0.0-38 x86_64, Python 3.14.5,
NumPy 2.4.6, SciPy 1.18.0, SymPy 1.14.0, mpmath 1.3.0.
Dossier and fitter SHA-256 hashes, script hashes, and the
critical run's input-result hash are retained. No fixtures were regenerated.

## Exact proposed `inference_numeric` replacements

These are **proposals only**, replacing the entire existing list in each
named dossier. `available` retains the schema's meaning of mathematical
qualification, not implemented dispatch or certified finite differences.
The proposals conservatively retain an unavailable GED joint fitted-Hessian
scope where that particular result is open. They do not label that model
non-DQM. Critical/Poisson methods require distinct procedures, so they do not
receive ordinary numerical-Hessian eligibility here. All-held evaluation is
outside these nonempty free-subset scopes.

Applying them later also requires coordinated adapter, availability-generator
and derivative-validation changes. Metadata alone must not advertise runtime
support. All classification links below point to this report's actual sections.

### tlambda/README.md

```yaml
inference_numeric:
  - regime: "regular interior model"
    available: true
    condition: "finite mu, sigma > 0 and lambda < 1/2; any nonempty supported free subset; if lambda and mu are free require lambda > -1"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#tukey-lambda"
  - regime: "active lower fitted shape bound"
    available: false
    condition: "lambda = -1 with both lambda and mu free"
    reason: "the model is DQM with full rank, but the fitted shape bound gives a Gaussian cone experiment rather than ordinary joint Wald calibration"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#tukey-lambda"
  - regime: "critical moving endpoints"
    available: false
    condition: "lambda = 1/2; any nonempty supported free subset"
    reason: "endpoint directions use sqrt(n log n) and the endpoint-preserving complement uses sqrt(n); a dedicated mixed-rate procedure is required"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#tukey-lambda"
  - regime: "Poisson moving endpoints"
    available: false
    condition: "lambda > 1/2; any nonempty supported free subset; lambda > 1 must be held"
    reason: "endpoint directions have non-Gaussian Poisson limits, with an additional root-n endpoint-preserving shape direction when sigma and lambda are both free; lambda = 1 also constrains free shape"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#tukey-lambda"
```

### ged/README.md

```yaml
inference_numeric:
  - regime: "held location interior"
    available: true
    condition: "finite mean held, sd > 0 and nu > 0; if nu is free require nu > 1; any nonempty remaining free subset"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#ged"
  - regime: "moving location with smooth or held shape"
    available: true
    condition: "finite mean free, sd > 0; either nu > 2, or nu > 1 is held; sd may be held or fitted"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#ged"
  - regime: "joint fitted cusp Hessian open"
    available: false
    condition: "finite mean free, sd > 0 and 1 < nu <= 2 with nu free"
    reason: "the model is DQM with full-rank information and an integrable correct a.e. Hessian at the truth; consistency of the joint fitted Hessian and cusp-crossing numerical derivatives remains unvalidated"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#ged"
  - regime: "regular location with unsuitable ordinary Hessian"
    available: false
    condition: "finite mean free, sd > 0 and 1/2 < nu <= 1 held"
    reason: "the model is DQM with full rank, but the ordinary location Hessian omits the Laplace kink at nu = 1 and is nonintegrable below 1; below 1 the location MLE also makes raw fitted OPG singular"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#ged"
  - regime: "critical or fractional cusp location"
    available: false
    condition: "finite mean free, sd > 0 and 0 < nu <= 1/2 held"
    reason: "location uses sqrt(n log n) at nu = 1/2 or an fBm likelihood at rate n^(1/(1+2*nu)) below it; ordinary Hessian inference is invalid and raw critical OPG has the wrong information constant"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#ged"
  - regime: "active fitted Laplace shape bound"
    available: false
    condition: "nu = 1 free, finite mean held or fitted, sd > 0"
    reason: "the regular underlying model has a constrained Gaussian shape tangent; moving-location ordinary Hessian inference additionally misses the cusp contribution"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#ged"
```

### gpd/README.md

```yaml
inference_numeric:
  - regime: "known threshold regular model"
    available: true
    condition: "finite mu held, sigma > 0 and xi > -1/2; sigma and/or xi free, including xi = 0 with xi free"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#gpd-and-gev"
  - regime: "critical upper endpoint"
    available: false
    condition: "finite mu held, sigma > 0 and xi = -1/2; sigma and/or xi free"
    reason: "the endpoint has a sqrt(n log n) Gaussian experiment and its complement has root-n rate; ordinary finite Fisher inference is unavailable"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#gpd-and-gev"
  - regime: "Poisson upper endpoint"
    available: false
    condition: "finite mu held, sigma > 0 and -1 <= xi < -1/2; sigma and/or xi free"
    reason: "the endpoint has a non-Gaussian Poisson experiment at rate n^(-xi); both free coordinates also permit a root-n shape tangent; free xi = -1 adds a fitted-bound constraint"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#gpd-and-gev"
  - regime: "unsupported free threshold"
    available: false
    condition: "mu free"
    reason: "the fitter does not estimate the threshold; known-threshold regularity does not justify this subset"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#gpd-and-gev"
```

### gev/README.md

```yaml
inference_numeric:
  - regime: "regular interior model"
    available: true
    condition: "finite mu, sigma > 0 and xi > -1/2; any nonempty supported free subset; if xi is free require xi < 1"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#gpd-and-gev"
  - regime: "critical upper endpoint"
    available: false
    condition: "finite mu, sigma > 0 and xi = -1/2; any nonempty supported free subset"
    reason: "one endpoint direction has a sqrt(n log n) Gaussian experiment and the complement has root-n rate; ordinary finite Fisher inference is unavailable"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#gpd-and-gev"
  - regime: "Poisson upper endpoint"
    available: false
    condition: "finite mu, sigma > 0 and -1 <= xi < -1/2; any nonempty supported free subset"
    reason: "one endpoint direction has a non-Gaussian Poisson experiment at rate n^(-xi), with a root-n complement; free xi = -1 additionally constrains shape"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#gpd-and-gev"
  - regime: "active upper fitted shape bound"
    available: false
    condition: "finite mu, sigma > 0 and xi = 1 free"
    reason: "the model is DQM with full rank but the fitted shape bound gives a Gaussian cone experiment rather than ordinary joint Wald calibration"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#gpd-and-gev"
  - regime: "held shape below fitted endpoint range"
    available: false
    condition: "xi < -1 held and at least one coefficient free"
    reason: "the mathematical endpoint experiment is nonregular; nontrivial accepted free-scale exceptions occur only when all observations are below held mu, an event of probability exp(-n) under this model"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#gpd-and-gev"
```

### snorm/README.md

```yaml
inference_numeric:
  - regime: "regular two-piece normal interior"
    available: true
    condition: "finite mean, sd > 0 and finite xi > 0; any nonempty free subset, including xi = 1 with xi free; observed information uses the a.e. piecewise Hessian with defined join handling"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#two-piece-normal-snorm"
```

### huber/README.md

```yaml
inference_numeric:
  - regime: "regular finite Huber interior"
    available: true
    condition: "finite mu, sigma > 0 and finite epsilon > 0; any nonempty free subset; observed information uses the a.e. piecewise Hessian with defined join handling; normal and Laplace limits are excluded"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#huber"
```

## Open results

No entire family was omitted. The following family-specific results were
**not finished**, and are not encoded as positive procedure claims:

1. **GED, mean and ν jointly fitted with 1 < ν ≤ 2:** a proof of consistency
   of H evaluated at the joint fit, including the data-dependent cusp
   proximity when ν is estimated. The held-shape conditional residual proof
   above does not extend just by substituting ν-hat, because it then depends
   on the observation being conditioned on. A joint implicit-map/leave-one-out
   bound or a different validated information estimate is needed. All-free
   model DQM, rank and true-point Hessian expectation are settled separately.
2. **tlambda, GED below 1, and nonregular GPD/GEV:** the exact sampling law
   and coverage of the package's returned numerical estimate, including a
   proved optimization error small enough on the local likelihood scale.
   Current fitter tolerances include O(n) face-comparison allowances and
   finite searches. These do not vanish at the O(1) local likelihood scale
   merely because the model has the derived experiment. Full-process
   argmax localization for every conditional fitter and uniform limits on
   transition sequences remain open. This report establishes density geometry
   and local experiments, not a new numerical MLE certificate.
3. **snorm/Huber production inference, and newly eligible regular endpoint
   scopes:** join/support-aware derivative adapters and nuisance-fitted
   covariance/test calibration. The main engine currently uses fixed nested
   Richardson stencils. DQM and an a.e. correct H do not give those stencils
   fourth-order truncation uniformly across a join; a stencil can also cross
   support. Analytic piecewise derivatives or a controlled step-refinement
   scheme, followed by calibration, are concrete missing work. Existing
   snorm dispatch permits only held ξ = 1; Huber and Tukey remain unavailable,
   GEV permits only held ξ = 0, and GPD's free ξ gate excludes zero and
   negative values. None of these gates was edited here.
4. **All six families near transitions or under misspecification:** uniform
   coverage, robust sensitivity/calibration and validity of the ordinary
   n-out-of-n bootstrap. Necessary crossover quantities were derived above;
   they are not sufficient uniform coverage theorems. Critical GED especially
   requires a separate information construction, not rescaling raw J.

No change to settled `dpqr.hpp`, `special/`, `truth.*` or generated d/p/q/r
is needed by these results. Any implementation should first change inference
adapters/documentation and its own validation; altering default fits or goldens
is not authorized by this research.

## Final verification

All commands finished in the foreground through the workstation slot locks.
The common environment was:

```sh
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
mkdir -p "$TMPDIR"
```

| Command | Machine | Final outcome |
|---|---|---|
| `/tmp/umvl-cpu/run4 just cpp-test` | workstation | 106/106 dev shards passed; 0 failed; ctest elapsed 65.25 s |
| `/tmp/umvl-cpu/run4 just test` | workstation | FAIL 0, WARN 0, SKIP 154, PASS 20612; testthat duration 121.8 s, runner elapsed 124.383 s |
| `/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-04-infer-moving-data/check.py` | workstation | 152 assertions passed; 0 quadrature warnings |
| `/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-04-infer-moving-data/critical.py` | workstation | 8 assertions passed, including four high-precision comparisons |
| `/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-04-infer-moving-data/validate_report.py` | workstation | Six schema-shaped replacement lists, 21 regime rows, local links, source/result/script hashes and working-tree scope checked |

The optional ignored HTML preview was rendered with Pandoc `--mathml` and
checked with the note skill's strict HTML validator: zero errors or code-math
candidates, 28 static MathML elements. The retained
[offline browser check](2026-11-04-infer-moving-data/preview.mjs) confirmed
28 visible MathML elements, no math errors, scripts or page errors, with
networking disabled; the endpoint formulas and cases display were also
visually inspected. Its [record](2026-11-04-infer-moving-data/preview-validation.json)
and the [handoff validation record](2026-11-04-infer-moving-data/validation.json)
are retained. This preview check is not a package-site test.
Opening the validated preview with the prescribed `codex-math-web/bin/show`
was attempted, but the viewer returned HTTP 403 because this worktree is
outside its configured readable roots. No viewer configuration was changed
and no file was copied outside this worktree. The report and preview remain
available at their local paths; this is a delivery limitation, not a render
or numerical-check failure.

The 154 R skips are explicitly “requires full test tier”, not unexpected
missing dependencies. C++ compiler: GCC 15.2.0; R 4.6.1; Rcpp 1.1.2;
BH 1.90.0.1. The CMake run used BH headers after the system Boost probe;
the R suite assembled its generated package without editing tracked sources.
The research-only change does not touch fitting, d/p/q/r or shared test
infrastructure, so the required dev tiers were run; no full, no-long-double,
JS or site suite is claimed. The additional research scripts are evidence
consumers, not new shared test infrastructure.

The numerical-bank first attempts and their resolved cancellation/quadrature
issues are described above; they are not hidden as successful runs. Final
source revision and family hashes are in the retained JSON. All final numerical
and dev suite runs are green. The retained evidence consists of small JSON
files and reproducer scripts; scratch compiler/test logs and the optional
HTML preview stay under ignored `build/`.
