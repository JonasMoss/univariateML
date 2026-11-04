# Interior regularity classification, part a — TASK-102.13

2026-11-04. Tested source: `b92c6189497f00b5917c8da8c29d07cc494f896d`,
branch `codex/dqm-a`. Research only: this report and its
[evidence directory](2026-11-04-dqm-a-data/) are the entire change.

All **31 assigned families** are classified in the
[14-column CSV](2026-11-04-dqm-a-data/classification.csv). Under the holds
specified below, 28 families are DQM throughout their README interiors;
27 have finite nonsingular information throughout. The exceptions are:

- **GED:** DQM and finite full-rank information exactly when `nu > 1/2`.
- **GPD and GEV:** DQM and finite full-rank DQM information exactly when
  `xi > -1/2`. The formal pointwise score second moment at `xi = -1` is a
  separate, finite matrix; it does not restore regularity.
- **Generalized beta (genbeta):** DQM everywhere, but information has rank
  two instead of three at `shape2 = 1`.

**Huber is regular** with its design epsilon held, despite its moving
second-derivative joins. Its ordinary piecewise Hessian estimates information.
The joint fitted GED Hessian for `1 < nu <= 2` remains an explicitly bounded
open theorem below; its true-parameter expectation is settled.

## Scope and conventions

Every coefficient of kind `regular` is free. Every README `design` or
`threshold` coefficient is held at a proper finite value. In particular,
**huber epsilon is held**, even though its fitter can also estimate it.
The held quantities are fatigue/frechet/hcauchy/hnorm/ht `mu`, fpareto `min`,
genbeta `scale`, gpd `mu`, and huber `epsilon`. There are **no integer-valued
coefficients** in these 31 families; ht `nu` is a positive real coefficient.
The table below gives the exact free-coordinate names and order.

“Interior” refers to the README **density domain**, not `fit_domain`.
Thus GED includes all `nu > 0`, and GPD/GEV include all finite real `xi`.
The fitter's GED `nu >= 1`, GPD/GEV `xi >= -1`, and GEV `xi <= 1`
restrictions have separate consequences when active. Bhatt `a = 0` and
foldnorm `mu = 0` are README domain boundaries, not interior singularities.
Holding a threshold here does not classify the different model that estimates it.
The [canonical inference plan](../../roadmap/2026-10-30-inference-and-dqm.md)
remains the coordination reference.

For i.i.d. observations from the stated model, use the original-data log density,
original free coordinates, per-observation score `s`, and

\[
 I=E_\theta[ss^T],\qquad
 H_n=-\sum_{i=1}^n\partial_\theta^2\log p_\theta(X_i),\qquad
 J_n=\sum_{i=1}^n s_\theta(X_i)s_\theta(X_i)^T.
\]

A positive Hessian statement means `H_n/n -> I`, not an optimizer's inverse
Hessian or the outer product of the total score. Except where explicitly
qualified, it holds at the truth and at consistent fitted parameters through a
local uniform law. The moving-endpoint fitted statements assume a root-n
consistent estimate in the regular regime, as in the prior report. These are
model information statements, not misspecification-robust sensitivity results.

The CSV `cramer` field means the **requested derivative conditions**:
there is a parameter neighborhood and a common probability-one observation
set on which log densities are C³, with third derivatives bounded throughout
that neighborhood by an integrable envelope under the truth. Nonsingularity
and estimator assumptions are recorded separately. For example, genbeta
satisfies these derivative conditions even where its information is singular.
The term does not assert every hypothesis of a classical MLE theorem.

Smoothness `k` is local at the specified parameter and concerns all free
coordinates jointly. `k_ae = infinity` means that for almost every fixed
observation there is an observation-dependent parameter neighborhood on which
the log density is smooth. It does **not** supply one neighborhood working
for all observations. This distinction matters for moving support and kinks.

The README sometimes calls a **closed endpoint** part of support while its
chosen density is zero or infinite there. A real-valued log density then has
no C⁰ classification at that endpoint. The CSV records this literally:
fatigue/frechet at `x = mu`, fpareto at `x = min`, genbeta at `x = scale`,
and finite GPD/GEV endpoints. At the fpareto/genbeta parameter values giving a
finite endpoint density, varying the free shapes still destroys real-valued
continuity there. At GPD/GEV `xi = -1`, moving the endpoint likewise destroys
continuity. For the fixed-support families these endpoints are a **common null
set**, removable for the derivative conditions; on the positive-density
observation interior the log density is C∞. Endpoint density versions do not
alter DQM or information. Hnorm/hcauchy/ht and exponential have finite smooth
log densities even at their held lower endpoints.

## Family-by-family arguments

Unless singled out in the next section, each row has DQM, finite positive
definite information, C∞ log density on the positive-density observation
interior, fixed support under its holds, the stated Cramér derivative
conditions, and a consistent ordinary observed Hessian. The closed-endpoint
qualification above remains part of the literal every-x classification.
The arguments here are analytical; finite numerical checks are described
separately under validation.

| id | Free coordinates | Held | One-line argument |
|---|---|---|---|
| beta | shape1; shape2 | none | Full exponential family with independent log(x), log(1-x) statistics. |
| betapr | shape1; shape2; scale | none | Fixed log transform gives the a=1 generalized logistic (GB2) submodel; score independence inherited. |
| bhatt | mu; sigma; a | none | Normal plus a*Uniform(-1,1); Gaussian mixture derivative envelopes; mean/variance/fourth cumulant have full-rank Jacobian for a>0. |
| burr | shape1; shape2; scale | none | Log-data generalized logistic with one beta shape fixed; GB2 rank proof and logarithmic tail envelopes. |
| cauchy | location; scale | none | Bounded rational location/scale scores; odd/even independence gives I=diag(1,1)/(2*scale^2). |
| emg | mu; sigma; lambda | none | Normal plus independent exponential; latent scores imply DQM; first three cumulants have nonsingular Jacobian. |
| exp | rate | none | One-parameter full exponential family; I=1/rate^2. |
| fatigue | alpha; beta | mu (known threshold) | Held shift: direct exp(-c*y-c/y) envelopes; alpha/beta scores have independent upper/lower-tail leading terms. |
| foldnorm | mu; sigma | none | Fixed folding of normal preserves DQM; positive-mu score grows linearly and sigma score quadratically in the upper tail. |
| fpareto | shape1; shape2; shape3; scale | min (known threshold) | Fixed translation of GB2; same information rank and parameter envelopes. |
| frechet | lambda; sigma | mu (known threshold) | Held shift and fixed log transform give Gumbel in smooth invertible coordinates. |
| gamma | shape; scale | none | Full exponential family with independent log(x), x statistics. |
| gb2 | shape1; shape2; shape3; scale | none | Log-data generalized logistic; opposite tail slopes and t*exp(-a*abs(t)) corrections prove full score rank. |
| ged | mean; sd; nu | none | Square-root cusp derivative is L2 iff nu>1/2; odd location score and independent even scale/shape scores. |
| genbeta | shape1; shape2; shape3 | scale (known threshold) | Y=-log(X/scale) has density a*exp(-a*p*y)*(1-exp(-a*y))^(q-1)/B(p,q); tail correction identifies a except q=1. |
| gev | mu; sigma; xi | none | Moving endpoint power alpha=-1/xi for xi<0; square-root derivative and support loss give threshold xi>-1/2; quantile derivatives prove rank. |
| gompertz | a; b | none | Superexponential tail dominates all local derivatives; x*exp(b*x) term separates b from a score. |
| gpd | sigma; xi | mu (known design) | Known lower endpoint; moving upper endpoint power alpha=-1/xi; exact information determinant positive for xi>-1/2. |
| gumbel | mu; sigma | none | Exponential variable exp(-(X-mu)/sigma) supplies finite log/power envelopes; location/scale scores independent. |
| hcauchy | sigma | mu (known threshold) | Known threshold and fixed absolute-value transform of Cauchy; nonconstant scale score. |
| hnorm | sigma | mu (known threshold) | Known threshold: square of standardized half-normal is chi-square(1); positive scale information 2/sigma^2. |
| ht | nu; sigma | mu (known threshold) | Known threshold: fixed folding of location-zero t; nu score has logarithmic tail while scale score tends to nonzero constant. |
| huber | mu; sigma | epsilon (known design) | Held epsilon: continuous matching first derivatives; exponential envelopes, odd/even independent scores and piecewise Hessian uniform LLN. |
| invburr | shape1; shape2; scale | none | Fixed reciprocal transformation of Burr, with invertible reciprocal scale map. |
| invexp | scale | none | Fixed reciprocal of exponential in rate=scale coordinates. |
| invgamma | alpha; beta | none | Fixed reciprocal of gamma with shape=alpha and gamma scale=1/beta. |
| invgauss | mean; shape | none | Full exponential family with independent x and 1/x statistics; nonsingular map from mean/shape. |
| invparalogis | shape; scale | none | Smooth full-rank two-coordinate embedding in inverse Burr (both shapes equal); inherits positive information. |
| invpareto | shape; scale | none | Smooth full-rank embedding in inverse Burr with second shape fixed to one. |
| invtrgamma | shape1; shape2; scale | none | Log reciprocal data give log-gamma location/scale/shape; independent t, exp(a*t), t*exp(a*t) terms prove rank. |
| invweibull | shape; scale | none | Fixed reciprocal Weibull, equivalently a held-threshold Frechet; invertible shape/scale map. |

## Exceptional regimes and smoothness

| Family | DQM and information | Largest k for every positive-density x | k almost everywhere | Support moves under the listed free coordinates | Cramér derivative conditions | Ordinary Hessian |
|---|---|---|---|---|---|---|
| ged | DQM/full rank iff ν > 1/2; infinite location information otherwise | ceil(ν) − 1 with ν free | ∞ away from x = mean | No | Yes iff ν > 3 | Correct finite expectation and truth LLN iff ν > 1; fitted yes for ν > 2, open for joint 1 < ν ≤ 2 |
| genbeta | DQM everywhere; rank 2/3 iff shape2 = 1 | ∞, excluding the closed upper endpoint as explained above | ∞ | No: scale held | Yes, modulo common null endpoint | Yes, including equality to singular I on shape2 = 1; no ordinary joint inverse there |
| gev | DQM/full rank iff ξ > −1/2 | ∞; finite closed endpoint lacks real C⁰ log density | ∞ | Yes locally at every ξ, including ξ = 0 | No throughout the all-free model | Yes for ξ > −1/2 under the stated fitted-rate condition; no finite joint DQM information estimate otherwise |
| gpd | DQM/full rank iff ξ > −1/2 | ∞; at ξ < 0 the closed upper endpoint lacks C⁰ | ∞ | Locally yes iff ξ ≤ 0; lower threshold always held | Yes iff ξ > 0 | Same regularity threshold and fitted-rate qualification as GEV |
| huber | DQM/full rank for every held finite ε > 0 | 1, including the joins | ∞ away from μ ± σε | No | No: second derivatives jump | Yes: use the ordinary a.e. piecewise Hessian |

There are no other interior singular or locally unidentified points in the
assigned scope. In particular, equal GB2 shapes, Burr special members,
positive Bhattacharjee width, and positive folded-normal location introduce
no rank loss. At the excluded boundary `foldnorm mu = 0`, its location score
vanishes; at `bhatt a = 0`, its width score vanishes. Neither boundary is an
exception to the interior claims.

### Generalized logistic / GB2 rank, and inherited submodels

Write `p = shape3`, `q = shape1`, `a = shape2`, `c = log(scale)` and
`t = log(x) - c`. After the fixed log-data transformation, GB2 has density

\[
 g(t)=\frac{a e^{apt}}{B(p,q)(1+e^{at})^{p+q}}.
\]

On compact positive parameter sets it has two exponential tails, while score
and log-derivative envelopes grow polynomially in `|t|`. These give L²
square-root differentiability and the required integrable derivative bounds.
To prove full rank, suppose a parameter direction has zero score for every t.
At positive and negative infinity the leading log-density slopes first force
`δ(aq) = δ(ap) = 0`. After removing those slopes and their constants, the
`t exp(-a|t|)` term in the differentiated first tail correction forces
`δa = 0`, since its coefficient contains `p + q > 0`. The slope equations
then give `δp = δq = 0`; the remaining location score forces `δc = 0`.
Analyticity extends an a.e. score identity to all t. Hence information is
positive definite, not merely positive at a numerical grid.

Betapr fixes `a = 1`; Burr fixes `p = 1`. Fpareto is GB2 after its held
translation. Invburr is Burr under the fixed reciprocal transformation and
an invertible reciprocal scale map. Invparalogis ties the two inverse-Burr
shapes to the same positive shape, and invpareto fixes its second shape to
one. Each embedding has full column-rank Jacobian, so its information is
`D^T I D > 0`. This also explains why overlap with a simpler named member
does not imply local singularity when additional shape directions are free.

### Genbeta: an interior identification ridge

Put `p = shape1`, `q = shape2`, `a = shape3`, with `b = scale` held.
For `Y = -log(X/b) > 0`,

\[
 g(y)=\frac{a e^{-apy}(1-e^{-ay})^{q-1}}{B(p,q)}.
\]

Near zero its density is a positive constant times `y^(q-1)`; at infinity it
has an exponential tail. Local score and derivative bounds involve powers
of y and `|log y|`, integrable on compact positive parameter sets. Thus DQM
and the Cramér derivative bounds hold at **every** positive p,q,a, even at
the ridge.

If `q != 1`, a zero score direction first gives `δ(ap) = 0` from the tail
slope. The `y exp(-ay)` correction then forces `δa = 0`, since its coefficient
contains `q - 1`; hence `δp = 0`. The nonconstant q score finally gives
`δq = 0`. At `q = 1`, instead, `g(y) = ap exp(-ap y)`: changing p and a
with product ap fixed leaves the law **exactly unchanged**. The q score
contains `log(1-exp(-ay))`, independent of the affine-in-y product score.
Consequently rank is exactly two, with a one-dimensional null tangent
`q` fixed and `a δp + p δa = 0`. This is exact local nonidentification,
not just poor conditioning. The ordinary expected Hessian equals this
singular information. In the q-free joint model, replacing three by two
in an AIC formula or using a pseudoinverse is not thereby justified.

### Other rank and envelope checks

For Bhattacharjee, the cumulant map is

\[
 (E X,\operatorname{Var}X,\kappa_4)
   =(\mu,\sigma^2+a^2/3,-2a^4/15),
 \qquad \det D=-16\sigma a^3/15\ne0\quad(a>0).
\]

Representing the uniform component as `a V` for fixed `V ~ Uniform(-1,1)`
gives DQM by marginalizing a smooth Gaussian latent experiment. Gaussian
conditional moments bound log derivatives polynomially in `|x|`. If a score
direction were zero, every differentiable cumulant direction would be zero,
contradicting this Jacobian. EMG similarly has cumulants

\[
 (\mu+\lambda^{-1},\sigma^2+\lambda^{-2},2\lambda^{-3}),
 \qquad \det D=-12\sigma/\lambda^4\ne0.
\]

Its normal/exponential latent scores have finite second moments; marginalizing
preserves DQM. Gaussian-tail Mills-ratio bounds and the exponential right tail
give local polynomial log-derivative envelopes. Foldnorm inherits DQM from
fixed folding; when μ > 0 its two scores have independent linear and quadratic
upper-tail terms. Smooth log-cosh derivatives have polynomial envelopes.

For invtrgamma, the log-reciprocal variable has log density
`log(a) - log Gamma(p) + pa t - exp(a t)`, where
`a = shape2`, `p = shape1`, `t = log(scale) - log(x)`.
A zero score combination has a `t exp(a t)` coefficient forcing the power
direction to zero, an `exp(a t)` coefficient forcing the location direction
to zero, and a linear t term forcing the gamma-shape direction to zero.
The exponential/superexponential tails give derivative envelopes.
Invweibull fixes p to one. Frechet with held mu becomes a Gumbel
location-scale model under `log(x-mu)`.

For fatigue, setting `y = x-mu` leaves tails bounded by powers times
`exp(-c y-c/y)`. All needed powers, inverse powers and logarithms are
integrable locally. The alpha score's leading terms at infinity and zero
are proportional to `y/(alpha^3 beta)` and `beta/(alpha^3 y)`; the beta
score's corresponding terms are `y/(2 alpha^2 beta^2)` and
`-1/(2 alpha^2 y)`. The opposite sign of the second ratio proves independence.
For Gompertz, its superexponential tail dominates `x^j exp(b x)` envelopes;
the b score contains `x exp(b x)`, absent from the a score.

Beta, gamma, exponential, inverse Gaussian and the reciprocal exponential/gamma
families follow from full exponential families with independent sufficient
statistics and nonsingular parameter maps. Cauchy has bounded rational scores.
For held-threshold half-t, the nu score grows logarithmically in the tail,
whereas the scale score tends to a nonzero constant; power tails with any
fixed positive nu integrate every required logarithmic power. The corresponding
half-normal and half-Cauchy scale arguments are immediate. These arguments
also supply local envelopes; pointwise differentiability alone was not used
as a DQM or rank proof.

### GED: three distinct thresholds, and the fitted-Hessian gap

Reuse the [moving-feature report](2026-11-04-infer-moving-report.md), GED
section, rather than treating its cusp as a blanket disqualification. In its
auxiliary power scale
`a = sd sqrt(Gamma(1/nu)/Gamma(3/nu))`,

\[
 s_\mu=\nu a^{-\nu}\operatorname{sign}(r)|r|^{\nu-1},\quad
 I_{\mu\mu}=\frac{\nu^2}{a^2}
       \frac{\Gamma(2-1/\nu)}{\Gamma(1/\nu)},\quad\nu>1/2.
\]

The squared location score is locally integrable exactly when ν > 1/2;
at equality the divergence is logarithmic. The scale/shape scores are even
functions involving `1,T,T log T`, `T = |r/a|^nu`, with finite moments and
independent nonconstant terms. Location is odd. Thus information is full rank
where finite, with no finite singular interior points. The square-root cusp
argument in that report supplies DQM exactly above this threshold.

With **shape free**, `|x-mu|^nu` is locally jointly C^k for
`k = ceil(nu)-1`, including even integers. At `nu = 2m`, fixing the shape
would give a polynomial, but joint variations from below contain derivatives
of order 2m proportional to `|r|^(nu-2m)`, unbounded as r tends to zero
sufficiently fast. This explains the difference from a held-shape normal
submodel. Away from r = 0, all orders exist locally. C³ throughout a common
parameter neighborhood with a dominated third derivative holds exactly when
ν > 3. Below or at three, moving the location through a positive-probability
observation band prevents that common C³ condition.

The ordinary a.e. negative location Hessian is

\[
 h_{\mu\mu}=\nu(\nu-1)a^{-\nu}|r|^{\nu-2}.
\]

For ν > 1 it is integrable and its expectation equals `I_mumu`; the other
entries satisfy the corresponding identities. Its truth LLN needs no finite
variance. At ν = 1 it is zero and misses the Laplace kink contribution;
for 0 < ν < 1 it is negative and nonintegrable. Thus DQM on `(1/2,1]` does
not license the ordinary Hessian. For ν > 2, continuity of second derivatives
and a local integrable envelope give fitted-Hessian consistency even on
`(2,3]`, where the classical third-derivative condition fails.

**Open: joint fitted Hessian for 1 < ν ≤ 2.** The prior report proves a
held-shape exact-location-MLE result with a residual-density bound, but leaves
the joint shape fit open. The extra obstacle is dependence of the fitted
shape and location on each observation. A uniform Hessian LLN over arbitrary
nearby parameters cannot fix this: for ν < 2 its supremum is infinite at
sample knots. Even at a true ν = 2, take the fitted location to be the
observation nearest the true mean and `nu_n = 2 - 1/n`; together with the
true scale these form root-n consistent parameter sequences, yet their knot
Hessians are undefined/infinite. This is a counterexample to inferring the
claim from consistency alone, not a counterexample for the actual MLE.

A possible extension would condition on all other observations, solve the
joint score system, and bound its nuisance-adjusted residual Jacobian by
`C{1+n^(-1)|r|^(nu-2)}` uniformly over the fitted shape neighborhood, then
control the small-residual sum. The held-shape argument does not establish
that bound after shape profiling, nor does DQM. No such bound or separate
joint-MLE theorem has been verified here. The missing result is exactly this
joint fitted-Hessian consistency theorem, including finite join handling at
ν = 2; it is **not** an open DQM, rank or true-Hessian-mean classification.
No numerical finite-difference eligibility is claimed for this regime.

### GPD/GEV: support, regular information, and the xi = -1 exception

Reuse the [moving-feature report](2026-11-04-infer-moving-report.md),
GPD/GEV section. For ξ < 0, upper-endpoint distance d has density
`c d^(alpha-1)` times a controlled smooth factor, where `alpha = -1/xi`.
Endpoint displacement loses mass of order `|h|^alpha`; the squared derivative
has local order `d^(alpha-3)` when its leading coefficient is nonzero.
DQM holds exactly for alpha > 2, or ξ > −1/2. At equality the derivative
integral diverges logarithmically. Below equality the moving endpoint is
nonregular. This conclusion includes the uniform-type alpha = 1 case even
though its leading pointwise derivative coefficient cancels.

At ξ > 0 the finite lower GEV endpoint has exponentially vanishing density;
at ξ = 0 the limiting Gumbel expansion is smooth for each finite observation.
The joint result is DQM iff ξ > −1/2. This agrees with
[Bücher and Segers, Proposition 3.2](https://arxiv.org/html/1601.05702v3),
also the source used in the prior report. For GPD with known mu, direct
integration gives

\[
 I=\begin{pmatrix}
 [\sigma^2(1+2\xi)]^{-1}&[\sigma(1+\xi)(1+2\xi)]^{-1}\\
 [\sigma(1+\xi)(1+2\xi)]^{-1}&2[(1+\xi)(1+2\xi)]^{-1}
 \end{pmatrix},\qquad
 \det I=\frac{1}{\sigma^2(1+\xi)^2(1+2\xi)}>0.
\]

For GEV, independent quantile variations in `1,v^(-xi),v^(-xi) log(v)`
prove rank when ξ != 0; at zero use `1,log(v),(log(v))^2`.
No zero-shape singularity occurs.

The **strict classical derivative condition fails for all-free GEV at every
xi**, including zero: any parameter neighborhood sweeps a positive-probability
observation band out of support. There cannot be a common finite log-density
neighborhood or an integrable supremum of its derivatives on that band.
For GPD this occurs exactly at xi ≤ 0; at positive xi a smaller positive-shape
neighborhood has fixed support and dominated third derivatives. Counting
integrable third derivatives at a single negative-shape parameter, or just
using an endpoint moment threshold of alpha > 3, would not establish the
requested neighborhood domination. This distinction is also explained in
[Bücher and Segers, Section 2](https://arxiv.org/html/1601.05702v3).

Nevertheless, the prior report supplies the ordinary Hessian identities and
consistency in the regular regime, including their evaluation at root-n
consistent fits. The shrinking fitted endpoint displacement is negligible
relative to the minimum endpoint distance when alpha > 2. These statements
concern exact derivatives; they do not validate an arbitrary finite-difference
stencil crossing support.

Outside the DQM regime, the formal score second moment diverges in an
endpoint-moving direction **except at xi = -1**. At that point let sigma = 1
for notation, and use log scale as the scale coordinate. For GPD,
`T = 1-(X-mu)` is uniform on `(0,1)`, and its pointwise scores are

\[
 (s_{\log\sigma},s_\xi)=(-1,\log T),\qquad
 E[ss^T]=\begin{pmatrix}1&1\\1&2\end{pmatrix}.
\]

For GEV, `T = 1-(X-mu)` is Exp(1), and its scores in
`(mu,log(sigma),xi)` are

\[
 (-1,\ T-2,\ (1-T)\log T+T-1).
\]

Both raw second-moment matrices are finite and full rank by independence of
the displayed functions. Every displayed score has mean −1: support terms
are missing from formal differentiation. The **centered score covariance**
is singular, since the first displayed coordinate is constant (ranks one
and two respectively). Hence neither raw OPG nor centered covariance is an
ordinary DQM Fisher information matrix here. The CSV gives both conventions
explicitly instead of falsely labeling this point simply “infinite Fisher”.
GPD's ordinary scale Hessian already has the wrong sign at this uniform
member; GEV's location Hessian is zero there. There is no valid finite joint
regular Hessian information estimate at these non-DQM points.

### Huber

With epsilon `c > 0` held and `z = (x-mu)/sigma`, the score in
`(mu,log(sigma))` is

\[
 \left(\frac{\operatorname{clip}(z,-c,c)}{\sigma},\quad
       z\operatorname{clip}(z,-c,c)-1\right).
\]

It is continuous at z = ±c and has a square-integrable polynomial envelope
under exponential tails, proving DQM. On the nonempty core interval the
location score is linear and odd while the scale score is quadratic and
even, proving full rank. Second derivatives jump, so the largest every-x k
is one. Piecewise integration has **no first-derivative jump**, hence no
missing point mass in the Hessian identity. Interval-indicator classes and
integrable polynomial envelopes give a uniform Hessian law at consistent
fits. These are the arguments established in the prior moving-feature report;
its stronger free-epsilon result also implies this held-epsilon submodel.

## Consequences for statistical practice

At regular, identifiable interior truths, smooth contrasts and ordinary
finite-dimensional interior hypotheses have the standard model Wald/LR and
regular AIC framework **provided** a consistent appropriate likelihood
solution and its likelihood expansion are available. This classification
does not certify every repository fitter's global optimization or asymptotic
numerical accuracy. Active fit restrictions require constrained inference,
even when the density model at that point is DQM. For example, GED at its
free-shape fit bound ν = 1 has a constrained shape tangent and an unsuitable
ordinary location Hessian; GEV at ξ = 1 has a constrained shape tangent.

For genbeta on its identification ridge, joint Wald inversion and regular
three-parameter LR/AIC formulas need singular-model analysis. For non-DQM
GPD/GEV and GED regimes, ordinary joint root-n covariance is unavailable;
endpoint-preserving or held-parameter submodels may have different results.
For regular GED with ν ≤ 1, Fisher or a justified score-based route replaces
the ordinary location Hessian. No bootstrap validity, misspecification
robustness, coverage calibration or implemented procedure availability follows
from DQM alone. Those are separate tasks under the inference plan.

## Validation and reproducibility

All runs used the **local workstation**, the prescribed slot locks, `nice`
from those wrappers, and one BLAS/OpenMP thread per worker. No simbox,
sub-agents, installations, board edits, git writes, library/binding/dossier/test
changes, or golden/truth regeneration occurred. The R package was assembled
only as ignored test output. The full tier was not required or run: fitting,
d/p/q/r and test infrastructure were unchanged.

Environment: Linux `7.0.0-38-generic`, x86_64, glibc 2.43; GCC 15.2.0;
R 4.6.1 (2026-06-24), BH 1.90.0.1; Python numerical environment recorded in
[checks.json](2026-11-04-dqm-a-data/checks.json): NumPy 2.4.6,
SciPy 1.18.0, SymPy 1.14.0 and PyYAML 6.0.3.

Every command ran in the worktree with

```sh
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp
export XDG_RUNTIME_DIR=$PWD/build/tmp
mkdir -p "$TMPDIR" build/scratch
```

The table is reproducible with

```sh
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-04-dqm-a-data/make_classification.py
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-04-dqm-a-data/check_classification.py
```

Final numerical/schema run: **PASS**, 31 metadata rows, exact 14-column
schema, 11 score quadratures, three symbolic identities, no integration
warnings. The generator's mathematical labels are authored conclusions;
matching them to metadata verifies coordinate coverage, not their theorems.
The independent quadrature calls no library likelihood or fitted estimator.
Its deterministic workloads are:

- GB2 `(p,q,a) = (1,1,1), (0.7,1.3,1.1), (2,2,0.8), (1,3,2)`;
  integration in `t` over `[-100/a,100/a]` in coordinates
  `(p,q,log(a),log(scale))`.
- Genbeta `(p,q) = (1,1), (2,1), (0.8,0.7), (1.2,2), (2.5,4)`;
  integration in `z = aY` over `(0,100/p)` in coordinates
  `(p,q,log(a))`. The two q = 1 cases have numerical zero eigenvalues
  (magnitude below `3e-16`) and the exact product null direction. Their other
  eigenvalues are positive. The nonsingular tested cases have minimum
  eigenvalues at least `2.18e-4`. Maximum absolute OPG/Hessian discrepancy
  is below `4.8e-12` across these five cases.
- GPD and GEV at xi = −1, sigma = 1: direct integrations of the displayed
  score vectors under uniform and exponential T, confirming nonzero means,
  full-rank raw second moments and deficient centered covariance.
- Symbolic nonzero cumulant Jacobians for Bhattacharjee and EMG, and the exact
  GPD information determinant.

Quadrature tolerances are `1e-10` absolute/relative for GB2 and `1e-9` for
the other integrations. Assertions allow `1e-8` or `1e-7` mass/score/identity
error as specified in the checker, `1e-9` numerical null eigenvalues for
genbeta, and strictly positive nonsingular eigenvalues above `1e-5` at the
selected points. Results, estimated quadrature errors and source SHA256 hashes
are retained in checks.json; the
[source ledger](2026-11-04-dqm-a-data/sources.json) distinguishes external
reading and reused local derivations. Tail truncation is finite numerical
evidence, not outward-rounded integration or a proof of global rank. The
analytical arguments above establish the classifications.

Required dev suites, with captured logs under `build/scratch/`:

```sh
/tmp/umvl-cpu/run4 just cpp-test
/tmp/umvl-cpu/run4 just test
```

Both finished with exit status zero. C++: **106/106 dev shards passed**,
zero failed; ctest elapsed 64.65 seconds (labels may also include cran/full;
only dev was selected). R: **FAIL 0, WARN 0, SKIP 154, PASS 20612**, elapsed
150.109 seconds for the test runner. All 154 skips explicitly require the
full test tier; they are not mathematical evidence and are not counted as
passes. These are regressions using the suites' existing per-case tolerances,
not coverage or test-calibration studies. No golden fixtures or tolerances
were changed to obtain green results.

The repository-wide link checker returned nonzero for **five pre-existing
broken links**: two missing scratch screenshots in
`2026-10-31-inference-6-report.md`, the absent `boundary-points.pdf` linked
from `2026-11-03-boundary-lit-report.md`, and two `/inference/` website routes
in `univariateML-cpp/notes/inference.md` that the filesystem checker treats
as files. All three source files are byte-identical to HEAD; none belongs
to this task. The authored report's local links pass a separate scoped
check. The global failure is retained as a failure, not counted as a pass.

The artifact-pane `show` command was attempted after completion, but the
viewer returned HTTP 403 because this worktree is outside its configured
readable roots. The report and CSV remain in the requested worktree; no
external copy or viewer configuration change was made.

The final artifact/link audit is recorded in
[validation.json](2026-11-04-dqm-a-data/validation.json). The only open
mathematical entry in this scope is the joint fitted GED Hessian theorem
specified above. Numerical derivatives near cusps/support edges and the
implemented optimizers' asymptotic likelihood accuracy remain procedure
validation gaps, not extra unknown model classifications.
