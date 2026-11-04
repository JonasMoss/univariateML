# Interior regularity classification, part c

TASK-102.13 / dqm-c, 2026-11-04. Source revision
`b92c6189497f00b5917c8da8c29d07cc494f896d`, branch `codex/dqm-c`.
Research only; no library, binding, dossier, production-test or golden changes.

The [classification CSV](2026-11-04-dqm-c-data/classification.csv) contains
all 35 assigned families and all 14 requested columns. All 32 families with
continuous free coefficients are DQM at every conditional interior point,
with finite information, C∞ log pmfs and locally dominated third log
derivatives. None has a DQM failure set in this scope. Information is
singular on the beta-negative-binomial diagonal and on the small-design
strata listed below. The three entirely integer families have no free
continuous coefficient once the requested holds are imposed.

The nonstandard rank and envelope arguments below settle the former open
classifications for bnbinom, dgamma, dnorm, dweibull, pig and skellam.
No mathematical classification in this report is left open. This does not
assert that every numerical fitter is consistent, globally maximizing or
suitable for automatic inference.

## Scope and interpretation of the CSV

- Sampling is i.i.d. under the stated law. Every continuous coefficient is
  free except quantities declared as design or threshold. Every integer
  coefficient is known, including integer coefficients otherwise called
  regular or endpoint. Thus negative-binomial `size` is free and real,
  whereas binomial `size`, discrete-Laplace `location`, finite-Zipf `N`,
  both discrete-uniform endpoints and all hyper/nhyper counts are known.
- The real truncation cutoffs `a,b` of tpois/tbinom are also held. There
  are no other held thresholds in this batch. Known designs range over
  all admissible values, including zero trials, one trial and singleton
  truncations. A design boundary does not remove an interior point of
  its conditional continuous submodel. Empty truncations are not laws.
- Interior means the Euclidean interior in the remaining continuous
  coordinates of the dossier **domain**, not just the fitting domain.
  Every finite inclusive endpoint of a free coordinate is excluded:
  in particular size=0 for NB, shape=0 for PIG, nu=0 for CMP, s=0 for
  finite Zipf and probabilities 0 or 1. Held integer strata have the
  usual discrete interpretation, not derivatives of interpolated integers.
- `infinity` means there is no largest finite k: the log pmf is C∞ at
  every support atom, on the whole conditional interior. `k_ae` is the
  same: under counting measure every positive atom matters. There is
  no moving-kink exception after integer location is held. Support
  dependence on a held design is not support movement in a free direction.
- `vacuous` and `not_applicable` mark dunif, hyper and nhyper. Their
  zero-dimensional conditional models have no continuous Wald problem;
  a 0-by-0 matrix is not being labeled a singular continuous model.
- `cramer=yes` records the requested C³ and local third-derivative
  domination, with lower-derivative integrability also established.
  If “classical Cramer conditions” additionally includes identification
  and invertible information, those extra conditions fail on exactly
  the rank strata recorded in the CSV. Smoothness and domination still
  hold there; the report keeps these logically separate.
- `observed_hessian_ok=yes` means `H_n(theta)/n -> I(theta)` for
  `H_n=-sum_i d² log p_theta(X_i)/dtheta dthetaᵀ` in original README
  coordinates, including convergence to a **singular** matrix on a
  singular stratum. The local derivative envelopes also justify this
  along a consistent sequence of estimates in the same chart. This is
  model information per observation. It is neither the total-score
  outer product nor an optimizer inverse Hessian. No empirical matrix
  inversion, numerical differentiation accuracy or misspecification-robust
  covariance guarantee is inferred from this mathematical statement.

The full model support can depend on the integer/design coordinates of
binom, bbinom, cmb, dunif, hyper, nhyper, tbinom, tpois, zib, zipf,
zmbinom and ztbinom. The CSV's support column explicitly conditions on
the listed holds; no result licenses regular inference for those
coordinates if subsequently estimated.

## Exact information-rank exceptions

All information matrices in the continuous scope are finite. Outside this
table they are positive definite. Ranks refer to all coordinates in the
CSV's `free` column, with no nuisance coefficients silently fixed.

| Family | Held design / interior exception | Rank and identification |
|---|---|---|
| bbinom | size=0; size=1 | Rank 0 of 2 at zero; rank 1 at one, identifying only alpha/(alpha+beta). |
| binom | size=0 | Rank 0 of 1; probability is unidentified. |
| bnbinom | size=beta, with all three positive | Rank 2 of 3; null vector (1,0,-1) in (size,alpha,beta). Globally labels can always be exchanged. |
| cmb | size=0; size=1 | Rank 0 of 2 at zero; rank 1 at one, with dispersion nu absent. |
| tbinom | L=U, L=max(0,floor(a)+1), U=min(size,floor(b)) | Rank 0 of 1; retained law is a point mass. |
| tpois | L=U, L=max(0,floor(a)+1), U=floor(b) | Rank 0 of 1; retained law is a point mass. |
| zib | size=0; size=1 | Rank 0 of 2 at zero; rank 1 at one, identifying only (1-pi)*prob. |
| zipf | N=1 | Rank 0 of 1; exponent absent. |
| zmbinom | size=1 | Rank 1 of 2; only p0 identified. |
| ztbinom | size=1 | Rank 0 of 1; probability absent. |

There are no additional singularities at CMB nu=0 or 1, discrete-Weibull
shape2=1, Skellam mu1=mu2, or at the finite mean/variance thresholds of
yulesimon, zeta or bnbinom. For the heavy-tailed models the score moments,
not moments of X itself, govern information finiteness. The bnbinom
singularity is local as well as a label issue; it must not be dismissed
as just two distant global maximizers.

## One-line family arguments

DQM, C∞, fixed conditional support, Cramer domination and the information
identity hold for every continuous row in this table; exceptions in the
last column concern **rank**, not those properties. The CSV records the
held coordinates and domains in full.

| id | Free coefficients | Argument | Rank exception |
|---|---|---|---|
| bbinom | alpha; beta | Finite positive analytic pmf; first two factorial moments identify both shapes for size >= 2. | size=0: rank 0; size=1: rank 1 (only alpha/(alpha+beta) identified) |
| binom | prob | Full one-parameter exponential family conditional on known size; information size/[prob*(1-prob)]. | size=0: rank 0 (prob unidentified) |
| bnbinom | size; alpha; beta | Polynomial-tail log envelopes give DQM and Cramer; adjacent-mass ratio proves rank except size=beta (proof B). | size=beta: rank 2 of 3; null direction (1,0,-1) in (size,alpha,beta) |
| cmb | prob; nu | Finite exponential family with statistics x and log choose(size,x); minimal exactly when size >= 2. | size=0: rank 0; size=1: rank 1 (nu unidentified) |
| cmp | lambda; nu | Full minimal exponential family in log(lambda) and -nu; factorial tails dominate all derivatives locally for nu > 0. | none |
| dgamma | shape; rate | Fixed binning preserves gamma DQM; conditional moment envelopes and tail score independence prove Cramer and rank (proof C). | none |
| dlaplace | scale | With integer location known this is a minimal exponential family in log(scale), statistic abs(x-location). | none |
| dnorm | mean; sd | Fixed binning preserves normal DQM; conditional polynomial envelopes and quadratic tail coefficients prove rank (proof C). | none |
| dunif | none | All coefficients are known integers; there is no continuous estimation problem. | not_applicable |
| dweibull | shape1; shape2 | Survival shape1**(k**shape2) identifies both tangent directions; stretched exponential envelopes prove Cramer (proof D). | none |
| geom | prob | Minimal geometric exponential family; exponentially decaying tail supplies all local derivative moments. | none |
| hyper | none | All coefficients are known integer population/design counts; no continuous estimation problem. | not_applicable |
| lgser | theta | Minimal exponential family in log(theta), statistic x; exponential tail. | none |
| nbinom | size; prob | Exponential-tail gamma-ratio envelopes; adjacent ratios (x+size)*(1-prob)/(x+1) identify both tangents. | none |
| nhyper | none | All coefficients are known integer population/design counts; no continuous estimation problem. | not_applicable |
| pig | mean; shape | IG-mixture DQM; GIG conditional moment bounds give Cramer; mean and variance derivatives prove rank (proof E). | none |
| pois | lambda | Minimal Poisson exponential family; information 1/lambda. | none |
| skellam | mu1; mu2 | Independent-Poisson difference preserves DQM; conditional count bounds give Cramer; mean/variance identify both tangents (proof F). | none |
| tbinom | prob | Finite conditional exponential family in logit(prob), statistic x; rank one exactly when retained support has >= 2 atoms. | L=U: rank 0; L=max(0,floor(a)+1), U=min(size,floor(b)) |
| tpois | lambda | Finite conditional exponential family in log(lambda), statistic x; rank one exactly when retained support has >= 2 atoms. | L=U: rank 0; L=max(0,floor(a)+1), U=floor(b) |
| yulesimon | rho | Gamma-ratio tail x**(-rho-1) controls logarithmic score envelopes; p(1)=rho/(rho+1) has nonzero derivative. | none |
| zeta | s | Minimal exponential family in -s, statistic log(x); log moments of every finite order exist for s > 1. | none |
| zib | prob; pi | Positive-count ratio identifies prob for size >= 2; total positive mass then identifies pi; finite analytic model. | size=0: rank 0; size=1: rank 1 (only (1-pi)*prob identified) |
| zinb | size; prob; pi | Positive adjacent ratios identify size and prob; positive total mass identifies pi; exponential envelopes. | none |
| zip | lambda; pi | Positive adjacent ratios identify lambda; total positive mass identifies pi; factorial envelopes. | none |
| zipf | s | Finite exponential family in -s, statistic log(x); minimal exactly for known N >= 2. | N=1: rank 0 (s unidentified) |
| zmbinom | prob; p0 | Independent Bernoulli zero indicator plus conditional positive binomial; finite analytic hurdle model. | size=1: rank 1 of 2 (prob unidentified) |
| zmgeom | prob; p0 | Independent Bernoulli zero indicator plus shifted geometric; exponential envelopes. | none |
| zmlogarithmic | prob; p0 | Independent Bernoulli zero indicator plus logarithmic-series exponential family; exponential envelopes. | none |
| zmnbinom | size; prob; p0 | Independent Bernoulli zero indicator plus positive NB; adjacent positive ratios prove rank; exponential envelopes. | none |
| zmpois | lambda; p0 | Independent Bernoulli zero indicator plus positive Poisson exponential family; factorial envelopes. | none |
| ztbinom | prob | Conditional finite binomial exponential family; positive support has >= 2 atoms exactly for size >= 2. | size=1: rank 0 (prob unidentified) |
| ztgeom | prob | Fixed shift of geometric; minimal exponential family and exponential envelopes. | none |
| ztnbinom | size; prob | Positive NB adjacent ratios identify size and prob; conditioning preserves local exponential derivative envelopes. | none |
| ztpois | lambda | Minimal positive Poisson exponential family in log(lambda); factorial envelopes. | none |

## Nonstandard arguments

These are derivations from the dossier pmfs in exact arithmetic, not claims inferred from a numerical grid.

### A. Common envelope and rank criteria

For positive pmfs on a fixed countable support, continuity of the vector
`(∂θ pθ(x))/(2√pθ(x))` in ℓ² implies DQM by integration along line segments.
A summable compact-neighborhood bound on `pθ(x)|sθ(x)|²` supplies that
continuity. The same bounds justify differentiation under the sum and
`Eθ[-∂² log pθ]=Eθ[sθsθᵀ]=Iθ`. Here a Cramer condition means C³ plus an
integrable local supremum of each third log derivative, with the analogous
lower-derivative integrability established separately. Rank is a separate
condition. Thus a singular row can satisfy the stated Cramer smoothness
condition and still prohibit an inverse-information Wald calculation.

A finite positive analytic pmf automatically has these envelopes. A minimal
exponential family has information equal to the nonsingular covariance of
its sufficient statistics, transported by a full-rank parameter Jacobian.
This covers the stated Poisson, geometric, logarithmic, zeta, finite Zipf,
CMP, CMB and conditional one-parameter binomial/Poisson arguments. CMP has
`nu>0` in the Euclidean interior, so on compact neighborhoods its factorial
factor beats every polynomial in x and log(x!). The zeta family has
`sum x^(-s) log(x)^j < infinity` for every fixed j and s>1. Infinite-support
geometric/NB/logarithmic laws and their interior zero modifications have
exponential envelopes, including polynomial and logarithmic derivative
factors. Finite positive truncation probability is bounded away from zero
on a sufficiently small parameter neighborhood.

For rank, a direction with zero information must make the log pmf derivative
zero at every atom. It therefore makes every adjacent log-mass ratio
stationary. Conversely, a pmf invariant in a direction has zero score there.
These implications, plus normalization when needed, are used below.

For hurdle laws, writing q for p0 gives the independent zero-indicator and
conditional-positive likelihood factors. In coordinates (q,eta),
`Iqq=1/[q(1-q)]`, `Iq,eta=0`, `Ieta,eta=(1-q) Ipositive`.
For inflation laws, positive adjacent ratios first determine the base
coordinates, then their total positive probability determines pi. The
map from interior inflation coordinates to (zero probability, base
coordinates) has nonzero derivative in pi, although the feasible hurdle
region is restricted. This proves rank for zip and zinb and for zib with
size at least two. NB positive ratios
`p(x+1)/p(x)=(x+r)(1-p)/(x+1)`, for x>=1, identify both r and p at the tangent
level. At size one, a positive binomial is a point mass; hence ztbinom has
rank zero, zmbinom identifies only p0, and zib identifies only `(1-pi)*p`.
At size zero ordinary binomial, beta-binomial, CMB and zib are point masses.
For beta-binomial n>=2, the first two factorial moments determine
`u=alpha/(alpha+beta)` and concentration alpha+beta with nonsingular
Jacobian. CMB statistics x and log choose(n,x) are independent modulo
constants for n>=2: the latter vanishes at both endpoints but is positive
inside. Conditional binomial/Poisson information is a positive transformed
variance exactly when at least two retained atoms remain.

### B. Beta negative binomial: exact singular set

Write r=size, a=alpha, b=beta and c=a+r+b. Expanding beta functions gives

`p(x)=C(r,a,b) Gamma(x+r) Gamma(x+b)/[Gamma(x+1) Gamma(x+c)]`,

with C symmetric in r and b. Its tail is `C x^(-a-1)(1+O(1/x))`.
Uniform gamma-ratio expansions on compact positive parameter sets, also
differentiated through order three, bound first log derivatives by
`C(1+log(1+x))` and the second/third ones by the same envelope (a loose
bound suffices). Taking a neighborhood with a>=a_min>0 proves all required
square-score and third-derivative integrability without requiring EX or
Var X to exist. Finite-bin terms are analytic and positive.

The adjacent ratio is

`p(x+1)/p(x)=(x+r)(x+b)/[(x+1)(x+c)]`.

For a null score direction v, its log derivative vanishes:

`v_r/(x+r)+v_b/(x+b)-(v_a+v_r+v_b)/(x+c)=0` for every x>=0.

This rational identity has distinct poles at -r,-b,-c if r!=b, because
c exceeds both r and b. Its residues give v=0. If r=b the identity instead
gives `v_r+v_b=0` and v_a=0. Symmetry makes this direction an actual zero
score, so rank is exactly two on r=b and three otherwise. The null vector
in dossier order is (1,0,-1). The labeled model is globally two-to-one off
the diagonal, and fails local injectivity on the diagonal; the latter is
a fold, not an assertion that an entire straight parameter line has the
same pmf. Ordering the labels restores global identification away from
the diagonal but places the diagonal on a constraint boundary.

### C. Fixed unit binning: gamma and normal

For Y from a continuous gamma or normal law, X=floor(Y) is a
parameter-independent statistic. Differentiating the bin integral gives
`s_X=E[s_Y | X]`. The square-root L² differentiability argument survives
this fixed coarsening (conditional expectation is an L² contraction),
so both models are DQM with finite information.

For gamma, derivatives of the bin log probability through order three
are sums of conditional cumulants of Y and log Y and smooth constants.
For bins x>=1, Y is in [x,x+1], so those moments are bounded by a polynomial
in x and log(1+x). The bin x=0 has uniformly finite log moments on compact
positive-shape neighborhoods, since integrals of
`y^(a_min-1)|log y|^j` at zero are finite. Gamma tails give an exponential
summable envelope. For normal, the analogous conditional polynomials in
Y have degree at most six at order three and are bounded by a polynomial
in |x|; Gaussian tails supply the envelope. These arguments justify C∞
pointwise smoothness and the stated Cramer condition directly.

Coarsening need not preserve rank; here it does. Gamma bin probabilities
have, uniformly on compact neighborhoods and with differentiated remainders,

`log p(x)=-rate*x+(shape-1)*log x+C(shape,rate)+O(1/x)` as x→infinity.

A zero score combination forces the rate coefficient to zero from the
x term, and then the shape coefficient to zero from log x. For the normal,
the positive tail is

`log p(x)=-(x-mean)^2/(2*sd^2)-log(x-mean)+C(sd)+O(1/x^2)`.

The subtracted upper-tail bin endpoint contributes only an exponentially
small relative term. A zero directional score first forces the sd
coefficient to zero via x², then the mean coefficient via x. Both
information matrices are positive definite at every finite allowed point.

### D. Discrete Weibull

Write q=shape1 and b=shape2. The survival at an integer k>=1 is
`P(X>=k)=q^(k^b)`. Its derivative at k=1 identifies dq and then k=2 identifies
db, so a zero score direction is impossible. With t=-log q>0, the pmf is

`exp(-t*x^b) * [1-exp(-t*((x+1)^b-x^b))]`.

At x=0 it is 1-q. For x>=1 all parameter derivatives exist. On a compact
interior neighborhood let b>=b_min>0 and t>=t_min>0. The tail is bounded
by `exp(-t_min*x^b_min)`. Log-pmf derivatives through any fixed order
are bounded by a polynomial in x and log(1+x): the only potentially
small difference is Δ=(x+1)^b-x^b, comparable locally to b*x^(b-1),
and the ratios from differentiating `log(1-exp(-t*Δ))` are bounded by
powers/logarithms of x, including when Δ tends to zero for b<1.
The stretched exponential dominates every such polynomial. This proves
DQM, finite positive information and Cramer domination also at b=1;
that geometric member is not a singularity of the two-coordinate model.

### E. Poisson inverse Gaussian

Let m=mean>0, s=shape>0, Y~IG(m,s) and X|Y~Poisson(Y). The IG density is

`[s/(2*pi*y^3)]^(1/2) exp[-s*y/(2*m^2)+s/m-s/(2*y)]`, y>0.

The complete-data scores involve constants, Y and 1/Y. On compact interior
neighborhoods the exponential factors at zero and infinity dominate every
needed moment, proving complete-data DQM; fixed marginalization preserves
DQM and finite information. More specifically,
`Y|X=x` is GIG with power index x-1/2, coefficients
`A=2+s/m^2`, `B=s` and Bessel argument sqrt(A*B). Its conditional jth moment
is `(B/A)^(j/2) K_(x-1/2+j)(sqrt(A*B))/K_(x-1/2)(sqrt(A*B))`.
For each fixed j, the Bessel recurrence
`K_(v+1)=K_(v-1)+2*v*K_v/z`, evenness K_-v=K_v and monotonicity in
nonnegative order bound these positive moments by a polynomial in 1+x,
uniformly on compact A,B neighborhoods. Negative moments of each fixed
order are bounded for sufficiently large x by monotonicity in order;
the finitely many remaining x have finite uniformly bounded integrals.
Thus conditional cumulants of Y,1/Y through order three, which form the
marginal log-pmf derivatives, have polynomial envelopes. X has an
exponential moment because the IG mgf is finite near zero and
`E exp(tX)=E exp[(exp(t)-1)Y]`. This proves Cramer domination, including
the lower derivatives and the information identity. These are bounds,
not a large-count numerical Bessel evaluation assumption.

Finally EX=m and Var X=m+m³/s. Their parameter Jacobian has determinant
`-m³/s²`, which is nonzero. A zero score direction would differentiate
both moments to zero (the just-established envelopes justify this), so
the two-coordinate information is positive definite. The declared s=0
member lies on a boundary and is outside this result.

### F. Skellam

Let U,V be independent Poissons with positive means a,b and X=U-V.
Fixed marginalization supplies DQM and finite information from the complete
Poisson experiment. Given X=x>=0, write U=x+J,V=J. The conditional weights
of J are proportional to `(ab)^j/[j!(j+x)!]`; negative x exchanges U,V.
Their successive ratio is `ab/[(j+1)(j+|x|+1)]`. In particular conditional
factorial moments satisfy
`E[(J)_k | X=x] <= (ab)^k/(|x|+1)_k`, by shifting the summation index and
bounding each extra factorial denominator. All fixed conditional moments
of J are therefore uniformly bounded on compact positive a,b sets.
Complete log-pmf derivatives and the conditional cumulants forming the
marginal first three log derivatives have polynomial bounds in |x|.
Skellam has exponential moments of all finite orders, yielding the required
local envelopes. Its pmf is positive analytic, including x=0 and a=b.
The Jacobian of EX=a-b and Var X=a+b has determinant 2. These moments
therefore prove full information rank everywhere in the positive quadrant.

## Source basis and statistical use

Dossier pmfs and parameter declarations at the recorded source revision are
the definitions used here. The common coarsening statement is also
[Pollard, *A note on insufficiency and the preservation of Fisher information*,
Theorem 7(i), PDF page 6](https://arxiv.org/pdf/1107.3797): DQM is preserved
under a fixed statistic and its score is the conditional mean of the
original score. The theorem explicitly extends to vector parameters.
The [source ledger](2026-11-04-dqm-c-data/sources.json) distinguishes this
read theorem from the family-specific derivations supplied here.

This classification follows the separation of DQM, information, Hessians
and estimator claims in the [canonical inference plan](../../roadmap/2026-10-30-inference-and-dqm.md).
The [prior inventory](2026-10-31-inference-1-report.md) is historical evidence;
its open rank/envelope entries for this batch are resolved above. The
[moving-feature report](2026-11-04-infer-moving-report.md) explains why a
moving second-derivative kink alone would not disqualify a model, but none
of this batch needs such an exception with the specified holds.

At an ordinary full-rank point these are the model regularity ingredients
for Wald inference on smooth contrasts, chi-square LR inference for smooth
nested hypotheses, and the usual regular AIC parameter penalty. Consistency,
an appropriate local MLE, a justified optimization result and the particular
hypothesis remain necessary. All continuous nuisance coefficients are fitted
in this classification; efficient target information uses the full nuisance
adjustment. At singular points ordinary joint inverse-information Wald and
Wilks/AIC arguments do not follow. Exactly redundant finite-design families
can instead be expressed in their identifiable lower-dimensional model
(e.g. Bernoulli); the bnbinom fold requires separate singular-model analysis
and cannot be repaired merely by replacing the AIC dimension with matrix
rank. An off-diagonal bnbinom fit can use a fixed label-order chart locally.
No regular parameter penalty or test is asserted for an estimated integer
design, nor is ordinary LR/AIC made robust by model DQM. Procedure-specific
coverage and test calibration were not measured by this research job.

## Verification and reproducibility

All runs used the local workstation, through the shared CPU slot locks,
under their `nice -n 10` policy. No simbox, package installation, subagent,
board edit, commit or publishing command was used. The source tree started
clean at the recorded revision; only this report and its evidence directory
were added. Generated R assembly and ignored local C++ products were built
by the ordinary recipes. Fixtures and default fits were not changed.

Environment: Linux 7.0.0-38-generic; GCC 15.2.0; R 4.6.1 (2026-06-24);
BH 1.90.0.1. Python, NumPy, SciPy and platform versions are retained in
[checks.json](2026-11-04-dqm-c-data/checks.json). Workloads are deterministic;
there is no random seed. BLAS/OpenMP were pinned to one thread per worker
by the slot wrappers; C++ and R dev jobs reserved all four slots.

```sh
mkdir -p build/tmp build/scratch
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp
export XDG_RUNTIME_DIR=$PWD/build/tmp
/tmp/umvl-cpu/run4 just cpp-test
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-04-dqm-c-data/make_classification.py
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-04-dqm-c-data/check.py
/tmp/umvl-cpu/run4 just test
```

| Check | Final result |
|---|---|
| C++ dev, `just cpp-test` | 106/106 shards passed; 0 failures; ctest elapsed 68.25 s. |
| R dev, `just test` | FAIL 0, WARN 0, SKIP 154, PASS 20612; R runner elapsed 132.519 s. |
| CSV structure | Exact 14 column names/order, exact 35 IDs/order, all cells populated: pass. |
| Numerical illustrations | 22/22 pass; retained probability, score rank and OPG/Hessian agreement checks. |

The first CMake configure could not find system Boost; the recipe's existing
BH-header fallback configured and built successfully. CMake policy and
existing misleading-indentation compiler warnings were emitted. R generation
also emits existing documentation/shell startup messages; the testthat WARN
count above is specifically zero. No failed assertion or permitted test
failure was accepted. All 154 R skips explicitly report `requires full test tier`;
their file locations and reasons remain in
`build/scratch/dqm-c-r-dev.log`. C++ has no skipped dev shards. The successful
C++ log is `build/scratch/dqm-c-cpp-dev.log`; these ignored logs are not the
sole retained evidence: the counts, commands and context are recorded here.

The retained [checker](2026-11-04-dqm-c-data/check.py) uses independent SciPy
pmf formulas, central differences in original coordinates with step 2e-4,
and deterministic finite support windows. It illustrates bnbinom on/off
the diagonal, grouped gamma/normal, discrete Weibull including shape2=1
and shape2<1, PIG, asymmetric/symmetric Skellam, and beta-binomial/CMB/zib
at designs 0,1,2,4. Assertions require retained mass within 3e-7 of one,
eigenvalue rank with cutoff 1e-7 and maximum absolute OPG/expected-negative-
Hessian discrepancy below 3e-5. Complete parameter vectors, support windows,
retained mass, mean score, eigenvalues and discrepancies are in checks.json.
Across these checks the largest discrepancy was 1.10594e-06.
These finite sums and finite differences are numerical illustrations,
without certified tail bounds or roundoff intervals; they are not the proofs
of DQM, rank or Hessian consistency. The proofs above supply those claims.
The [CSV generator](2026-11-04-dqm-c-data/make_classification.py) reads the
canonical coefficient metadata and attaches the reviewed arguments; it is
not an automatic mathematical classifier. Research-only scope did not
trigger the fitting/dpqr/test-infrastructure full-tier requirement. Full R,
no-long-double, JS and site suites were not run, and no claim about those
unrun tiers is made.

## Implementation follow-ups and remaining scope

The requested interior classification is complete. These implementation and
procedure-validation gaps remain separate work; no production changes were
authorized here.

1. `bnbinom/README.md` currently qualifies any positive regular subset without
   the diagonal exception. Its `inference_numeric` needs a singular regime
   when both size and beta are free and equal (alpha may be held or fitted).
   `core/inference_numeric_generated.hpp` currently declares all positive
   coordinates eligible without an explicit diagonal guard. A future change
   should update the authored generator/eligibility source and regenerate,
   with a targeted structural-singularity check rather than relying only
   on finite-sample matrix conditioning. Holding either size or beta removes
   this null direction; equality alone does not disqualify those submodels.
2. `core/inference_numeric_special.hpp` still rejects PIG with “DQM and
   score-tail domination remain open.” Proof E resolves that mathematical
   reason. Enabling numerical inference still requires its own derivative
   and extreme-parameter validation, plus calibration checks for the
   procedures to be advertised; this report did not implement them.
3. The earlier open rank claims for grouped gamma/normal, discrete Weibull,
   Skellam and bnbinom should link to proofs B–F in a later authorized
   dossier/catalog maintenance task. This report is not a live numerical-
   inference availability inventory. No changes to dpqr.hpp, special/,
   truth files, code generation or generated d/p/q/r were needed for the
   mathematical results.
4. Boundary experiments and free integer-coordinate inference are excluded
   explicitly. Model-based coverage, test calibration and misspecification
   sensitivity have no new validation here. In particular the bnbinom
   diagonal's LR distribution and AIC correction are open procedure-specific
   questions, not open interior DQM/rank questions.

The final report-local-link check passed all 8 local destinations;
`2026-11-04-dqm-c-data/validation.json` retains the compact
run record. The artifact-pane `show` command was attempted after completion,
but the viewer returned HTTP 403 because this worktree is outside its
configured readable roots. Files remain in the requested worktree; no viewer
configuration or external copy was made.
