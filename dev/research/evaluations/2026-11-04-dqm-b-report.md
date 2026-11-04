# Pointwise interior regularity: part b

TASK-102.13, 2026-11-04. Source revision
`b92c6189497f00b5917c8da8c29d07cc494f896d`, branch `codex/dqm-b`.

All **33 assigned families** are classified in the
[CSV](2026-11-04-dqm-b-data/classification.csv), with the requested 14 columns.
Twenty-seven are DQM everywhere in the specified conditional interior model.
Among those, `sn` loses information rank at symmetry. `sged` and `tlambda`
have parameter-dependent DQM regions; `pareto`, `power`, `triang` and `unif`
are nowhere jointly DQM with the specified endpoints free.

This is a research classification of the density models. No library, binding,
dossier, golden, test infrastructure, roadmap or board was edited; no commit,
installation, delegation or simbox run was made. The only new files are this
report and its evidence directory. Production inference availability is a
separate question from the mathematics here.

## Scope and conventions

Every coefficient is free **except** README `design` and `threshold`
coefficients: `nsbeta` holds `min,max`; `pareto2`, `pareto3`, `pareto4` hold
`min`; `tnorm` holds `a,b`. Held finite bounds must be strictly ordered;
`tnorm` also permits fixed infinite bounds. There are **no integer
coefficients** in these 33 families. In particular, Pareto `b`, power `alpha`,
triangular `a,b,c` and uniform `min,max` are free. For triangular distributions
the interior is `a<c<b`; for shifted Gompertz it is `eta>0`.

The parameter domains are the README **density domains**, not `fit_domain`
restrictions. Thus `sged` includes every `nu>0`, `st` every `nu>0`, `lst`
every `df>0`, and `tlambda` every real `lambda`. `std,sstd` require `nu>2`.
A restriction-bound fit has different estimator asymptotics even when the
underlying density is DQM there. A fit whose restrictions exclude the true
parameter is not covered by an interior estimator conclusion.

Information means the per-observation score covariance
$I=E_\theta[s_\theta s_\theta^T]$ in the listed free coordinates in a DQM
model. Outside DQM, the CSV separately records **formal** interior
log-density score second moments when useful; these need not be centered,
need not equal $-E\ell''$, and are not a regular Fisher covariance recipe.
Infinite entries are distinguished from finite rank deficiency. All finite
information matrices in DQM regions are positive definite except `sn` at
`alpha=0`. There is no exact local identification failure in these models:
the skew-normal singularity is first-order, and moving support can identify
parameters missed by formal scores. Tukey's two uniform representations are
a global duplication, not a continuous local unidentified curve.

`k_every_x` uses the usual **local C^k neighborhood** meaning at the true
parameter, with all listed free coefficients variable. `k_ae` permits a
truth-dependent probability-zero set of observations. Every row has
`k_ae=infinity`: at a fixed observation away from a join or endpoint, a
sufficiently small parameter neighborhood stays on the same analytic branch.
Closed zero-density endpoints have log density minus infinity, so no
real-valued C^k classification exists there. Moving positive-density
endpoints can make the log density discontinuous. The CSV explicitly records
these cases, rather than silently dropping them from “every x”. They do not
affect the a.e. statements.

“Cramér” here means the question's C3 and locally integrable third-derivative
envelope conditions on a common support in an open parameter neighborhood.
It is not a synonym for DQM or information rank. Thus moving support fails
this classical sufficient condition, even when boundary decay establishes
DQM and a consistent Hessian by another argument. Integrability of a third
derivative at the single true parameter is a weaker property. For `sn` at
symmetry the derivative conditions hold but the separate nonsingularity
condition fails.

An affirmative observed-Hessian entry means
$-n^{-1}\sum_i\ell_i''(\theta_0)\to I(\theta_0)$, and also consistency at a
consistent interior plug-in where expressly stated. It does not approve a
particular finite-difference stencil or BFGS inverse. The plug-in distinction
is material for `sged`.

## Family-by-family classification

In the following table **R** means DQM with finite positive definite
information throughout the specified interior. Unless another value is
shown, smoothness is infinity for every positive-density interior
observation, support is fixed, Cramér holds and the observed Hessian is
consistent at truth and a consistent interior plug-in. The CSV supplies
all exceptions, endpoint values and held coefficients explicitly.

| Family | Free coefficients; held quantities | Classification | One-line argument |
|---|---|---|---|
| kumar | a, b | R | Endpoint power/log envelopes; log(x) and log(1-x^a) score asymptotics establish rank. |
| laplace | mu, sigma | R; k=0 at join; Cramér no; joint Hessian no | The square root density has an L2 translation derivative; location/scale scores have positive diagonal covariance; the pointwise Hessian omits the cusp mass. |
| lgamma | shapelog, ratelog | R | Fixed log(x) transform of the full two-parameter gamma exponential family. |
| llogis | shape, scale | R | Fixed log(x) transform gives a logistic location-scale family with location log(scale) and scale 1/shape. |
| lnorm | meanlog, sdlog | R | Fixed log(x) transform of normal. |
| logis | location, scale | R | Smooth positive location-scale density with exponential tails and independent odd/even scores. |
| logitnorm | mu, sigma | R | Fixed logit(x) transform of normal. |
| lomax | lambda, kappa | R | Use Y=log(1+lambda*X) for integrability; the shape score grows as Y and the rate score is bounded and nonconstant. |
| lst | mu, sigma, df | R | Student tails give logarithmic df score and bounded nuisance scores; parity and tail orders give rank. |
| naka | shape, scale | R | Fixed X^2 transform gives a full gamma family with shape=shape and rate=shape/scale. |
| norm | mean, sd | R | Full normal exponential family. |
| nsbeta | shape1, shape2 (held: min, max) | R | Held affine transform gives a full beta exponential family. |
| paralogis | shape, scale | R | Log(X/scale) has exponential tails; distinct lower/upper tail slopes of the shape score and the bounded scale score give rank. |
| pareto | a, b | Non-DQM everywhere; moving endpoint; Hessian no | Moving b excludes mass of order \|h\| at a positive-density endpoint; fixed-support formal scores do not capture that loss. |
| pareto2 | shape, scale (held: min) | R | Held min and smooth scale=1/lambda transformation of lomax. |
| pareto3 | shape, scale (held: min) | R | Held min and fixed log(X-min) transform of logistic location-scale. |
| pareto4 | shape1, shape2, scale (held: min) | R | Held min gives Burr XII; independent lower/upper tail exponents and scale score give rank, with logarithmic envelopes. |
| power | alpha, beta | Non-DQM everywhere; moving endpoint; Hessian no | Moving alpha excludes mass of order \|h\| at a positive-density endpoint. |
| rayleigh | sigma | R | Fixed X^2 transform gives exponential with rate 1/(2*sigma^2). |
| sged | mean, sd, nu, xi | R iff nu>1/2; cusp k=ceil(nu)-1; Cramér iff nu>3; Hessian regimes below | Smooth chart to raw two-piece exponential-power density; L2 cusp derivative iff nu>1/2; tail powers/logs and side coefficients prove rank. |
| sgomp | b, eta | R | Exponential tail envelopes; rate score grows linearly while the eta score tends to 1/(1+eta), giving rank. |
| slash | mu, sigma | R | The removable origin in integral_0^1 u*phi(u*z)du is analytic; polynomial tails have bounded scale and decaying location scores. |
| sn | xi, omega, alpha | DQM everywhere; finite I; rank 2 at alpha=0; Hessian still estimates I | Gaussian/Mills-ratio envelopes give DQM; alpha=0 makes shape and location scores proportional; unequal tail curvatures give rank elsewhere. |
| snorm | mean, sd, xi | R; local k=1 at join; Cramér no; Hessian yes | Raw two-piece normal has continuous first derivatives at the join; bounded piecewise Hessian envelopes and side-polynomial scores give DQM, rank and Hessian consistency. |
| sstd | mean, sd, nu, xi | R; local k=1 at join; Cramér no; Hessian yes | Raw two-piece t has continuous first derivatives at the join; bounded/logarithmic tail envelopes, side scales and df tail exponent give full rank. |
| st | xi, omega, alpha, nu | R | Student times positive bounded-argument t CDF; logarithmic df envelopes; tail exponent, tail ratio, common amplitude and next tail coefficient identify all score directions. |
| std | mean, sd, nu | R | Smooth invertible chart of lst: sigma_raw=sd*sqrt((nu-2)/nu), df=nu. |
| tlambda | mu, sigma, lambda | R iff lambda<1/2; support moves for positive lambda; formal rank exceptions below; Cramér iff lambda<0; regular Hessian iff lambda<1/2 | Quantile implicit differentiation and endpoint power alpha=1/lambda for 0<lambda<1; positive edge density for lambda>=1; see report for score-rank exceptions. |
| tnorm | mean, sd (held: a, b) | R | Held a<b leaves a full exponential family with sufficient statistics X and X^2 and finite normal-tail moments. |
| trgamma | shape1, shape2, scale | R | Y=(X/scale)^shape2 is gamma; scores span 1, log(Y), Y, Y*log(Y), with three independent centered directions. |
| triang | a, b, c | Non-DQM everywhere; infinite endpoint score moments; k=0 at mode; Hessian no | Linear endpoint vanishing yields divergent endpoint score squares; the mode join also omits a distributional term from the ordinary Hessian. |
| unif | min, max | Non-DQM everywhere; formal rank 1; Hessian no | Moving positive-density endpoints give order-\|h\| support loss; the formal score is a constant vector of rank one. |
| weibull | shape, scale | R | Y=(X/scale)^shape is exponential; score envelopes are powers/logs of Y and the two centered scores are independent. |

## Arguments and exceptional points

### Smooth fixed-support models

For the normal, gamma and beta transformations in the table, a fixed
one-to-one data transformation preserves Hellinger distance and DQM; its
Jacobian is parameter independent. The parameter charts used are invertible:
Nakagami uses $X^2\sim\mathrm{Gamma}(m,\mathrm{rate}=m/\mathrm{scale})$;
Rayleigh uses $X^2\sim\mathrm{Exp}(1/(2\sigma^2))$; `lgamma` uses gamma on
$\log X$; lognormal and logit-normal use normal on their named transforms.
The covariance of the corresponding nonconstant exponential-family
sufficient statistics is positive definite. This also applies to `tnorm`:
on any nondegenerate held interval, $X$ and $X^2$ cannot have a constant
nonzero linear combination, and the natural map
$(\mu,\sigma)\mapsto(\mu/\sigma^2,-1/(2\sigma^2))$ has full rank.

For the other positive-support models, local power/log envelopes at zero
and infinity establish square-root differentiation and dominated third log
derivatives. Compact parameter neighborhoods have shape exponents bounded
away from zero. Polynomial log moments at an algebraic endpoint are finite;
Weibull/generalized-gamma tails also dominate powers of $X$ and $\log X$.
Logistic and log-logistic scores have exponential-tail envelopes in log data.
Burr/Lomax scores and their differentiated versions are bounded rational
functions plus logarithms, requiring no ordinary mean or variance of $X$.
Student scores have bounded nuisance terms and a logarithmic df term; every
logarithmic moment is finite for any fixed positive df. These envelopes also
give uniform LLNs for the Hessians in the smooth cases.

For completeness, rank in three useful non-exponential-family cases follows
without a numerical eigenvalue search. Kumaraswamy shape variations change
the lower and upper endpoint exponents independently. For held-threshold
Pareto IV (Burr XII), a zero score combination must leave the lower exponent
$p$, upper exponent $kp$ and then the endpoint amplitude unchanged; hence
$p,k,\mathrm{scale}$ have zero variations. For transformed gamma, write
$Y=(X/a)^p$; the shape, log-scale and power scores have distinct coefficients
of $\log Y$, $Y$ and $Y\log Y$, forcing every zero combination to vanish.
Paralogistic's lower and upper log-data slopes are $a$ and $-a^2$;
its shape score is unbounded in those tails while its scale score is bounded
and nonzero. For shifted Gompertz the rate score grows linearly in $X$ and
the shape score tends to the nonzero constant $1/(1+\eta)$.

The apparent slash singularity at its center is removable:
$f(z)=\int_0^1 u\phi(uz)\,du$, positive and analytic at zero. In its tails
$f(z)\sim\phi(0)/z^2$; location derivatives decay and scale derivatives
remain bounded. Odd location and nonzero even scale scores establish rank.
The symbolic difference-of-exponentials expression should not be mistaken
for a density cusp.

### Moving endpoints and Laplace

For `pareto`, `power` and `unif`, an endpoint displacement removes probability
of order $|h|$, contradicting the $o(h^2)$ support-loss requirement of DQM.
Their formal score second moments are finite: positive definite for Pareto
and power, but rank one for two-endpoint uniform. This distinction explains
why a finite score matrix alone is insufficient. In uniform, the formal
negative expected Hessian is the **negative** of its formal score second
moment. Pareto's endpoint score has nonzero mean. None provides the regular
joint information identity.

Triangular endpoint density vanishes only linearly. The endpoint score has
size proportional to inverse distance, giving a logarithmically divergent
second moment. With endpoints held, the mode alone would be DQM with
$I_{cc}=1/((c-a)(b-c))$, yet $-E\ell_{cc}=-I_{cc}$: the omitted moving-mode
term matters. This conditional illustration does not change the all-free
row.

Laplace is DQM because the square root density has an L2 translation
derivative. Its location/scale information is
$\operatorname{diag}(1/\sigma^2,1/\sigma^2)$. At the moving center its log
density is only C0; off it, the ordinary location Hessian is zero. The
missing distributional derivative at the center supplies the information.
Known-location scale inference is a different regular submodel with a valid
ordinary scale Hessian. These results reuse the
[canonical inference calculations](../../roadmap/2026-10-30-inference-and-dqm.md).

### Two-piece models: snorm, sstd and sged

Use the raw chart $(m,a,\xi,\nu)$, with $a,\xi>0$, and density

$$
 f(x)=\frac{2}{a(\xi+\xi^{-1})}
 g_\nu\left(\frac{x-m}{a\xi^{\operatorname{sign}(x-m)}}\right).
$$

For the power base, take
$g_\nu(z)=\nu\exp(-|z|^\nu)/(2\Gamma(1/\nu))$; normal and Student bases
have the analogous smooth symmetric densities. The README mean/sd charts
are smooth invertible changes of this chart on their stated domains. Their
shift is the mean of the raw law and their scale is its positive standard
deviation, so this change neither fixes the moving mode nor loses a free
coordinate.

For normal and Student bases, the first parameter derivatives match at the
join because $g_\nu'(0)=0$. The second derivatives can jump when $\xi\ne1$,
but are bounded near the join. Tail envelopes are polynomial under a Gaussian
base and bounded/logarithmic under a Student base. Piecewise differentiation,
with no jump in the first derivatives, therefore gives DQM and
$-E\ell''=E ss^T$. Splitting off a shrinking join interval gives the local
L1 continuity needed for the fitted piecewise-Hessian LLN. Thus **snorm and
sstd have valid Hessians throughout their finite interiors**. This extends
the [moving-feature report](2026-11-04-infer-moving-report.md)'s snorm
argument to the Student base; a second-derivative join does not itself
invalidate observed information.

Rank is also direct in this chart. On either side, the scale and skew scores
have independent constant and tail-shape coefficients. The location score
has a different near-origin power (linear for normal/Student); a Student df
variation adds an uncancellable log-tail term. For a power base, a free
exponent adds $|r|^\nu\log|r|$, distinct from the scale/skew $|r|^\nu$
terms and location $\operatorname{sign}(r)|r|^{\nu-1}$ term. At $\nu=1$,
tail coefficients first force scale/skew variations to vanish, leaving the
nonzero two-sided location score. Consequently there is no skew-normal-style
rank loss at two-piece symmetry.

For `snorm,sstd`, the local C^k neighborhood at the join is C1, including
$\xi=1$ when skewness remains free. At exactly symmetric parameters a
second-order Taylor derivative at the join exists; this is stronger
pointwise differentiability, **not a C2 neighborhood**: arbitrarily nearby
asymmetric members have the second-derivative jump. Holding $\xi=1$ would
give a smooth symmetric submodel, which is outside this job's all-free scope.

For `sged`, near its moving mode the location score has size
$|r|^{\nu-1}$, so its square is integrable exactly for **$\nu>1/2$**.
The cusp Hellinger calculation in the moving-feature report applies on each
side after rescaling: below the threshold the increment has order
$|h|^{1+2\nu}$, and at the threshold $h^2\log(1/|h|)$.
Together with the smooth tail/shape envelopes this proves DQM and positive
definite information precisely above $1/2$, not just above the fitter's
free-shape restriction of 1.

The local smoothness at the cusp with exponent/skew free is
$k=\lceil\nu\rceil-1$. At a symmetric even-integer exponent an extra
pointwise Taylor derivative can exist, as for the normal case, but every
neighborhood contains lower exponents and asymmetric members and has the
stated C^k limit. Cramér holds exactly for $\nu>3$.

In raw location coordinates the ordinary negative Hessian is proportional to
$\nu(\nu-1)|r|^{\nu-2}$ on each side. Therefore:

- $\nu>2$: an integrable local Hessian envelope gives consistency at truth
  and a consistent interior plug-in, even where C3 fails.
- $1<\nu\le2$: the ordinary Hessian is integrable at truth and its average
  estimates information there. Consistency at the **jointly fitted** mode,
  exponent and skewness is open below.
- $\nu=1$: ordinary location curvature misses a nonzero join point mass.
- $0<\nu<1$: ordinary location curvature is nonintegrable and has the wrong
  sign, including the DQM interval $1/2<\nu<1$.

### Azzalini sn versus st

For `sn` at $\alpha=0$, the scores for $\xi$ and $\alpha$ are respectively
$z/\omega$ and $\sqrt{2/\pi}\,z$. The scale score is an independent quadratic,
so the finite information has **rank two of three**. DQM and C3 domination
still hold: Gaussian/Mills-ratio bounds dominate the derivatives on compact
parameter neighborhoods. The Hessian consistently estimates the singular
matrix; inverting it does not produce valid ordinary symmetry inference.
For $\alpha\ne0$, the Gaussian-side tail separates location and scale, after
which the nonzero shape score cannot disappear. This direct calculation
agrees with the rank-two symmetry statement in
[Hallin and Ley](https://arxiv.org/html/1209.4177); no broader theorem from
that source is needed here.

For `st`, let $z=(x-\xi)/\omega$ and write

$$
 f(x)=\frac2\omega t_\nu(z)
 T_{\nu+1}\left(\alpha z\sqrt{\frac{\nu+1}{\nu+z^2}}\right).
$$

At every finite $\nu>0,\alpha$, the CDF argument is bounded in $z$ and its
CDF is bounded away from zero locally in parameters. Its differentiated
terms are bounded; the Student df derivatives grow at most as powers of
$\log(1+|z|)$. This gives C-infinity, dominated third derivatives, DQM and
finite information even when ordinary moments of $X$ do not exist.

To prove full four-coordinate rank, suppose a score combination vanishes.
Both tails have exponent $-(\nu+1)$, so the coefficient of $\log|x|$ forces
$\delta\nu=0$. Their leading-constant ratio is

$$
 \frac{T_{\nu+1}(\alpha\sqrt{\nu+1})}
 {T_{\nu+1}(-\alpha\sqrt{\nu+1})},
$$

strictly increasing in $\alpha$, forcing $\delta\alpha=0$. The sum of the
leading constants is a positive constant times $\omega^\nu$, forcing
$\delta\omega=0$. Finally the next relative tail coefficient is
$(\nu+1)\xi/x$, forcing $\delta\xi=0$. Thus information is nonsingular
**also at symmetry**, for every finite positive df. In particular its odd
symmetry scores are proportional to $z/(\nu+z^2)$ and
$z/\sqrt{\nu+z^2}$, which are independent. The singular normal limit as
$\nu\to\infty$ is outside this finite interior classification.

### Tukey lambda on its entire README domain

The [moving-feature report](2026-11-04-infer-moving-report.md) proves DQM,
full rank and the regular Hessian result for $\lambda<1/2$. Reuse its
quantile and endpoint argument: for $0<\lambda<1$ the endpoint density is
proportional to $d^{1/\lambda-1}$. The endpoint square-root derivative is
integrable iff $1/\lambda>2$; the equality case diverges logarithmically.
For $\lambda\ge1$ the endpoint density is positive, including both uniform
members, so joint DQM fails there as well. Negative and zero shape have
full real support and logarithmic quantile-coordinate score envelopes.

The full positive-shape extension needs an additional distinction. With
$u=F(x)$, define

$$
 q_\lambda(u)=\frac{u^\lambda-(1-u)^\lambda}{\lambda},\quad
 A=u^{\lambda-1}+(1-u)^{\lambda-1},\quad B=A',
$$

using the continuous extension at zero. In coordinates $(\mu,\log\sigma,\lambda)$
at $(\mu,\sigma)=(0,1)$, formal scores are

$$
 s_\mu=B/A^2,\qquad
 s_{\log\sigma}=-1+Bq/A^2,\qquad
 s_\lambda=-A_\lambda/A+Bq_\lambda/A^2.
$$

Here the subscript on the last $q$ means differentiation with respect to
shape. For $0<\lambda<1$, $s_\mu$ has leading order $u^{-\lambda}$;
for $1<\lambda<2$ it has leading order $u^{\lambda-2}$. At $\lambda=1,2$,
$B$ vanishes identically. These expansions and the mirrored upper endpoint
prove the exact formal-score classification:

| Shape | Formal score second moment, separate from DQM |
|---|---|
| $\lambda<1/2$ | Finite, positive definite, genuine regular information |
| $1/2\le\lambda<1$ | Infinite endpoint-direction entries |
| $\lambda=1$ | Finite rank 2; location score zero |
| $1<\lambda\le3/2$ | Infinite endpoint-direction entries |
| $\lambda>3/2$, $\lambda\ne2$ | Finite positive definite; **non-DQM** |
| $\lambda=2$ | Finite rank 2; location score zero |

For rank, a zero formal score combination means
$\partial_u(v/A)=0$, with $v=\delta\mu+\delta\sigma q+\delta\lambda q_\lambda$.
Parity first forces the odd combination of $q,q_\lambda$ to vanish. They
are independent: the shape derivative introduces a nonzero
$u^\lambda\log u$ endpoint term that a multiple of $q$ cannot contain
(the limiting zero-shape case has logarithmic powers instead).
Thus only $\delta\mu=cA$ remains. $A$ is constant exactly at 1 and 2,
yielding precisely the two stated rank losses. At shape 1 the scale score
is $-1$ and the shape score is $-\tfrac12\log\{u(1-u)\}$, explicitly
showing the missing centering. Shape 2 likewise has a constant scale score
and a nonconstant entropy-shaped score. Uniform laws at shape 1 and 2 have
the same law after doubling the second scale; this global duplication does
not repair either moving-support experiment.

At every strictly interior observation the implicit quantile inverse is
smooth, including at zero shape. At finite endpoints the log density is
minus infinity for $0<\lambda<1$, and cannot be a continuous finite-valued
parameter map for $\lambda\ge1$ as the endpoint crosses the observation.
Classical common-support Cramér conditions hold for $\lambda<0$ and fail
for $\lambda\ge0$: at zero, every parameter neighborhood includes bounded
supports. Although truth-level third derivatives in the regular positive
shape region have finite expectations for $\lambda<1/3$, that fact alone
is not a common-support local third-derivative envelope. The separate
Hessian result still holds throughout $\lambda<1/2$, as proved in the
moving-feature report; finite Hessian variance is not required for its LLN.

## Open result and practical interpretation

**Open: jointly fitted skew-GED Hessian for $1<\nu\le2$.** What is missing
is a theorem controlling empirical inverse residual powers at the exact
joint mode/scale/exponent/skew likelihood solution, strong enough to prove
$H(\hat\theta)/n\to I(\theta_0)$. The true-parameter identity and the
held-exponent symmetric location argument in the moving-feature report do
not supply that theorem. The latter conditions on the other observations
while solving a location equation with fixed exponent; estimating exponent
and skewness makes these coefficients depend on the same observation.
A uniform envelope over all nearby centers is also unavailable because it
contains singular Hessians at observation knots. An extension needs an
anti-concentration bound for these joint fitted residuals or an equivalent
truncated-Hessian argument. The independent integrals below validate the
truth-level identity only. No DQM, information-rank or smoothness entry is
left open.

For statistical practice these classifications concern the correctly
specified i.i.d. density and unrestricted **local** parameter hypotheses,
with every listed nuisance parameter fitted. DQM plus positive information
gives the regular local experiment. Applying Wald, chi-square LR or the
usual parameter-count AIC to a returned fit additionally needs a consistent,
adequately optimized interior estimator and the corresponding likelihood
expansion and moment conditions. Global likelihoods can be unbounded despite
local regularity; the fitters' restrictions and escape checks are separate.
At singular symmetry, a moving endpoint, or an active fitter restriction,
ordinary full-parameter calibration is not established by the table.
Laplace and the regular two-piece models show why lack of classical C3 alone
is insufficient to reject regular inference. No misspecification-robust
inference or bootstrap validity is asserted. No change to settled d/p/q/r
or derivative code is needed to express this research classification.

## Reproducible validation and final runs

All builds, tests and numerical work ran locally through the shared CPU slot
locks under nice 10; no simbox was used. The environment was Linux x86_64,
GCC 15.2.0, R 4.6.1, BH 1.90.0.1, Python 3.14.5, NumPy 2.4.6 and SciPy
1.18.0; the numerical JSON records the exact Python/platform strings.
Temporary paths were set to `build/tmp` using `TMPDIR`, `JUST_TEMPDIR` and
`XDG_RUNTIME_DIR`. BLAS/OpenMP/MKL had one thread; full dev suites reserved
all four slots. Tests used source revision above, with only the research
files added. Fixtures and goldens were not regenerated.

| Command after exporting the temporary paths | Machine | Final result |
|---|---|---|
| `/tmp/umvl-cpu/run4 just cpp-test` | workstation | **106/106** dev shards; exit 0; test elapsed 69.05 s |
| `/tmp/umvl-cpu/run4 just test` | workstation | **FAIL 0, WARN 0, SKIP 154, PASS 20612**; exit 0; testthat 129.1 s |
| `/tmp/umvl-cpu/run env OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 MKL_NUM_THREADS=1 nice -n 10 python3 dev/research/evaluations/2026-11-04-dqm-b-data/check_calculations.py` | workstation | **39/39** deterministic integration points pass; exit 0 |
| `/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-04-dqm-b-data/build_classification.py` | workstation | **33/33** exact family/schema and free/held metadata checks pass; exit 0 |

The 154 R skips all require the full tier; there were no permitted failures.
Unchanged C++ code emitted compiler warnings and shell startup printed missing
`cargo/env` notices; neither was a test failure. Full R, no-long-double, JS
and site suites were not run because this job changes no fitting, d/p/q/r or
test infrastructure. Retained [run summaries](2026-11-04-dqm-b-data/final-test-results.txt)
distinguish these omissions from passes.

The independent checker uses raw two-piece normal, power and Student
formulas, and the Azzalini t density, without calling package/library code.
It checks normalization, score centering, positive information eigenvalues
and the applicable **location** Hessian identity. It covers two-piece normal
at three skews, power at five exponents and three skews, two-piece t at
three dfs and three skews, and Azzalini t at four dfs and three slants.
Power exponents include 0.7, 1, 1.2, 2 and 3.5; Azzalini df includes 0.5.
The invalid power-location Hessian is deliberately not integrated at
exponents at most 1. No simulation or seed is involved.

Quadrature absolute/relative tolerances are `2e-8` for two-piece integrals
and `1e-7` for Azzalini t; its df derivative uses a central relative step
`2e-5`. Acceptance tolerances are `2e-6` for normalization and score means,
`3e-6` absolute for the location Hessian identity and `1e-7` as the minimum
positive eigenvalue. Maximum observed mass error was `6.04e-13`, score mean
`2.26e-10`, and Hessian discrepancy `1.52e-9`; the smallest eigenvalue was
`6.88e-6`. These finite numerical checks corroborate the derivations; they
are not DQM proofs, interval certificates, full Hessian derivative tests or
coverage/calibration studies.

Evidence is retained in the [numerical results](2026-11-04-dqm-b-data/calculation-results.json),
[checker](2026-11-04-dqm-b-data/check_calculations.py),
[CSV builder](2026-11-04-dqm-b-data/build_classification.py),
[dossier hashes](2026-11-04-dqm-b-data/dossier-sha256.json) and
[source ledger](2026-11-04-dqm-b-data/sources.json). The ledger explicitly
marks external fetch timeouts; the skew-t and extended Tukey rank arguments
above do not rely on unread papers.
