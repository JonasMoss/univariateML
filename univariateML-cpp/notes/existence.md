# Existence and uniqueness of the MLE

## What can fail

A likelihood can be unbounded, have an unattained finite supremum, or have several maximizers. For an integer design, even a well-behaved likelihood at each fixed size can require an infinite search. The statements below concern mathematical likelihoods; a numerical optimizer's convergence is a separate question. [Section “Classification” of ML behaviour](../../dev/research/ml-behaviour.tex); [open existence inventory](../../dev/research/evaluations/2026-10-31-open-existence-report.md).

## Exponential families

### Proposition (proved): sufficient statistics and convex support

In a minimal, full regular exponential family, under the usual convex-support existence theorem, the finite natural-parameter MLE exists exactly when the empirical sufficient statistic lies in the interior of its convex support. It is then unique. The qualifications matter: a restricted natural-parameter space or an added finite boundary member needs separate analysis.

The log-likelihood is $n(\theta\cdot\bar T-A(\theta))$ plus a constant. Its Hessian is minus $n$ times the covariance of $T$; minimality makes it negative definite. At an interior empirical statistic the mean map reaches that statistic; on an exposed boundary a parameter sequence can instead escape. For positive gamma data, the statistic is $(\overline{\log\, x},\bar x)$ and strict Jensen gives interior exactly for nonconstant data. For beta data in $(0,1)$ it is $(\overline{\log\, x},\overline{\log(1-x)})$; the condition is $\exp(u)+\exp(v)<1$, again exactly nonconstant data. Constant samples have unbounded likelihood in both families. For Poisson counts, a positive sample mean gives the unique positive-rate MLE; mean zero gives the attained rate-zero member if that member is included. [Gamma dossier](../include/cppunivariate/distributions/gamma/README.md); [beta proof](../../dev/research/evaluations/2026-10-31-existence-1-report.md); [Poisson report](../../dev/research/evaluations/2026-10-31-existence-1-report.md).

## Fixed-shape location and scale

### Theorem (proved): log-concavity in inverse scale

For a fixed standardized log-concave density $f$, define $u=\mu/\sigma$, $b=1/\sigma>0$. Then

$$L(u,b)=n\log\, b+\sum_i\log\, f(bx_i-u)$$

is jointly concave. For the exponentially modified Gaussian (EMG) and Bhattacharjee densities at finite fixed dimensionless shape, a nonconstant finite sample has a unique finite maximum; a constant sample has unbounded likelihood. An exact interior zero of the score is therefore a global certificate for that fixed-shape problem.

Each log-density term is concave after an affine transformation, and $n\log\, b$ is concave. The Gaussian convolutions are strictly log-concave; exponential tail bounds make the objective tend to minus infinity along every escaping direction for nonconstant data. For constant data, choosing $u=bx_0-z_0$ and letting $b\to\infty$ makes $n\log\, b$ diverge. Fixed-shape concavity also covers Azzalini skew-normal, GED with $\nu\ge1$, skew-normal, and skew-GED with fixed skewness and $\nu\ge1$; the latter statement alone does not prove existence or uniqueness in every nonsmooth case. In EMG the fixed shape is $\lambda\sigma$, and in Bhattacharjee it is $a/\sigma$: holding the public rate $\lambda$ or width $a$ while varying $\sigma$ is different. The result does not certify the outer shape search or an approximate score. [Proof and scope](../../dev/research/evaluations/2026-10-31-logconcave-report.md), using Prékopa's theorem as cited there.

### Propositions (proved): logistic, Weibull and Cauchy

Nonconstant real data have a unique finite joint logistic MLE; constant data have unbounded likelihood as scale collapses. Its log-density is strictly concave in the standardized coordinate, and the inverse-scale objective is coercive for nonconstant data. Positive nonconstant data likewise have a unique finite Weibull MLE. If $z_i=\log\, x_i$, its profiled shape score is $1/k+\bar z-\sum_i z_i e^{kz_i}/\sum_i e^{kz_i}$; its derivative is $-1/k^2$ minus a weighted variance, and its endpoint signs differ. At a constant $c$, scale $c$ and $k\to\infty$ make the density $k/(ec)$ diverge. These derivations and references appear in the [existence report](../../dev/research/evaluations/2026-10-31-existence-3-report.md) and the [Weibull](../include/cppunivariate/distributions/weibull/README.md) and [logistic](../include/cppunivariate/distributions/logis/README.md) dossiers.

For Cauchy, if the largest tie has multiplicity $k<n/2$, a finite joint maximum exists; if $k>n/2$, likelihood is unbounded. At $k=n/2$ neither conclusion follows automatically. Along a collapse at the tied value, likelihood is proportional to $\sigma^{n-2k}$. For the two observations 0 and 1 there is a continuum of maxima with $0<\mu<1$ and $\sigma^2=\mu(1-\mu)$. The [hold analysis](../../dev/research/evaluations/2026-10-29-p0-locscale.md) and [errata](../../dev/research/evaluations/2026-10-28-ml-behaviour-errata.md) give the equality example; do not interpret an equality rejection as a proof of nonexistence.

## Beta-binomial dispersion

### Proposition (proved); conjecture for larger sizes

Fix integer size $N\ge2$, free positive shapes, and assume at least one count is strictly between 0 and $N$. Let $p_0=\bar x/N$ and $S^2=n^{-1}\sum_i(x_i-\bar x)^2$. If $S^2>Np_0(1-p_0)$, a finite beta-binomial maximum exists. For $N=2$ this strict overdispersion condition is also necessary, including the equality case. For $N\ge3$ necessity is a **conjecture**, not a theorem.

At the binomial face the derivative in inverse concentration has the sign of $S^2-Np_0(1-p_0)$, so strict excess produces a finite point better than the face; compactness supplies attainment. A nonpositive derivative alone cannot rule out a later rise. For $N=2$, concavity over the feasible probability vectors proves that neither underdispersion nor equality can beat or tie the binomial face. See the [beta-binomial existence report](../../dev/research/evaluations/2026-10-31-bbinom-existence-report.md), including its numerical search and Smith (1983) and Griffiths (1973) background citations.

## Integer-size certificates

### Proposition (proved bound; numerical implementation)

For a truncated binomial conditioned on finite integer window $[L,U]$, let $c_N(k)=\sum_{j=0}^{k-1}\log(1-j/N)$ for $N\ge U$. Its fixed-size profile is at most the truncated-Poisson face profile plus $\sum_i c_N(x_i)-n c_N(U)$. This correction is nonnegative, decreases with $N$, and tends to zero. Consequently, if a finite candidate beats the face, there is a cutoff $N^*$ beyond which the bound lies below that candidate. One must still examine all feasible smaller sizes, including sizes below $U$ where the conditioning support changes.

The bound follows by comparing each binomial kernel with its Poisson kernel and bounding the normalizer using $c_N(U)$. The hybrid implementation enumerates the remaining sizes only when its work budget permits; completed passes are **numerical certificates** with a tolerance, not interval-arithmetic proofs. Fallbacks and the Poisson-limit decision remain numerical; a budget is not a tail proof. Infinite windows and the zero-truncated and zero-inflated binomial searches need other certificates. [Size-search report](../../dev/research/evaluations/2026-10-31-size-search-report.md).

## Lomax theorem

### Theorem (proved, Jonas Moss)

For strictly positive observations, put $y_i=x_i/\bar x$, $\bar u=\bar x/(\prod_i x_i)^{1/n}$ and

$$\psi(u)=\frac1n\sum_i\log(e^{-u}+uy_i).$$

A finite Lomax MLE exists exactly when $\psi(u)\leq 0$ for some $u>0$; moreover $\psi(u)>0$ for $u\geq\bar u$. Thus the question reduces to a bounded interval. A sample coefficient of variation greater than one is sufficient; a value below one alone does not rule out an interior maximum. Equality matters, and the theorem does not assert uniqueness. See [Section “Lomax and inverse Pareto: when the MLE exists” of ML behaviour](../../dev/research/ml-behaviour.tex) and the [Lomax dossier](../include/cppunivariate/distributions/lomax/README.md).

### Proof sketch

Profiling the shape gives $\ell_p-\ell_E=n\{\log\, q-\log\, A(q)-A(q)\}$, with $q=\lambda\bar x$ and $A(q)=n^{-1}\sum_i\log(1+qy_i)$. The profile approaches the fitted exponential likelihood as $q\downarrow0$ and tends to $-\infty$ as $q\to\infty$. Writing $q=ue^u$, monotonicity of $ze^z$ makes the sign of $\ell_p-\ell_E$ opposite to that of $\psi(u)$. Finally $\psi(u)>\log(u/\bar u)$ bounds the search.

### Numerical limitation

The current bounded profile scan refines candidate maxima but is not a global certificate; its tolerance sends sufficiently small improvements and attained ties to the exponential face. The exact criterion is not a guarantee that every numerical invocation classifies the sample correctly. [Lomax dossier](../include/cppunivariate/distributions/lomax/README.md).
