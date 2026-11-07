# Findings worth showing

A running list of results from this project that could anchor a statistics
paper: theorems, counterexamples and phenomena, each with its source and two
statuses. **Status** says how the claim is established: proved (an argument in
the linked source), derived (a likelihood-limit argument), certified (an
exact-arithmetic certificate on a concrete sample, conditional on computed
comparisons) or numerical. **Literature** says what we know about prior work:
known (with a reference), partly known, or not checked. "Not checked" means
nobody has searched; it is not a novelty claim.

## 1. The framing: a certified fit or a correct stop

Every fitter either returns a maximum likelihood estimate it has certified as
the global maximum, or stops with a typed reason: the supremum is attained only
in a limit family (with that family's fit), a comparison is unresolved (with
the competing witnesses and likelihood bounds), or a work budget ran out. It
never returns an unchecked optimizer iterate. Limit families come from the
Hellinger closure of each family ([ml-behaviour.tex](ml-behaviour.tex)).

Coverage on 2026-11-07: 67 of 99 families certified for every held subset,
29 partially, 3 not (sstd, st, tlambda). Several of the
certificates needed new existence or uniqueness arguments (section 2), and
several found that uniqueness fails (section 3).

Status: implemented and validated. Literature: verified global optimization
and interval methods are established fields; a per-family certified-MLE library
with typed non-existence stops is not checked.

## 2. Existence and uniqueness results

| Result | Status | Literature | Source |
|---|---|---|---|
| **Poisson-inverse Gaussian: exists iff overdispersed, as for NB.** With $m=\bar x$ and $v=n^{-1}\sum_i(x_i-m)^2$, a finite MLE exists if and only if $v>m$, and it is then unique; otherwise the supremum is the Poisson face. The free mean reduces exactly to $m$; the shape profile is strictly concave in the compact coordinate $w=(\sqrt{1+2\mu^2/\lambda}-1)/(\sqrt{1+2\mu^2/\lambda}+1)$. The key step is a new inequality for half-integer Bessel polynomials $Q_{k-1}(1/z)$: the degree's mean $E_k$ and variance $V_k$ under the positive term weights satisfy $V_k<E_k(k^2-E_k^2)/(k^2+E_k^2)$ for $k\ge2$. With the mean held at $\mu$: exists iff $v+(m-\mu)^2>m$ | proved | not checked (PIG: Sichel; Willmot 1987); the Bessel-polynomial bound not checked | [pig](../../univariateML-cpp/include/cppunivariate/distributions/pig/README.md), [report](evaluations/2026-11-06-pig-cert-report.md) |
| Kumaraswamy, both shapes free: with $t_i=-\log x_i$ and $G(a)=\sum_i-\log(1-e^{-at_i})$, the profile $h(a)=n\log a-n\log G(a)-aT+G(a)+C$ is strictly concave, because $G$ is a log-convex Laplace transform and $G''(a)<n/a^2$; so the MLE is unique for every nonconstant sample in $(0,1)$ | proved | not checked | [kumar](../../univariateML-cpp/include/cppunivariate/distributions/kumar/README.md) |
| Burr XII with scale held: $\ell_p(c)=\text{const}-n\log(G(c)/c)-R(c)$ with $G(c)=\sum_i\log(1+e^{cz_i})$; $G(c)/c$ is log-convex, so the profile is strictly concave and the finite maximum unique | proved | possibly known for the two-parameter Burr XII (Wingo 1983, 1993); check | [burr](../../univariateML-cpp/include/cppunivariate/distributions/burr/README.md), [report](evaluations/2026-11-06-burr-scale-report.md) |
| Zero-truncated NB with probability held: the size profile is strictly concave | proved | not checked (truncated NB ML goes back to Sampford 1955) | [ztnbinom](../../univariateML-cpp/include/cppunivariate/distributions/ztnbinom/README.md) |
| ZINB with inflation free and either size or probability held: unique maximizer for positive data | proved | not checked | [zinb](../../univariateML-cpp/include/cppunivariate/distributions/zinb/README.md) |
| GPD: profiling scale and shape jointly in $b=\xi M/\sigma$ gives mean log likelihood $-\log M-\log D-bD-1$, continuous through $\xi=0$ | proved | not checked | [gpd](../../univariateML-cpp/include/cppunivariate/distributions/gpd/README.md) |

Context: for NB the classical result is that a finite MLE exists if and only
if $s^2>\bar x$ (divisor $n$), and it is then unique (Anscombe 1950; Levin and
Reeds 1977; Aragón, Eberly and Eberly 1992).

## 3. Non-uniqueness and multimodality

| Phenomenon | Status | Literature | Source |
|---|---|---|---|
| **GPD, joint MLE not unique.** For excesses $(a_*,a_*,1)$, with $a_*=\max_{k>-1}a(k)$ from the profile construction, an interior member $(\sigma,\xi)=(e^{-1-k_*},k_*)$ and the uniform member $(1,-1)$ both have total log likelihood exactly zero, and both are global maximizers | proved | partly known: multiple likelihood roots for GPD are discussed (Grimshaw 1993); an exact tie with the uniform endpoint not checked | [gpd](../../univariateML-cpp/include/cppunivariate/distributions/gpd/README.md) |
| **ZINB, size and inflation held: two maxima in probability.** The probability score has at most three roots, isolated through a quadratic. 99 zeros and one count of 10 at size 10 have two maxima whose order swaps as inflation crosses $\pi\approx0.7684$; an exact tie lies in the crossover interval. Mechanism: the zeros come either from the inflation or from a base law concentrated at zero. ZIP with inflation held shows the same | proved (branch count), exact tie by sign change | not checked; the mechanism is the standard zero-inflation ambiguity, but with inflation free ZIP is unimodal (classical) | [zinb](../../univariateML-cpp/include/cppunivariate/distributions/zinb/README.md), [report](evaluations/2026-11-06-zinb-cert-report.md) |
| **Paralogistic, one observation, three stationary points.** Scale held at 1, $x=e^{-0.001}$: local maximum at shape 2.8954 (log likelihood $-0.5701$), minimum at 1140.9, global maximum at 9262.78 (log likelihood $8.1267$). A single observation gives a bimodal likelihood in the shape | certified; 70-digit check | not checked | [report](evaluations/2026-11-06-paralogis-modes-report.md) |
| **Half-t, bimodal $\nu$ profile.** Five observations give local maxima at $\nu=0.415$ and $1.171$; 32 of 1200 scanned samples were multimodal | numerical | not checked | [failure gallery](failure-gallery.tex), section on half-t |
| **Location-scale t, two interior maxima in $\nu$**, and a sample where an interior maximum beats a stable Cauchy-like competitor; both now certified (global $\nu=49.7$ and $33.9$) | certified | partly known (t likelihood pathologies: Fernández and Steel 1999); these examples not checked | [t-close report](evaluations/2026-11-06-t-close-report.md) |
| Cauchy location: multiple roots | classical | known (Reeds 1985) | [failure gallery](failure-gallery.tex) |
| **Beta negative binomial, labeled non-uniqueness.** With $\alpha$ held, the size/$\beta$ maximum is attained; on the 500-count golden sample with $\alpha=2$, a witness at $(1,2,5)$ beats every point of the diagonal $r=\beta$ by at least 0.100 in log likelihood (861 certified segments plus both exterior tails). The pmf is symmetric in $r$ and $\beta$, so swapping them gives a second global maximizer. Each scalar fit (one shape free) is unique, via a single-crossing digamma integral representation of its score | proved; diagonal exclusion certified at 60 digits | not checked | [bnbinom](../../univariateML-cpp/include/cppunivariate/distributions/bnbinom/README.md), [report](evaluations/2026-11-06-bnbinom-cert-report.md) |
| **Skew t with skew held: three scale roots.** Fernández–Steel skew t, mean 0, $\nu=3$, $\xi=10$ held, six seeded points: the scale score has roots near $\log\sigma=3.765$, $4.850$, $5.895$ (maximum, minimum, maximum; log likelihoods $-50.43$ and $-38.81$). At $\xi=1$ the scale score is strictly monotone, so skewing alone creates the extra mode | numerical (multimodality); proved (monotone case) | not checked | [sstd report](evaluations/2026-11-06-sstd-cert-report.md) |

## 4. Boundaries: when the boundary score does not decide

| Phenomenon | Status | Literature | Source |
|---|---|---|---|
| **ZINB versus ZIP.** With $t=-r\log p$ held, $\log f_{\mathrm{NB}}(x)=\log f_{\mathrm{Pois}}(x;t)+r^{-1}[x(x-1)/2-xt/2]+O(r^{-2})$, with no $1/r$ term at $x=0$. The first-order coefficient $C=A-S\lambda/2$, $A=\sum_i x_i(x_i-1)/2$, $S=\sum_i x_i$, is the boundary score, the ZINB analogue of the NB criterion $s^2>\bar x$. $C>0$ proves a finite maximum. $C<0$ does not prove that ZIP wins globally; the certificate also covers every finite rectangle | proved | partly known: the boundary score underlies the ZIP-versus-ZINB score test (Ridout, Hinde and Demétrio 2001); a global existence theory (as Levin and Reeds give for NB) not known to us | [zinb](../../univariateML-cpp/include/cppunivariate/distributions/zinb/README.md) |
| **A proved-finite sample the fitters cannot yet resolve.** 5000 zeros, 3958 ones, 4063 twos, 1979 fours: $C\approx+0.757$, so a finite ZINB maximum beats the ZIP face, at very large size. The historical fitter reported the ZIP limit (false non-existence); the certified fitter stops on budget even at iterlim 20000. The same positive counts make the zero-truncated NB report a false Poisson limit historically | proved (existence); open (location) | not checked | [zinb report](evaluations/2026-11-06-zinb-cert-report.md), [nbfam report](evaluations/2026-11-06-nbfam-cert-report.md) |
| **The likelihood at a limit face is not the limit law's density.** Skew t (Fernández–Steel) with mean and sd held: as a half-width shrinks, the mode is forced toward the half-t endpoint, and each observation at that endpoint carries a density factor $(1+\rho^2)^{-(\nu+1)/2}$ relative to the closed half-t, with $\rho=h_\nu/[2(\nu-2)v]$, $v=1/(\nu-2)-h_\nu^2$; the factor is $0.604$ at $\nu=3$ and $e^{-\rho_N^2/2}\approx0.547$ at the joint normal corner. With $\nu$ free, or with fewer moments held, the full factor is reachable. Azzalini skew t: free location attains the closed half-t endpoint, but location held at the endpoint halves each tied observation's density. Weak convergence alone does not give the reachable likelihood | proved | not checked | [skew-theory](evaluations/2026-11-07-skew-theory-report.md) |
| **Standardized t, zero seam slope but the collapse wins.** Symmetric three-point sample (mass $2/3$ at $b$, $1/6$ at $b\pm d$): the normal-seam slope is exactly zero with positive second coefficient $n/4$, yet the profile decreases strictly in $\nu$ over $(2,\infty)$ and the supremum is the scale collapse at $\nu\downarrow2$, $-n\log(2d)$ | proved | not checked | [std](../../univariateML-cpp/include/cppunivariate/distributions/std/README.md) |

## 5. Interior regularity: a pointwise classification of 99 families

DQM, information rank and smoothness at every interior point, all
coefficients free ([roadmap section](../roadmap/2026-10-30-inference-and-dqm.md#pointwise-interior-classification-2026-11-04)).
About 80 families are regular and smooth. The interesting rows:

| Finding | Status | Literature | Source |
|---|---|---|---|
| **Two skew-normals, opposite behaviour at symmetry.** Azzalini's skew normal has singular information at $\alpha=0$. The Fernández–Steel skew normal is regular at $\xi=1$: in $(\mu,\log\sigma,\log\xi)$ its information is exactly $\operatorname{diag}(1,2,3-8/\pi)$, with skew score $\operatorname{sign}(z)z^2-2mz$, $m=\sqrt{2/\pi}$, orthogonal to location and scale | proved | Azzalini singularity known (Azzalini 1985; Hallin and Ley 2012); the two-piece constant not checked | [infer-moving](evaluations/2026-11-04-infer-moving-report.md) |
| **GED: three different thresholds in $\nu$.** Location is DQM for $\nu>1/2$; the ordinary Hessian has the right expectation only for $\nu>1$; the fitted-point Hessian for $1<\nu\le2$ is open. At $\nu=1/2$ the likelihood information is $1/16$ while $J/(n\log n)\to1/8$: the outer-product estimator converges to twice the right constant | proved | Laplace (non-differentiable, DQM) known; the $1/8$ versus $1/16$ mismatch not checked | [infer-moving](evaluations/2026-11-04-infer-moving-report.md) |
| **Moving kinks need not break regularity.** snorm, sstd and Huber have a moving second-derivative join, yet DQM, full rank and a valid piecewise Hessian hold at every interior point | proved | not checked | [infer-moving](evaluations/2026-11-04-infer-moving-report.md) |
| **Singular information inside the interior.** bnbinom on the diagonal $r=\beta$; genbeta on $\text{shape}_2=1$; Tukey lambda at $\lambda=1,2$ (where it is uniform) | proved | sn known; the others not checked | [dqm reports](evaluations/2026-11-04-dqm-a-report.md) |
| **Endpoint rates belong to endpoint coordinates.** For GEV/GPD with $\xi<-1/2$ and Tukey lambda, the fast rate attaches to the endpoint, not to each named coefficient. All-free GEV at $\xi=-3/4$ has one fast endpoint coordinate and two $\sqrt n$ coordinates, not three at $n^{3/4}$. At the critical exponent the endpoint experiment is Gaussian at rate $\sqrt{n\log n}$ | derived | the $\xi\le-1/2$ nonregularity is known (Smith 1985); the coordinate split not checked | [infer-moving](evaluations/2026-11-04-infer-moving-report.md) |

## 6. Local experiments at closure points

Testing a family against the limit family it escapes to
([infer-closure](evaluations/2026-11-04-infer-closure-report.md)).

| Limit | Local experiment | Null likelihood-ratio limit |
|---|---|---|
| Exponential versus Lomax; Poisson versus NB or unknown-size binomial; binomial versus beta-binomial | Gaussian ray | $\tfrac12\chi^2_0+\tfrac12\chi^2_1$ |
| Weibull versus Burr | Gaussian ray with efficient information $1-6/\pi^2\approx0.392$ | same |
| Poisson versus beta-binomial, size and both shapes free | Gaussian half-plane | $\tfrac12\chi^2_1+\tfrac12\chi^2_2$ |
| Pareto versus Burr, endpoint fitted | Poisson point process at the endpoint, rates $n^{-1}$, plus an independent Gaussian log-tail factor | a point-process supremum, not chi-bar-square |

Status: derived. Literature: chi-bar-square boundary tests are established
(Self and Liang 1987); the half-plane at the beta-binomial Poisson corner and
the point-process Pareto face not checked. The
[boundary-points note](boundary-points.tex) separates support leakage, DQM and
intrinsic dimension, which a proposed regular/singular/degenerate trichotomy
had conflated.

## 7. Software findings

| Finding | Source |
|---|---|
| Historical fitters reported false non-existence (ZIP or Poisson limits) for samples with a proved finite maximum, and `mlstd` could return its start values | sections 3 and 4; TASK-77 in [STATUS](../STATUS.md) |
| Density and CDF defects in extraDistr and actuar, e.g. `dzinb(10000, size = 2, mu = 0.5, pi = 0.1, log = TRUE)` returns `-Inf` instead of about $-6923.75$ | [upstream](upstream/) |
| Ten textbook ML pathologies reproduced against current fitters, with profile figures | [failure gallery](failure-gallery.tex) |

## References

- Anscombe, F. J. (1950). Sampling theory of the negative binomial and logarithmic series distributions. *Biometrika* 37, 358–382.
- Aragón, J., Eberly, D. and Eberly, S. (1992). Existence and uniqueness of the maximum likelihood estimator for the two-parameter negative binomial distribution. *Statistics & Probability Letters* 15, 375–379.
- Azzalini, A. (1985). A class of distributions which includes the normal ones. *Scandinavian Journal of Statistics* 12, 171–178.
- Fernández, C. and Steel, M. F. J. (1999). Multivariate Student-t regression models: pitfalls and inference. *Biometrika* 86, 153–167.
- Grimshaw, S. D. (1993). Computing maximum likelihood estimates for the generalized Pareto distribution. *Technometrics* 35, 185–191.
- Hallin, M. and Ley, C. (2012). Skew-symmetric distributions and Fisher information: a tale of two densities. *Bernoulli* 18, 747–763.
- Levin, B. and Reeds, J. (1977). Compound multinomial likelihood functions are unimodal: proof of a conjecture of I. J. Good. *Annals of Statistics* 5, 79–87.
- Reeds, J. A. (1985). Asymptotic number of roots of Cauchy location likelihood equations. *Annals of Statistics* 13, 775–784.
- Ridout, M., Hinde, J. and Demétrio, C. G. B. (2001). A score test for testing a zero-inflated Poisson regression model against zero-inflated negative binomial alternatives. *Biometrics* 57, 219–223.
- Sampford, M. R. (1955). The truncated negative binomial distribution. *Biometrika* 42, 58–69.
- Self, S. G. and Liang, K.-Y. (1987). Asymptotic properties of maximum likelihood estimators and likelihood ratio tests under nonstandard conditions. *Journal of the American Statistical Association* 82, 605–610.
- Smith, R. L. (1985). Maximum likelihood estimation in a class of nonregular cases. *Biometrika* 72, 67–90.
- Willmot, G. E. (1987). The Poisson-inverse Gaussian distribution as an alternative to the negative binomial. *Scandinavian Actuarial Journal* 1987, 113–127.
- Wingo, D. R. (1983). Maximum likelihood methods for fitting the Burr type XII distribution to life test data. *Biometrical Journal* 25, 77–84.

References are from memory and need checking before use.
