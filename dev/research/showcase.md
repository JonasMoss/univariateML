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

Coverage on 2026-11-08: 79 of 99 families certified for every held subset
and 20 partially; none is uncertified. Several of the
certificates needed new existence or uniqueness arguments (section 2), and
several found that uniqueness fails (section 3).

Status: implemented and validated. Literature: verified global optimization
and interval methods are established fields; a per-family certified-MLE library
with typed non-existence stops is not checked.

## 2. Existence and uniqueness results

| Result | Status | Literature | Source |
|---|---|---|---|
| **Poisson-inverse Gaussian: exists iff overdispersed, as for NB.** For integer samples with $m=\bar x>0$ and $v=n^{-1}\sum_i(x_i-m)^2$, a finite MLE exists if and only if $v>m$, and it is then unique; otherwise the supremum is the unattained Poisson face. (All-zero data attain likelihood one at the zero law, with $\mu$ unidentified.) The free mean reduces exactly to $m$; the shape profile is strictly concave in the compact coordinate $w=(\sqrt{1+2\mu^2/\lambda}-1)/(\sqrt{1+2\mu^2/\lambda}+1)$. The key step is a new inequality for half-integer Bessel polynomials $Q_{k-1}(1/z)$: the degree's mean $E_k$ and variance $V_k$ under the positive term weights satisfy $V_k<E_k(k^2-E_k^2)/(k^2+E_k^2)$ for $k\ge2$. With the mean held at $\mu$: exists iff $v+(m-\mu)^2>m$ | proved | Not located after search: Willmot (1987) remains metadata-only; Carlson (2002) retrieval failed. Neither establishes the profiled concavity, Bessel-weight inequality or held-mean criterion in the inspected evidence; see the 2026-11-08 search report. | [pig](../../univariateML-cpp/include/cppunivariate/distributions/pig/README.md), [report](evaluations/2026-11-06-pig-cert-report.md) |
| Kumaraswamy, both shapes free: with $t_i=-\log x_i$ and $G(a)=\sum_i-\log(1-e^{-at_i})$, the profile $h(a)=n\log a-n\log G(a)-aT+G(a)+C$ is strictly concave, because $G$ is a log-convex Laplace transform and $G''(a)<n/a^2$; so the MLE is unique for every nonconstant sample in $(0,1)$ | proved | Partly known: Jones (2009), likelihood-inference preview, gives the shape profile; strict concavity and the all-nonconstant-sample theorem not located | [kumar](../../univariateML-cpp/include/cppunivariate/distributions/kumar/README.md) |
| Burr XII with scale held: $\ell_p(c)=\text{const}-n\log(G(c)/c)-R(c)$ with $G(c)=\sum_i\log(1+e^{cz_i})$; $G(c)/c$ is log-convex, so the profile is strictly concave and the finite maximum unique | proved | Partly known: Wingo (1983; 1993), abstracts only, explicitly studies conditional modality and existence/uniqueness. Exact theorem overlap open; no novelty claim for uniqueness | [burr](../../univariateML-cpp/include/cppunivariate/distributions/burr/README.md), [report](evaluations/2026-11-06-burr-scale-report.md) |
| Zero-truncated NB with probability held: the size profile is strictly concave | proved | Not located after search: Sampford (1955), §8 primary-scan reading recorded in the earlier ZTNB ledger, concerns the joint problem; the present search did not locate the held-probability strict-concavity theorem. | [ztnbinom](../../univariateML-cpp/include/cppunivariate/distributions/ztnbinom/README.md) |
| ZINB with inflation free and either size or probability held: unique maximizer for positive data | proved | Partly known: Haslett et al. (2022), §3.2 equations (4)–(6), preprint read, gives zero/nonzero likelihood separation; Ridout et al. (1998), §3 read, discusses fitting. Held-size/held-probability uniqueness not located after search. | [zinb](../../univariateML-cpp/include/cppunivariate/distributions/zinb/README.md) |
| **Fréchet with shape held: exact existence criterion.** With location and scale free and $k$ the multiplicity of the sample minimum, a finite MLE exists if and only if $\lambda(n-k)>k$, and it is then unique. At equality the supremum is finite but not attained; below it the likelihood is unbounded. Proof: with scale profiled out, the location-gap score has the sign of $n\lambda R-(\lambda+1)$, where $R=S_{\lambda+1}/(S_\lambda S_1)$ decreases strictly in the gap from $1/k$ to $1/n$, because $\sum q_i^{p+1}/\sum q_i^p$ increases in $p$ | proved | Not located after search: Bücher and Segers (2016), §2.1 Lemma 2.1 and Appendix A proof read, proves uniqueness with location fixed, a different problem. Smith (1985), Cheng and Iles (1987), and Hall and Wang (2005) full theorem comparisons remain open; the minimum-multiplicity threshold and equality case were not located. | [frechet](../../univariateML-cpp/include/cppunivariate/distributions/frechet/README.md), [report](evaluations/2026-11-07-evt-scalar-report.md) |
| GPD: profiling scale and shape jointly in $b=\xi M/\sigma$ gives mean log likelihood $-\log M-\log D-bD-1$, continuous through $\xi=0$ | proved | Known scalar reduction: Grimshaw (1993), §2, equation (2.1) and root theorem read. Ours supplies integral seam evaluation and certification; the scalar reduction is not new | [gpd](../../univariateML-cpp/include/cppunivariate/distributions/gpd/README.md) |

Context: for NB the classical result is that a finite MLE exists if and only
if $s^2>\bar x$ (divisor $n$), and it is then unique (Anscombe 1950; Levin and
Reeds 1977; Aragón, Eberly and Eberly 1992). Wang (1996) reports a major
problem with the 1992 proof; we located a concrete gap in the accessible
1991 preprint (Case 3 cancels boundary values on the wrong slice). The nbinom
dossier now carries a complete self-contained proof, expanding the convexity
argument of Brøns and Jensen (1988, Copenhagen preprint): the negative profile log-likelihood is
strictly convex in $b=r\log(1+m/r)/m$ ([nb-proof](evaluations/2026-11-07-nb-proof-report.md)).

## 3. Non-uniqueness and multimodality

| Phenomenon | Status | Literature | Source |
|---|---|---|---|
| **GPD, joint MLE not unique.** For excesses $(a_*,a_*,1)$, with $a_*=\max_{k>-1}a(k)$ from the profile construction, an interior member $(\sigma,\xi)=(e^{-1-k_*},k_*)$ and the uniform member $(1,-1)$ both have total log likelihood exactly zero, and both are global maximizers | proved | Partly known: Grimshaw (1993), §2 read, treats multiple profile roots; exact global tie with the restricted uniform endpoint not located | [gpd](../../univariateML-cpp/include/cppunivariate/distributions/gpd/README.md) |
| **ZINB, size and inflation held: two maxima in probability.** The probability score has at most three roots, isolated through a quadratic. 99 zeros and one count of 10 at size 10 have two maxima whose order swaps as inflation crosses $\pi\approx0.7684$; an exact tie lies in the crossover interval. Mechanism: the zeros come either from the inflation or from a base law concentrated at zero. ZIP with inflation held shows the same | proved (branch count), exact tie by sign change | Partly known: Ridout et al. (1998), §§1, 3–4 read, discusses structural/sampling zeros, numerical fitting and ZIP/ZINB score testing. The fixed-inflation three-root bound and equal-height crossover were not located after search. | [zinb](../../univariateML-cpp/include/cppunivariate/distributions/zinb/README.md), [report](evaluations/2026-11-06-zinb-cert-report.md) |
| **Paralogistic, one observation, three stationary points.** Scale held at 1, $x=e^{-0.001}$: local maximum at shape 2.8954 (log likelihood $-0.5701$), minimum at 1140.9, global maximum at 9262.78 (log likelihood $8.1267$). A single observation gives a bimodal likelihood in the shape | certified; 70-digit check | Not located after search: Delignette Muller and Dutang, fitdistrplus starting-values article, §2.5.5 read, supplies likelihood and gradients, but no one-observation bimodality result. | [report](evaluations/2026-11-06-paralogis-modes-report.md) |
| **Half-t, bimodal $\nu$ profile.** Five observations give local maxima at $\nu=0.415$ and $1.171$; 32 of 1200 scanned samples were multimodal | numerical | Not located after search: Tsagris, foldedt manual, half-t MLE entries inspected through search extraction, provides numerical fitting; no matching degrees-of-freedom bimodality theorem or sample frequency located. Folding reduces the positive-sample likelihood to the zero-location t likelihood plus a constant. | [failure gallery](failure-gallery.tex), section on half-t |
| **Location-scale t, two interior maxima in $\nu$**, and a sample where an interior maximum beats a stable Cauchy-like competitor; both now certified (global $\nu=49.7$ and $33.9$) | certified | Partly known: Fernández and Steel (1999), abstract only, establishes boundary unboundedness, not these interior profile modes; certified examples may add evidence, priority open | [t-close report](evaluations/2026-11-06-t-close-report.md) |
| Cauchy location: multiple roots | classical | Known: Reeds (1985), author abstract read, explicitly treats multiple Cauchy-location maxima | [failure gallery](failure-gallery.tex) |
| **Beta negative binomial, labeled non-uniqueness.** With $\alpha$ held, the size/$\beta$ maximum is attained; on the 500-count golden sample with $\alpha=2$, a witness at $(1,2,5)$ beats every point of the diagonal $r=\beta$ by at least 0.100 in log likelihood (861 certified segments plus both exterior tails). The pmf is symmetric in $r$ and $\beta$, so swapping them gives a second global maximizer. Each scalar fit (one shape free) is unique, via a single-crossing digamma integral representation of its score | proved; diagonal exclusion certified at 60 digits | Partly known: Ueki (2023), §2.2 read, explicitly states BNB parameter-swap non-identifiability; certified exclusion of the diagonal and scalar uniqueness not located | [bnbinom](../../univariateML-cpp/include/cppunivariate/distributions/bnbinom/README.md), [report](evaluations/2026-11-06-bnbinom-cert-report.md) |
| **Skew t with skew held: three scale roots.** Fernández–Steel skew t, mean 0, $\nu=3$, $\xi=10$ held, six seeded points: the scale score has roots near $\log\sigma=3.765$, $4.850$, $5.895$ (maximum, minimum, maximum; log likelihoods $-50.43$ and $-38.81$). At $\xi=1$ the scale score is strictly monotone, so skewing alone creates the extra mode | numerical (multimodality); proved (monotone case) | Not located after search: Schoups and Vrugt (2010), Appendix A read, supplies the Fernández–Steel standardization construction for SEP, not this held-skew t scale counterexample. Searches did not locate an equivalent three-scale-root example. | [sstd report](evaluations/2026-11-06-sstd-cert-report.md) |
| **SGED, constant sample, two global scale maxima.** Repeated observations $x_i=-1$, held mean 0, $\nu=1/2$: with $R\in(4,6)$ solving $\log R=2(1-1/R)$ ($R\approx4.9216$), $K=2\sqrt{R-1}/R$ and $\xi=\sqrt{1+K^2/6}\approx1.0526$, the cost relative to the cusp is $n[-\log r+K\sqrt{r-1}]$ beyond it, so the cusp $r=1$ and $r=R$ tie as separated global maxima, although no observation equals the held mean | proved (analytic); 70-digit check | Not located after search: Schoups and Vrugt (2010), Appendix A (A1)–(A10), text read, supplies the standardized Fernández–Steel SEP construction. Its stated power range excludes the showcased subunit exponent; no constant-sample separated global scale tie located. | [sged](../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md), [report](evaluations/2026-11-07-sged-subunit-report.md) |
| **Beta prime, one shape held: an exact tie by continuity.** With shape1 held at $0.2269$, a nine-point sample has two separated profile maxima in shape2. Multiplying the last observation by $e^t$ moves the certified global winner across $\text{shape}_2=0.12$ between $t=5.6463$ and $5.6466$, with uniform margins over the gamma face and the shape exteriors; continuity then gives a sample with two separated global maxima | proved, conditional on the endpoint certificates | Not located after search: Tulupyev et al. (2013), §2 equation (5) and Appendix Lemma 1 read, fixes the numerator shape at 2 and discusses critical points, not the arbitrary held-shape global tie here. No equivalent tie located in beta-prime/GB2 searches. | [betapr](../../univariateML-cpp/include/cppunivariate/distributions/betapr/README.md), [report](evaluations/2026-11-07-betapr-prof-report.md) |

## 4. Boundaries: when the boundary score does not decide

| Phenomenon | Status | Literature | Source |
|---|---|---|---|
| **ZINB versus ZIP.** With $t=-r\log p$ held, $\log f_{\mathrm{NB}}(x)=\log f_{\mathrm{Pois}}(x;t)+r^{-1}[x(x-1)/2-xt/2]+O(r^{-2})$, with no $1/r$ term at $x=0$. The first-order coefficient $C=A-S\lambda/2$, $A=\sum_i x_i(x_i-1)/2$, $S=\sum_i x_i$, is the boundary score, the ZINB analogue of the NB criterion $s^2>\bar x$. $C>0$ proves a finite maximum. $C<0$ does not prove that ZIP wins globally; the certificate also covers every finite rectangle | proved | Partly known: Ridout, Hinde and Demétrio (2001), abstract only, gives the ZIP-versus-ZINB score test; exact coordinate expansion and global sufficiency/necessity not verified there | [zinb](../../univariateML-cpp/include/cppunivariate/distributions/zinb/README.md) |
| **A proved-finite sample the fitters cannot yet resolve.** 5000 zeros, 3958 ones, 4063 twos, 1979 fours: a finite ZINB maximum exists and is unique, certified independently at $r=8108.2389$, $p=0.99980$, $\pi=0.16328$, beating the ZIP face by only $4.67\times10^{-5}$ in total log likelihood ($3\times10^{-9}$ per observation). The historical fitter's $10^{-8}n$ face tolerance exceeds the whole gain, hence its false ZIP report; the certified fitter stops on budget because the profiled curvature in $\log r$ is $-9.3\times10^{-5}$ (condition number about $5\times10^8$) | certified (65 digits, padded) | Not located after search: Ridout et al. (1998), §§3–4 read, gives numerical-fitting context and the ZIP/ZINB score-test connection, not this certified near-ZIP sample or tolerance failure. | [zinb-theory](evaluations/2026-11-07-zinb-theory-report.md) |
| **The likelihood at a limit face is not the limit law's density.** Skew t (Fernández–Steel) with mean and sd held: as a half-width shrinks, the mode is forced toward the half-t endpoint, and each observation at that endpoint carries a density factor $(1+\rho^2)^{-(\nu+1)/2}$ relative to the closed half-t, with $\rho=h_\nu/[2(\nu-2)v]$, $v=1/(\nu-2)-h_\nu^2$; the factor is $0.604$ at $\nu=3$ and $e^{-\rho_N^2/2}\approx0.547$ at the joint normal corner. With $\nu$ free, or with fewer moments held, the full factor is reachable. Azzalini skew t: free location attains the closed half-t endpoint, but location held at the endpoint halves each tied observation's density. Weak convergence alone does not give the reachable likelihood | proved | Not located after search: Schoups and Vrugt (2010), Appendix A read, explains mean/variance standardization, not a held-moment endpoint-density penalty. The constrained reachable-likelihood factors were not located. | [skew-theory](evaluations/2026-11-07-skew-theory-report.md) |
| **ZINB, all free: what we can prove.** (i) If $n_0/n\le e^{-\bar x}$ (no excess zeros), a finite MLE exists iff $v>\bar x$ and is unique: the profile coincides with the NB profile. (ii) With the admissible ZIP fit $(\lambda_*,\pi_*)$, $C=A-S\lambda_*/2>0$ is sufficient. (iii) For integer data $C\neq0$ on the positive-inflation face, by Hermite–Lindemann. (iv) If every positive count equals one value $j\ge2$ and $k/n\le1-e^{-\lambda_j}$, no finite MLE exists; so $(0,0,0,4)$, with $v=3>\bar x=1$, has none: the NB variance criterion does not carry over. Open: necessity of $C>0$ and uniqueness in the excess-zero region, reduced to one inequality for the zero-truncated NB profile; LP searches over histograms found no counterexample | proved; open remainder | Sampford (1955, §8) left necessity and uniqueness for the two-parameter truncated NB conjectural (primary pages read); profile separation as in Haslett et al. (2022) | [zinb-theory](evaluations/2026-11-07-zinb-theory-report.md) |
| **Sampford's 1955 conjecture (zero-truncated NB), partial results.** Sampford stated that his endpoint conditions for a finite two-parameter truncated-NB MLE are necessary and the solution unique, as conjectures. (i) At fixed mean, truncated NB laws decrease strictly in convex order as the size grows, and all dominate the truncated Poisson; so if the positive counts are below their fitted truncated Poisson in convex order, the size profile increases strictly and no finite MLE exists. This covers every sample on two adjacent integers. (ii) The stationary-curvature inequality that would prove the conjecture reduces exactly to histograms on $\{1,j,j+1\}$, leaving one scalar inequality (positive in 1,012 evaluations up to $j\approx1.8\times10^6$). Proving it would give the full ZINB criterion: finite MLE iff $C>0$, unique | proved (i), (ii); open remainder | No post-1955 settlement found (Sampford §8, Simon 1961, Wyshak 1974 read; Brass 1958 inaccessible) | [ztnb-conjecture](evaluations/2026-11-07-ztnb-conjecture-report.md) |
| **Standardized t, zero seam slope but the collapse wins.** Symmetric three-point sample (mass $2/3$ at $b$, $1/6$ at $b\pm d$): the normal-seam slope is exactly zero with positive second coefficient $n/4$, yet the profile decreases strictly in $\nu$ over $(2,\infty)$ and the supremum is the scale collapse at $\nu\downarrow2$, $-n\log(2d)$ | proved | Not located after search: general standardized-t and boundary-likelihood searches did not locate the zero-normal-slope three-point collapse theorem. General t pathologies do not establish this exact statement. | [std](../../univariateML-cpp/include/cppunivariate/distributions/std/README.md) |

## 5. Interior regularity: a pointwise classification of 99 families

DQM, information rank and smoothness at every interior point, all
coefficients free ([roadmap section](../roadmap/2026-10-30-inference-and-dqm.md#pointwise-interior-classification-2026-11-04)).
About 80 families are regular and smooth. The interesting rows:

| Finding | Status | Literature | Source |
|---|---|---|---|
| **Two skew-normals, opposite behaviour at symmetry.** Azzalini's skew normal has singular information at $\alpha=0$. The Fernández–Steel skew normal is regular at $\xi=1$: in $(\mu,\log\sigma,\log\xi)$ its information is exactly $\operatorname{diag}(1,2,3-8/\pi)$, with skew score $\operatorname{sign}(z)z^2-2mz$, $m=\sqrt{2/\pi}$, orthogonal to location and scale | proved | Partly known: Azzalini singularity (Hallin and Ley 2012) and nonsingular two-piece information (Rubio and Steel 2014), theorem text read; standardized constant is a coordinate specialization | [infer-moving](evaluations/2026-11-04-infer-moving-report.md) |
| **GED: three different thresholds in $\nu$.** Location is DQM for $\nu>1/2$; the ordinary Hessian has the right expectation only for $\nu>1$; the fitted-point Hessian for $1<\nu\le2$ is open. At $\nu=1/2$ the likelihood information is $1/16$ while $J/(n\log n)\to1/8$: the outer-product estimator converges to twice the right constant | proved | Partly known: Bottazzi and Secchi (2008 working-paper version), Theorems 2.1–2.2 read, gives EP information and the shape >1/2 threshold; critical constants and fitted-Hessian issue not located | [infer-moving](evaluations/2026-11-04-infer-moving-report.md) |
| **Moving kinks need not break regularity.** snorm, sstd and Huber have a moving second-derivative join, yet DQM, full rank and a valid piecewise Hessian hold at every interior point | proved | Partly known: Rubio and Steel (2014), Theorems 1–3 read, gives two-piece information; full DQM and fitted-Hessian scopes remain unverified | [infer-moving](evaluations/2026-11-04-infer-moving-report.md) |
| **Singular information inside the interior.** bnbinom on the diagonal $r=\beta$; genbeta on $\text{shape}_2=1$; Tukey lambda at $\lambda=1,2$ (where it is uniform) | proved | Partly known: BNB swap symmetry is published (Ueki 2023, §2.2 read) and forces diagonal rank loss; exact genbeta and Tukey-lambda singular sets not located. No novelty claim for BNB symmetry | [dqm reports](evaluations/2026-11-04-dqm-a-report.md) |
| **Endpoint rates belong to endpoint coordinates.** For GEV/GPD with $\xi<-1/2$ and Tukey lambda, the fast rate attaches to the endpoint, not to each named coefficient. All-free GEV at $\xi=-3/4$ has one fast endpoint coordinate and two $\sqrt n$ coordinates, not three at $n^{3/4}$. At the critical exponent the endpoint experiment is Gaussian at rate $\sqrt{n\log n}$ | derived | Partly known: Smith (1985), abstract only, includes nuisance parameters and critical Gaussian rates; Bücher and Segers (2017), §1 read, warns of proof gaps. Exact coordinate specializations remain open | [infer-moving](evaluations/2026-11-04-infer-moving-report.md) |

## 6. Local experiments at closure points

Testing a family against the limit family it escapes to
([infer-closure](evaluations/2026-11-04-infer-closure-report.md)).

| Limit | Local experiment | Null likelihood-ratio limit |
|---|---|---|
| Exponential versus Lomax; Poisson versus NB or unknown-size binomial; binomial versus beta-binomial | Gaussian ray | $\tfrac12\chi^2_0+\tfrac12\chi^2_1$ |
| Weibull versus Burr | Gaussian ray with efficient information $1-6/\pi^2\approx0.392$ | same |
| Poisson versus beta-binomial, size and both shapes free | Gaussian half-plane | $\tfrac12\chi^2_1+\tfrac12\chi^2_2$ |
| Pareto versus Burr, endpoint fitted | Poisson point process at the endpoint, rates $n^{-1}$, plus an independent Gaussian log-tail factor | a point-process supremum, not chi-bar-square |

Status: derived. Literature: partly known. Gaussian-ray chi-bar-square laws
follow established boundary theory (Self and Liang 1987; bibliographic metadata,
not theorem text); the individual family assumptions still need checking.
For Weibull versus Burr, the efficient-information constant was not located.
For the beta-binomial Poisson corner, the Gaussian half-plane was not located;
the mixture is standard once that tangent geometry is established. For Pareto
versus Burr with a fitted endpoint, Smith (1985, abstract only) supplies related
nonregular endpoint theory, but this point-process likelihood supremum was not
located. These are candidate specializations, not established novelty claims. The
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

- Anscombe, F. J. (1950). Sampling theory of the negative binomial and logarithmic series distributions. *Biometrika* 37, 358–382. [Source](https://doi.org/10.1093/biomet/37.3-4.358).
- Aragón, J., Eberly, D. and Eberly, S. (1992). Existence and uniqueness of the maximum likelihood estimator for the two-parameter negative binomial distribution. *Statistics & Probability Letters* 15, 375–379. [Source](https://doi.org/10.1016/0167-7152(92)90157-Z).
- Azzalini, A. (1985). A class of distributions which includes the normal ones. *Scandinavian Journal of Statistics* 12, 171–178. [Source](https://www.research.unipd.it/handle/11577/142107?mode=full).
- Fernández, C. and Steel, M. F. J. (1999). Multivariate Student-t regression models: pitfalls and inference. *Biometrika* 86, 153–167. [Source](https://doi.org/10.1093/biomet/86.1.153).
- Grimshaw, S. D. (1993). Computing maximum likelihood estimates for the generalized Pareto distribution. *Technometrics* 35, 185–191. [Source](https://doi.org/10.1080/00401706.1993.10485040).
- Hallin, M. and Ley, C. (2012). Skew-symmetric distributions and Fisher information – a tale of two densities. *Bernoulli* 18, 747–763. [Source](https://doi.org/10.3150/12-BEJ346).
- Haslett, J., Parnell, A. C., Hinde, J. and de Andrade Moral, R. (2022). Modelling excess zeros in count data: a new perspective on modelling approaches. *International Statistical Review*, doi:10.1111/insr.12479.
- Levin, B. and Reeds, J. (1977). Compound multinomial likelihood functions are unimodal: proof of a conjecture of I. J. Good. *Annals of Statistics* 5, 79–87. [Source](https://doi.org/10.1214/aos/1176343741).
- Reeds, J. A. (1985). Asymptotic number of roots of Cauchy location likelihood equations. *Annals of Statistics* 13, 775–784. [Source](https://doi.org/10.1214/aos/1176349554).
- Ridout, M., Hinde, J. and Demétrio, C. G. B. (2001). A score test for testing a zero-inflated Poisson regression model against zero-inflated negative binomial alternatives. *Biometrics* 57, 219–223. [Source](https://doi.org/10.1111/j.0006-341X.2001.00219.x).
- Sampford, M. R. (1955). The truncated negative binomial distribution. *Biometrika* 42, 58–69. [Source](https://doi.org/10.1093/biomet/42.1-2.58).
- Self, S. G. and Liang, K.-Y. (1987). Asymptotic properties of maximum likelihood estimators and likelihood ratio tests under nonstandard conditions. *Journal of the American Statistical Association* 82, 605–610. [Source](https://doi.org/10.1080/01621459.1987.10478472).
- Smith, R. L. (1985). Maximum likelihood estimation in a class of nonregular cases. *Biometrika* 72, 67–90. [Source](https://doi.org/10.1093/biomet/72.1.67).
- Wang, Y. (1996). Estimation problems for the two-parameter negative binomial distribution. *Statistics & Probability Letters*, doi:10.1016/0167-7152(94)00259-2.
- Willmot, G. E. (1987). The Poisson-inverse Gaussian distribution as an alternative to the negative binomial. *Scandinavian Actuarial Journal* 1987, 113–127. [Source](https://doi.org/10.1080/03461238.1987.10413823).
- Wingo, D. R. (1983). Maximum likelihood methods for fitting the Burr type XII distribution to life test data. *Biometrical Journal* 25, 77–84. [Source](https://doi.org/10.1002/bimj.19830250109).

- Bücher, A. and Segers, J. (2017). On the maximum likelihood estimator for the Generalized Extreme-Value distribution. *Extremes* 20, 839–872. [Source](https://doi.org/10.1007/s10687-017-0292-6).
- Bottazzi, G. and Secchi, A. (2008 working-paper version, dated 4 December). Maximum likelihood estimation of the symmetric and asymmetric exponential power distribution. LEM Working Paper 2006/19. [Version read](https://www.lem.santannapisa.it/WPLem/files/2006-19.pdf).
- Jones, M. C. (2009). Kumaraswamy’s distribution: A beta-type distribution with some tractability advantages. *Statistical Methodology* 6, 70–81. [Source](https://doi.org/10.1016/j.stamet.2008.04.001).
- Ridout, M., Demétrio, C. G. B. and Hinde, J. (1998). Models for count data with many zeros. International Biometric Conference, Cape Town. [Conference paper read](https://www.kent.ac.uk/IMS/personal/msr/webfiles/zip/ibc_fin.pdf); proceedings pagination not verified.
- Rubio, F. J. and Steel, M. F. J. (2014). Inference in two-piece location-scale models with Jeffreys priors. *Bayesian Analysis* 9, 1–22. [Source](https://doi.org/10.1214/13-BA849).
- Ueki, M. (2023; online 2022). Beta-negative binomial nonlinear spatio-temporal random effects modeling of COVID-19 case counts in Japan. *Journal of Applied Statistics* 50, 1650–1663. [Source](https://doi.org/10.1080/02664763.2022.2064439).
- Wingo, D. R. (1993). Maximum likelihood estimation of Burr XII distribution parameters under Type II censoring. *Microelectronics Reliability* 33, 1251–1257. [Source](https://doi.org/10.1016/0026-2714(93)90126-J).

Bibliographic fields checked on 2026-11-07 against publisher, author-repository or DOI metadata. Source access and unresolved theorem comparisons are recorded in the [literature report](evaluations/2026-11-07-lit-check-report.md).

References added for the [2026-11-08 bounded search](evaluations/2026-11-08-showcase-lit-report.md) (access limits and sections read are recorded there):

- Bücher, A. and Segers, J. (2016). Maximum likelihood estimation for the Fréchet distribution based on block maxima extracted from a time series. [Version read: arXiv:1511.07613v2](https://arxiv.org/pdf/1511.07613). Cited as the inspected 2016 manuscript.
- Ramos, P. L., Louzada, F., Ramos, E. and Dey, S. (2020; online 2019). The Fréchet distribution: Estimation and application—an overview. *Journal of Statistics and Management Systems* 23, 549–578. [DOI](https://doi.org/10.1080/09720510.2019.1645400); [2018 manuscript read](https://arxiv.org/pdf/1801.05327).
- Cheng, R. C. H. and Iles, T. C. (1987). Corrected maximum likelihood in non-regular problems. *Journal of the Royal Statistical Society, Series B* 49, 95–101. [DOI](https://doi.org/10.1111/j.2517-6161.1987.tb01430.x). Summary/metadata only.
- Hall, P. and Wang, J. (2005). Bayesian likelihood methods for estimating the end point of a distribution. [DOI](https://doi.org/10.1111/j.1467-9868.2005.00523.x); [institutional record](https://hdl.handle.net/1885/85232). Abstract/metadata only; full-text comparison open.
- Schoups, G. and Vrugt, J. A. (2010). A formal likelihood function for parameter and predictive inference of hydrologic models with correlated, heteroscedastic, and non-Gaussian errors. *Water Resources Research* 46, W10531. [DOI and full text](https://doi.org/10.1029/2009WR008933).
- Tulupyev, A., Suvorova, A., Sousa, J. and Zelterman, D. (2013). Beta prime regression with application to risky behavior frequency screening. *Statistics in Medicine* 32, 4044–4056. [DOI](https://doi.org/10.1002/sim.5820); [full text](https://pmc.ncbi.nlm.nih.gov/articles/PMC3789864/).
- Delignette Muller, M. L. and Dutang, C. (undated; accessed 2026-11-08). Starting values used in fitdistrplus, version 1.2-6, §2.5.5. [Author-maintained documentation](https://lbbe-software.github.io/fitdistrplus/articles/starting-values.html).
- Tsagris, M. (undated; accessed 2026-11-08). foldedt reference manual, entries `halft.mle` and `halft1.mle`. [Manual](https://mirror.fem-net.de/cran/web/packages/foldedt/refman/foldedt.html). Search extraction inspected; direct retrieval timed out.
- SciPy developers (accessed 2026-11-08). Beta prime distribution, *SciPy User Guide*. [Moment formula read](https://docs.scipy.org/doc/scipy/tutorial/stats/continuous_betaprime.html).
- NIST (2026). *Digital Library of Mathematical Functions*, §5.15, equation (5.15.8). [Trigamma asymptotic expansion](https://dlmf.nist.gov/5.15.E8).
