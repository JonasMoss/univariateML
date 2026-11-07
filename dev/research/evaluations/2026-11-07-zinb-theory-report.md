# ZINB global existence and uniqueness — TASK-116.14

The near-face sample has a **unique finite global maximum**, certified independently of the fitter:

| Coefficient or comparison | Value |
|---|---:|
| size | 8108.2388581481329544584742965 |
| prob | 0.999803508015713486829597414531 |
| pi | 0.163276396309303523872445174295 |
| total log likelihood | −22998.0004858914916933762597635 |
| maximized ZIP log likelihood | −22998.0005325865669794057284629 |
| finite improvement over ZIP | 0.0000466950752860294686993812113 |

For general samples containing positives, **necessity of the correctly defined condition C > 0, and uniqueness of an attained finite maximum, remain open**. This investigation proves an exact criterion and uniqueness in the zero-deficit region, a non-existence result for constant positive counts with sufficiently many zeros, and a reusable concavity bound that resolves the supplied near-face sample. It reduces the remaining general question to a specific inequality for the truncated-NB profile. Searches did not produce a counterexample to that inequality.

The literature search used 20 queries. A particularly relevant primary-source finding is that Sampford (1955), §8, explicitly leaves necessity and uniqueness for the two-parameter truncated-NB likelihood conjectural. That historical observation does not establish the status of all later literature.

No fitting code, distribution dossiers, tests, goldens, bindings, roadmap, status files or release notes were changed. No commits, Git writes, board edits, installations or sub-agents were used. All evidence is in [the matching data directory](2026-11-07-zinb-theory-data/). The tested source revision is `31e2b66d395dd87068e1e204e934298407ea5955`, branch `codex/zinb-theory`.

## 1. Model, profiling and the two regions

Let n0 be the number of zeros, k = n − n0 > 0, S = Σxi, z = n0/n, q = k/n, m = S/k, and x̄ = S/n. Write v = n⁻¹Σ(xi − x̄)², with divisor n. Define

\[
 A=\sum_{x_i>0}\frac{x_i(x_i-1)}2,
 \qquad B_2=\sum_{x_i>0}\sum_{j=1}^{x_i-1}j^2.
\]

At positive likelihood, r > 0, 0 < p < 1 and 0 ≤ π < 1. The finite endpoint members excluded by these inequalities are point masses at zero and cannot maximize a sample containing a positive count. All-zero samples attain that point mass with nonunique coefficients; the nondegenerate uniqueness question below excludes them.

For fixed r,p, let f0 = pʳ. Inflation profiles out as

\[
 \widehat\pi(r,p)=\max\left\{0,\frac{z-f_0}{1-f_0}\right\}.
\]

When f0 ≤ z, the zero/nonzero indicator is saturated and the log likelihood becomes

\[
 B+\sum_{x_i>0}\log\frac{f_{\rm NB}(x_i;r,p)}{1-p^r},
 \qquad B=n_0\log z+k\log q,
\]

with 0 log 0 = 0. The expression B plus the positive-count likelihood is also an upper bound for every ZINB fit, including those with f0 > z. This separation has prior literature support; see Haslett et al., equation (4), in §6 below.

At each r the probability maximum is unique. On the π = 0 branch it is

\[
 p_N(r)=\frac r{r+\bar x}.
\]

On the saturated branch write p = exp(−τ); its unique conditional-mean solution is

\[
 \frac{r(e^\tau-1)}{1-e^{-r\tau}}=m. \tag{1}
\]

Both NB and positive-count NB, at fixed r, are strictly concave exponential-family likelihoods in log(1−p). Their probability scores agree at pʳ = z. Thus if p_N(r)ʳ ≥ z the first branch supplies the global fixed-r maximum; otherwise the second branch does. This also proves that a probability root is not lost at the join.

Let N(r) denote the ordinary-NB profile on all observations, and T(r) the truncated-NB profile on the positives. Since

\[
 f_{0,N}(r)=\left(1+\frac{\bar x}{r}\right)^{-r}
\]

strictly decreases from 1 to exp(−x̄), the complete all-free profile has a simple structure:

* If z ≤ exp(−x̄), Q(r) = N(r) for every r > 0.
* If z > exp(−x̄), there is exactly one rj with f0,N(rj) = z. Then Q(r) = N(r) below rj and Q(r) = B + T(r) above it.

The joined profile and its first derivative are continuous. These statements concern the fully profiled iid likelihood, including the attained π = 0 boundary.

## 2. Proved existence and non-existence results

### 2.1 Exact criterion and uniqueness under a zero deficit

**Theorem.** For a sample containing a positive count and satisfying z ≤ exp(−x̄), a finite ZINB maximum exists if and only if v > x̄. When it exists, it is unique and equals the ordinary-NB maximum with π = 0.

**Proof.** For every finite r,

\[
 p_N(r)^r>e^{-\bar x}\ge z.
\]

The fixed-r profiling argument in §1 therefore gives Q = N throughout the size domain. Apply the classical ordinary-NB existence and uniqueness theorem supplied in the task brief: its finite maximum exists exactly when v > x̄, and is unique. When v ≤ x̄ its supremum is the Poisson face. This proves the ZINB statement. ∎

This proof invokes the classical NB theorem; it does not supply a repaired proof of Aragón et al. The literature qualifications in §6 matter when citing that theorem's history.

### 2.2 The correctly constrained C > 0 condition is sufficient

Let (λ*,π*) maximize the **admissible** ZIP likelihood. In the zero-deficit region λ* = x̄ and π* = 0. In the zero-excess region, λ* is the positive solution

\[
 \frac{\lambda_*}{1-e^{-\lambda_*}}=m,
 \qquad \pi_*=1-\frac q{1-e^{-\lambda_*}}>0.
\]

Set C = A − Sλ*/2. Along the admissible finite-NB path p = exp(−λ*/r), π = π*,

\[
 \ell_{\rm ZINB}-\ell_{\rm ZIP}
 =\sum_{x_i>0}\left\{\sum_{j=0}^{x_i-1}\log(1+j/r)
 +x_i\log\frac{1-e^{-\lambda_*/r}}{\lambda_*/r}\right\}
 =\frac C r+O(r^{-2}). \tag{2}
\]

Consequently C > 0 gives a finite witness strictly above the global ZIP supremum. Attainment follows: Q is continuous, its infinite-size limit is the maximized ZIP likelihood, and Q(r) → −∞ as r ↓ 0. For the last claim, when 0 < r ≤ 1 and integer x ≥ 1, (r)x/x! ≤ r, hence every positive-observation mass is at most r, uniformly in p and π. The remaining probability maximizations at finite r are the unique proper solutions in §1. ∎

On the π* = 0 face, C = n(v − x̄)/2, and §2.1 supplies necessity too. On the positive-inflation ZIP face, m > 1 and **C cannot equal zero for a finite integer sample**: equality would make λ* = 2A/S a positive rational, while equation (1) at the Poisson limit would make exp(−λ*) = 1 − λ*/m rational. This contradicts the Hermite–Lindemann theorem. Thus the unsolved zero-excess case has a strictly positive or strictly negative C, although it can be arbitrarily close to zero across samples.

**Open:** when z > exp(−x̄) and C < 0, equation (2) establishes only local superiority of the ZIP face over sufficiently large finite r. A finite maximum further away has not been ruled out in general.

### 2.3 A proved zero-excess non-existence region

**Theorem.** Suppose every positive observation equals an integer j ≥ 2. Let λj/(1−exp(−λj)) = j. If q ≤ 1−exp(−λj), the ZINB supremum occurs only on the ZIP face; no finite maximum exists.

**Proof.** A finite NB with positive likelihood is a nondegenerate gamma mixture of Poisson laws. Conditioning on positivity makes its positive-count law a mixture of zero-truncated Poisson laws, with mixing measure proportional to (1−exp(−λ)) times the gamma measure. The conditional mass

\[
 g_j(\lambda)=\frac{e^{-\lambda}\lambda^j}{j!(1-e^{-\lambda})}
\]

has a unique maximum at λj: its log derivative is j/λ − 1/(1−exp(−λ)), and the conditional mean is strictly increasing. A nondegenerate continuous mixing measure gives a conditional mass strictly below g_j(λj). Therefore every finite ZINB likelihood is strictly below the upper bound B + k log g_j(λj). The assumed inequality makes the ZIP parameters at λj and saturated indicator mass admissible, so a sequence r → ∞ attains that supremum in the closure. ∎

An **exact counterexample to extending the ordinary-NB variance criterion** is the sample (0,0,0,4). It has x̄ = 1 and v = 3. Here 3 < λ4 < 4, so q = 1/4 < 1−exp(−λ4), and the theorem proves non-existence. Its C = 6−2λ4 is strictly negative. The rate bracket follows from exp(3) > 4 and monotonicity of the conditional Poisson mean.

A separate feasibility trap is (1,4): its conditional-Poisson rate λ solves a conditional mean of 5/2, and λ < 12/5, giving A−Sλ/2 > 0. However, with no zeros that conditional fit would require negative inflation. The admissible ZIP fit has λ* = 5/2, π* = 0 and C = −1/4; v = 9/4 < 5/2, so §2.1 proves non-existence. The inequality λ < 12/5 follows from exp(12/5) < exp(3) < 25. **The conditional-rate formula must be accompanied by its inflation feasibility check.**

## 3. The precise missing inequality and counterexample searches

For the positive sample define wj = k⁻¹#{i: xi > j}, j ≥ 1, and let τ(r) solve (1). Put

\[
 J(t)=1-\frac t{e^t-1},\quad E(t)=\frac1{e^t-1}-\frac1t,
 \quad H(r)=\sum_{j\ge1}\frac{j w_j}{r+j}.
\]

Direct differentiation gives

\[
 \frac{rT'(r)}k=F(r)=mJ(\tau(r))-H(r),
\]

\[
 \tau'(r)=\frac{\tau E(r\tau)}{1+E(\tau)-rE(r\tau)},\qquad
 F'(r)=mJ'(\tau)\tau'(r)+\sum_{j\ge1}\frac{j w_j}{(r+j)^2}. \tag{3}
\]

At a stationary point, T″(r)/k = F′(r)/r. A sufficient route to a universal theorem is therefore to prove

\[
 F(r)=0\quad\Longrightarrow\quad
 \sum_{j\ge1}\frac{j w_j}{(r+j)^2}<-mJ'(\tau)\tau'(r) \tag{4}
\]

for every empirical positive-count distribution with mean m > 1, or at least for stationary points feasible in the ZINB branch. Strictness must cover degenerate stationary points too.

If (4) holds, every stationary point of T is a strict downward crossing. The ordinary-NB branch already has at most one such crossing, the join is continuously differentiable, and the small-r profile initially increases. Thus the complete ZINB profile can have at most one maximum. Its tail derivative has sign −C. Together with §2, this would establish the desired iff-C criterion and nondegenerate uniqueness. **I have not proved (4).** Separate concavity in held parameters does not establish it.

The searches exploit that, at fixed r,m, the mean solution is independent of the histogram, while likelihood differences, F and the empirical term in F′ are linear in histogram weights. Continuous frequency weights enlarge the set of integer samples and provide useful candidate searches; floating-point LP solutions do not constitute certificates.

| Search | Workload and result |
|---|---|
| Finite improvement with C ≤ 0 | 360 successful LPs; positive counts 1–100; m in {1.1,1.5,2,3,5,10}; 60 logarithmic sizes from 0.001 to 10000. No improvement above 10⁻⁹ per positive observation; the largest computed gain was negative. |
| A negative-to-positive F crossing | 1827 successful LPs over pairs of sizes, counts 1–100 and seven means from 1.1 to 20. No candidate minimum. |
| Nonnegative stationary curvature | 1300 LPs, counts 1–1000, 13 means from 1.001 to 100, sizes from 10⁻⁴ to 10⁵. The workstation solver reported success on all cells and nominated 23 candidates. |
| High-precision adjudication | Re-solving mean and stationarity constraints at 80 digits leaves 20 feasible candidates, all with negative curvature. Three single-atom nominations require negative corrected weights and are infeasible. |

Simbox nominated the same 20 three-point candidates; it reported 1296 successful curvature LPs, one infeasible cell and three solver-problem cells. These outcomes are retained separately, including [its search](2026-11-07-zinb-theory-data/curvature-search-simbox.json) and [adjudication](2026-11-07-zinb-theory-data/curvature-recheck-simbox.json). The final workstation [search](2026-11-07-zinb-theory-data/curvature-search.json) and [adjudication](2026-11-07-zinb-theory-data/curvature-recheck.json) supersede neither record. Machine-dependent solver feasibility decisions explain why every nominated candidate needed rechecking.

**Open, precisely:** prove (4), prove an adequate weaker statement on the feasible branch, or supply an exact/certified counterexample. These finite searches establish neither the inequality nor exhaustive literature coverage. No positive-data example with two equal global maxima was found. The known all-zero nonidentification and held-parameter multimodality do not settle that question.

## 4. Independent global certificate for the near-face sample

The sample has n0 = 5000, positive counts 1(3958), 2(4063), 4(1979), k = 10000, S = 20000 and m = 2. Use dispersion d = 1/r and u = −r log p, so the base zero mass is exp(−u). Define the analytic extension

\[
 h(v)=\log\frac{1-e^{-v}}v,\qquad h(0)=0.
\]

The positive-count log likelihood is exactly

\[
 L(d,u)=\sum_{x_i>0}\sum_{j=0}^{x_i-1}\log(1+jd)
 +S\log u+S h(ud)-ku-k\log(1-e^{-u})
 -\sum_{x_i>0}\log(x_i!). \tag{5}
\]

At d = 0 this is the truncated-Poisson likelihood. This formula avoids large log-gamma differences. The mean equation is expm1(ud)/[d(1−exp(−u))] = 2, with its continuous d = 0 extension.

### 4.1 Compact size region

For this histogram, w1 = 6042/10000 and w2 = w3 = 1979/10000. The independent checker proves T′(r) > 0 for 0 ≤ r ≤ 100:

* On [0,0.01], it uses the lower bound A0(a)−τ0[1+E(aτ0)] for T′/k, where A0(r) = Σwj/(r+j), a = 0.01, and τ0 is an upper bracket for the limiting mean root. The computed lower bound exceeds 0.13289.
* On [0.01,100], monotonicity of τ and J gives F(r) ≥ 2J(τ(b))−H(a) on [a,b]. A contiguous cover of **445 intervals**, obtained with 889 evaluations, has minimum lower bound greater than 3.45×10⁻⁷.

All conditional-mean roots have independently signed brackets. The full cover is retained in [near-cover.json](2026-11-07-zinb-theory-data/near-cover.json).

### 4.2 All sizes above 100, including the infinite-size face

Here 0 ≤ d ≤ 0.01. The mean root lies in 1.55 ≤ u ≤ 1.6 throughout: the mean at (d,u) = (0.01,1.55) is below two and at (0,1.6) is above two, and the mean increases with d and u.

For v ≥ 0,

\[
 0<h''(v)\le\frac1{12},\qquad
 h'(v)+vh''(v)=-J'(v),\qquad 0<J'(v)\le\frac12,
\]

with the appropriate endpoint limits. These inequalities follow from the exponential formulas and sinh inequalities. For example, J′ ≤ 1/2 is equivalent to sinh(v) ≥ v. The h″ upper bound follows by expanding (12−v²)sinh²(v/2): its v² term is 3v², its v⁴ term vanishes and all higher coefficients are negative.

The Hessian of (5) is

\[
 L_{dd}=-\sum_i\sum_j\frac{j^2}{(1+jd)^2}+Su^2h''(ud),
\]
\[
 L_{du}=S[h'(ud)+ud h''(ud)],\qquad
 L_{uu}=-S/u^2+\frac{k e^{-u}}{(1-e^{-u})^2}+Sd^2h''(ud).
\]

More generally, with largest positive count M and a box [0,D]×[V,U], sufficient uniform bounds are

\[
 a=\frac{B_2}{(1+(M-1)D)^2}-\frac{SU^2}{12}>0,
\]
\[
 b=\frac S{U^2}-\frac{k e^{-V}}{(1-e^{-V})^2}-\frac{SD^2}{12}>0,
 \qquad ab>S^2/4. \tag{6}
\]

They imply −Ldd ≥ a, −Luu ≥ b and |Ldu| ≤ S/2, hence a negative-definite Hessian throughout the box. This is a sufficient concavity condition, not an assertion that every ZTNB likelihood is jointly concave.

For this sample B2 = 31769, D = 0.01, V = 1.55, U = 1.6. The computed bounds are

| Quantity | Lower bound, or stated upper bound |
|---|---:|
| −Ldd | 25678.6627706 |
| −Luu | 4392.03101108 |
| absolute Ldu, upper bound | 10000 |
| determinant of −Hessian | 12781483.2116 |

Thus L is strictly concave on the entire box. At each d its unrestricted best u lies inside that box, so its unique stationary maximum dominates every u at every size r ≥ 100. The compact-region monotonicity excludes all smaller sizes.

A mean-profiled dispersion score changes from positive to negative across the narrow bracket in [near-certificate.json](2026-11-07-zinb-theory-data/near-certificate.json). Its endpoint score bounds are approximately +7.5689×10⁻²⁶ and −7.5689×10⁻²⁶. The certified size interval is

\[
 8108.2388581481329544584734856
 <r_*<
 8108.2388581481329544584751074.
\]

At that maximum the inflation recovered from the empirical positive fraction is strictly positive. Hence B + L attains the universal bound from §1. The unique truncated-NB maximum and the unique indicator maximum give the **unique all-free ZINB maximum**, including comparison with the NB boundary and ZIP face. No ordinary-NB delegate or numerical size cap is needed for this argument.

The checker uses 65-digit arithmetic with a stated practical comparison padding of 10⁻⁴⁵; the root proposal is independently calculated at 70 and 110 digits. This is a certificate in exact arithmetic conditional on the computed, padded comparisons, following the package convention. It is not outward-rounded interval arithmetic or a universal mpmath error theorem. The certificate budget is 128 min(iterlim,100000), default iterlim = 1000; exhaustion raises an error and supplies no accepted maximum.

## 5. Why the fitter has difficulty

[near-face.json](2026-11-07-zinb-theory-data/near-face.json) records the full high-precision calculation; [near_face.py](2026-11-07-zinb-theory-data/near_face.py) independently differentiates (5) and the full joint log likelihood.

The fitted u is 1.59336048871561574; the ZIP rate is 1.59362426004004009, and its inflation is 0.16333268338934982. C is only 0.75739959959907677 despite 10000 positive observations.

At the Poisson face the mean-profiled dispersion curvature is −6145.31187225. Expanding the profiled likelihood gives

\[
 T(d)=T(0)+Cd+\tfrac12 H_0d^2+O(d^3),
\]

\[
 H_0=-B_2+\frac{S\lambda_*^2}{12}+\frac{S^2}{4I},
 \qquad I=\frac S{\lambda_*^2}-\frac{k e^{-\lambda_*}}{(1-e^{-\lambda_*})^2}.
\]

The quadratic prediction r ≈ −H0/C is 8113.6983, close to the actual 8108.2389. A small positive C combined with ordinary dispersion curvature puts the optimum close to d = 0.

At the optimum, the observed mean-profiled curvature in log size is only **−9.33482447×10⁻⁵**. A one-percent size displacement has a quadratic likelihood cost of about **4.7×10⁻⁹**. The full observed negative Hessian of the **total** log likelihood, in coordinates (log r, log u, π), has eigenvalues approximately

\[
 (9.33482418\times10^{-5},\ 10303.12377,\ 49375.32281),
\]

so its coordinate-dependent condition number is approximately **5.29×10⁸**. These are optimization-conditioning diagnostics, not Fisher information or an inferential covariance claim.

The finite advantage is only 3.11×10⁻⁹ per observation. The retained historical proposer in `zinb/mle.hpp` compares the face within 10⁻⁸n = 0.00015, exceeding the entire improvement; that rule explains its historical ZIP classification. The current default instead requires a certificate and safely stops when it cannot close the cover, as documented in the [prior certificate report](2026-11-06-zinb-cert-report.md).

The truncated-NB certificate's normalized score is F = rT′/k. A log-size displacement of 5×10⁻⁷ near the optimum changes F by only about 4.7×10⁻¹⁵, below its roughly 10⁻¹³ binary64 practical comparison scale. Subdivision also loses information by bounding nearly cancelling terms independently. Increasing the work budget alone does not resolve that conditioning.

A later implementation could use the stable dispersion objective (5), derivative recurrences/series for h, and an analytic concavity box such as (6), with a certified compact cover for the rest. It would still need its own representation guards, floating-point margins and coefficient enclosure. This job makes no fitting-code changes and does not propose accepting an unchecked high-precision optimizer result.

## 6. Literature: inspected results versus abstracts

The exact queries are in [literature-searches.json](2026-11-07-zinb-theory-data/literature-searches.json); [literature-readings.json](2026-11-07-zinb-theory-data/literature-readings.json) records source access and scope. This is research reconnaissance, not a formal paper-workflows bibliography/support audit. Search non-discovery does not establish novelty.

**Primary statements and derivations inspected:**

* **Sampford (1955), “The Truncated Negative Binomial Distribution,” Biometrika 42, 58–69.** Read §§7–8, pp. 64–66, from the primary scanned pages. These give likelihood equations and endpoint conditions sufficient for a solution. On p. 66 necessity and uniqueness are explicitly conjectural. The earlier moment-estimation iteration does not prove likelihood uniqueness. [Publisher record](https://doi.org/10.1093/biomet/42.1-2.58), [primary pages 65–66](https://amsr.contentdm.oclc.org/digital/api/collection/p21050coll5/id/892/download).
* **Aragón, Eberly and Eberly (1991 preprint; published 1992).** Read the ordinary-NB theorem statement on printed p. 2, PDF p. 4, and the opening setup. It claims the variance criterion and uniqueness. The preprint is not a verified copy of the published proof, and Wang's later objection must accompany its citation. [Stanford primary preprint](https://purl.stanford.edu/gy645ty0351), [published record](https://doi.org/10.1016/0167-7152(92)90157-Z).
* **Haslett, Parnell, Hinde and de Andrade Moral (2022; online 2021), “Modelling Excess Zeros in Count Data: A New Perspective on Modelling Approaches.”** Read selected primary full-text sections, especially §2.2 and equation (4): the indicator/truncated-count likelihood separation and the NB mixing/zero-probability argument. These support the reduction framework; the inspected material supplies no universal joint ZINB criterion. [Primary full text](https://onlinelibrary.wiley.com/doi/full/10.1111/insr.12479).
* **Santos Silva (2017), “Local Maxima in the Estimation of the ZINB and Sample Selection models.”** Read the slides, particularly pp. 8–10. Their examples use covariates and compare numerical regression optima. They establish neither a counterexample for an iid intercept-only sample nor equality of global maxima. [Primary slides](https://www.stata.com/meeting/uk17/slides/uk17_Santos_Silva.pdf).

**Abstracts, previews or metadata only:**

* **Levin and Reeds (1977), “Compound Multinomial Likelihood Functions are Unimodal: Proof of a Conjecture of I. J. Good.”** Primary journal metadata located; attempts to obtain readable full text were blocked/challenged. The original NB proof was not inspected here. It remains the classical reference supplied in the brief. [DOI](https://doi.org/10.1214/aos/1176343741).
* **Wang (1996), “Estimation problems for the two-parameter negative binomial distribution.”** The publisher abstract reports a major problem with the 1992 proof. The full correction was not read, so its precise objection is not reconstructed here. [Publisher DOI](https://doi.org/10.1016/0167-7152(94)00259-2).
* **Dai, Bao and Bao (2013), “Maximum likelihood estimate for the dispersion parameter of the negative binomial distribution.”** Publisher abstract/introduction preview only: uniqueness is described under a condition, with a fixed-point convergence result when the score root is unique. The condition and theorem were not inspected; this is not evidence for a universal ZINB or truncated-NB theorem. [Publisher DOI](https://doi.org/10.1016/j.spl.2012.08.017).
* **Wyshak (1974), Algorithm AS 68, “A Program for Estimating the Parameters of the Truncated Negative Binomial Distribution.”** Publisher record, keywords and references inspected; no algorithm PDF or theorem read. The platform's 2018 timestamp is not the publication year. [Publisher record](https://doi.org/10.2307/2347059).

The inspected literature therefore contains a directly relevant historical conjecture, useful profile decompositions, and regression warnings. It does not settle the two open claims in this report. Source digests and acquisition URLs for the inspected scanned pages are retained in [source-records.json](2026-11-07-zinb-theory-data/source-records.json); copyrighted source PDFs and raster previews remain only in ignored scratch.

## 7. Final validation and reproduction

All final checks finished successfully before this report was written. [validation.json](2026-11-07-zinb-theory-data/validation.json) records the source revision, numerical tolerances, cross-machine comparison, search solver statuses and test counts. [source-sha256.txt](2026-11-07-zinb-theory-data/source-sha256.txt) freezes the NB, truncated-NB and ZINB dossier/code/fixture inputs. The tracked tree is unchanged from HEAD.

| Final command | Machine | Result |
|---|---|---|
| `/tmp/umvl-cpu/simbox-run just cpp-test` | simbox | 106/106 development shards passed |
| `/tmp/umvl-cpu/simbox-run just test` | simbox | 20983 passes; 0 failures, 0 warnings; 154 development-tier skips |
| `/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-07-zinb-theory-data/validate.py` | workstation | Passed all research replays and evidence assertions |
| Corrected 23-candidate adjudication | simbox | 20 negative-curvature corrections; 3 infeasible corrections |

The workstation used Python 3.14.5, mpmath 1.3.0 and SciPy 1.18.0. The retained simbox root calculation used Python 3.12.3 and mpmath 1.2.1; the C++ suite used GNU 13.3.0 and R was 4.6.1. Commands used the supplied reservations/runners, `nice -n 10`, one BLAS/OpenMP thread and worktree-local temporary directories. No simbox exit-75 fallback was needed. Local CPU reservation waits exceeded an hour; the waiting commands were allowed to finish.

The 70/110-digit relative size discrepancy is 2.04×10⁻⁵⁶. The stored 100-digit numerical values agree across machines. The signed global certificate independently encloses size to relative width 2×10⁻²⁵. Agreement of numerical roots is a cross-check; global optimality and uniqueness use §4's inequalities and cover.

The C++ configure step emitted an existing CMake CMP0167 development warning; all builds and tests passed. SciPy warns that its `threads=1` option is forwarded to HiGHS; that forwarding intentionally pins its worker count. Research LP solver statuses are recorded separately from test passes and from proofs. The corrected checker treats infeasible candidates explicitly. An earlier orchestration attempt needed an import-path fix, and the first workstation adjudicator assumed three-point LP outputs; both were corrected before the final successful replay. No failure was converted into an accepted maximum.

No full tier, no-long-double, JS or site rerun was required by the change scope: only new research artifacts were added, with no fitter, density, test-infrastructure, binding, message or dossier change. The development tiers were run as requested. The earlier certificate report's full-suite records remain historical evidence, not runs attributed to this job.

To reproduce the numerical work, set `TMPDIR`, `JUST_TEMPDIR` and `XDG_RUNTIME_DIR` to `build/tmp`, set `PYTHONDONTWRITEBYTECODE=1`, and run the final workstation validation command in the table through the slot lock. It runs the LP searches, high-precision adjudication, 70/110-digit roots and the full near-face certificate. `run_remote.py` provides a foreground simbox replay with immediate evidence retrieval before another mirror. `read_sources.py` acquires sources only into ignored scratch; source acquisition is separate from mathematical validation.

A read-only host lock-holder inspection was not executed because automatic approval review timed out. This diagnostic timeout did not block any research or final check. No command started for this job remains running.

## Artifact display

The requested `show` command was run after writing this report. The viewer returned HTTP 403 because its readable-root list excludes this worktree. The report remains at the requested path; no viewer configuration or file outside this worktree was changed.
