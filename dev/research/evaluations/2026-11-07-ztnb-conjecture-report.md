# The two-parameter zero-truncated NB conjecture — TASK-116.20

**The general conjecture is not resolved by this investigation.** No exact or certified counterexample was found, and no later published settlement was verified within the 20-query literature search. The results proved here are:

1. A sharp reduction of the stationary-curvature problem from arbitrary positive-count histograms to histograms supported on **`{1,j,j+1}`**. There is no imposed maximum-count cutoff in this reduction. It leaves the explicit scalar inequality (12) below.
2. Strict increase of the size profile, and therefore non-existence of a finite-size maximum, whenever the empirical positive-count distribution is below its fitted truncated Poisson in convex order. This includes **every sample supported on two adjacent positive integers**, and also some non-adjacent samples.
3. The corresponding non-existence result for ZINB when the saturated ZIP fit is admissible.

The remaining scalar inequality was positive in all **1,012 numerical evaluations**, including sizes from `1e-12` to `1e12`, means as close to one as `1.00000001` and as large as `1000001`, and extremal support indices up to **1,802,147**. These are numerical results, not a proof over the continuous parameter domain.

The distinction between positive size and the package domain matters: `ztnbinom` includes the **attained size-zero logarithmic-series member**. The conjectured interior criterion has two endpoint conditions; the package's conjectured finite-maximum criterion has only the Poisson-end condition. All-one samples are a separate nonidentification case.

No fitting code, dossiers, tests, goldens, bindings, generated d/p/q/r inputs, roadmap, status or release notes were changed. No commits, Git writes, board edits, installations or sub-agents were used. Evidence and reproducible checks are in [the data directory](2026-11-07-ztnb-conjecture-data/). All final research checks and the C++/R dev suites completed successfully before this report was written.

## 1. Model and the exact question

Let the positive observations have empirical distribution P, sample size k, mean m > 1 and maximum M. Put

\[
 f_{r,t}(x)=\frac{(r)_x}{x!}\frac{(1-e^{-t})^x}{e^{rt}-1},
 \qquad x=1,2,\ldots,\quad r,t>0.
\]

Here `size = r`, `prob = exp(-t)`. For each r the unique probability maximum solves

\[
 m=\frac{r(e^t-1)}{1-e^{-rt}}. \tag{1}
\]

Write t = t(r) and T(r) for the total probability-profiled log likelihood. The limit at r = 0 is logarithmic series, with mean equation

\[
 m=\frac{e^{t_0}-1}{t_0}.
\]

The infinite-size limit is the truncated Poisson Y with rate λ determined by

\[
 m=\frac{\lambda}{1-e^{-\lambda}}.
\]

Define, for integer x ≥ 1,

\[
 a_r(x)=\sum_{i=1}^{x-1}\frac1{r+i},\quad
 h_r(x)=\sum_{i=1}^{x-1}\frac{i}{r+i},\quad
 b_r(x)=\sum_{i=1}^{x-1}\frac{i}{(r+i)^2}.
\]

All three vanish at x = 1. Let

\[
 J(t)=1-\frac{t}{e^t-1},\qquad E(u)=\frac1{e^u-1}-\frac1u.
\]

The identities in the [ZINB theory report, §3](2026-11-07-zinb-theory-report.md#3-the-precise-missing-inequality-and-counterexample-searches) become

\[
 F(r):=\frac{rT'(r)}k=mJ(t(r))-\mathbb E_P h_r(X),
\]
\[
 t'(r)=\frac{tE(rt)}{1+E(t)-rE(rt)}<0,
 \qquad F'(r)=\mathbb E_P b_r(X)-\mathcal D(r,m),
\]
\[
 \mathcal D(r,m):=-mJ'(t)t'(r)>0. \tag{2}
\]

At a stationary point T″/k = F′/r. Thus the requested inequality is precisely

\[
 \mathbb E_P h_r(X)=mJ(t)
 \quad\Longrightarrow\quad
 \mathbb E_P b_r(X)<\mathcal D(r,m). \tag{3}
\]

The numerical checks independently differentiate the profiled likelihood by finite differences with probability re-solved at each point. Their errors against (2) are below `6e-28` at the stated test point. This verifies the implementation of these formulas numerically; the algebraic identities themselves follow by differentiation and (1).

## 2. What the conjecture would imply, including both endpoints

Set

\[
 K=\mathbb E_P\frac{X(X-1)}2,\qquad
 C=K-\frac{m\lambda}{2},\qquad
 S_0=\mathbb E_P\sum_{i=1}^{X-1}\frac1i-\frac{t_0}{2}.
\]

These signs use C in the same direction as the ZINB report's `C/k`; the truncated-NB dossier's tail-bound constant uses the opposite sign. The profile extends continuously to both ends, and

\[
 T'(0)/k=S_0,\qquad
 T(r)=T(\infty)+\frac{kC}{r}+O(r^{-2}),\qquad
 T'(r)/k=-\frac C{r^2}+O(r^{-3}). \tag{4}
\]

**Already proved without the conjecture:** C > 0 guarantees attainment of a maximum on the package domain r ≥ 0. A sufficiently large finite size improves on the Poisson limit, so continuity and compactification force a finite maximum. If also S₀ > 0, neither endpoint can maximize, and a positive-size global maximum exists. These are sufficiency statements, not uniqueness statements.

For a finite integer sample with m > 1, neither S₀ nor C is zero. Indeed S₀ = 0 would make t₀ a positive rational and `exp(t₀) = 1 + mt₀` rational; C = 0 would make λ a positive rational and `exp(-λ) = 1 - λ/m` rational. Both contradict the Hermite–Lindemann theorem. This observation concerns empirical rational frequencies, not arbitrary real frequency weights.

**Conditional on (3) for all stationary points:** every stationary point is a strict downward crossing, so there can be at most one. Consequently:

| Domain and sample | Conditional complete conclusion |
|---|---|
| Classical r > 0, m > 1 | A positive-size MLE exists iff S₀ > 0 and C > 0; it is unique. |
| Package r ≥ 0, m > 1 | A finite-size maximum exists iff C > 0; it is unique. If S₀ < 0 it is the logarithmic member; if S₀ > 0 it has positive size. |
| C < 0 | The profile strictly increases to the truncated-Poisson supremum. |
| All observations one | `prob = 1` attains the point mass; free size is unidentified. |

The endpoint sign assertions follow from (4). A negative-to-positive transition would require a stationary point that is not a downward crossing. In particular the sign combination S₀ < 0, C < 0 would be impossible under (3). **This table is conditional, not a claimed solution.**

The positive sample `(1,4)` illustrates why the ordinary-NB sample-variance rule cannot replace these conditions. It has m = 5/2 and variance 9/4 < m, yet both sufficient interior conditions hold. Here K = 3, `E harmonic(X-1) = 11/12`, λ < 12/5, and t₀ < 11/6. The first bound follows because `exp(12/5) < exp(5/2) < 3^(5/2) < 25`; the second follows from the degree-four exponential Taylor lower bound at 11/6. Monotonicity of the two mean equations gives C > 0 and S₀ > 0. Thus this truncated sample has an interior global maximum, without assuming uniqueness. As explained in the prior ZINB report, applying its conditional-Poisson fit to the untruncated sample would instead require inadmissible negative inflation.

## 3. A proved non-existence class via convex order

For equal-mean distributions, P ≤cx Q means that every convex function with finite expectations has expectation under P no greater than under Q.

**Lemma 1.** At the same conditional mean m > 1, truncated NB distributions become strictly less dispersed in convex order as r increases. Every finite-r distribution strictly dominates the mean-m truncated Poisson in convex order.

**Proof.** For 0 < r₁ < r₂, write Rₓ = f₁(x)/f₂(x), with each probability fitted to mean m. Then

\[
 \frac{R_{x+1}}{R_x}
 =\frac{1-e^{-t_1}}{1-e^{-t_2}}\frac{r_1+x}{r_2+x}
\]

strictly increases with x. The likelihood ratio is therefore strictly log-convex. Its sublevel set below one is an interval. Equal mass and equal mean, together with nonidentity, force the difference of the probabilities to have the sign pattern `+, -, +`, allowing empty integer stretches at a crossing. For any convex function, subtract its secant line through the crossing locations. The function-minus-line is nonnegative outside and nonpositive inside; multiplying by the probability difference and summing proves the convex-order assertion. The Poisson comparison is identical: the consecutive likelihood-ratio factor is `(1-exp(-t))(r+x)/λ`, again strictly increasing. The distributions are distinct, so a strictly concave function with strictly negative second differences has a strict expectation inequality. ∎

**Theorem 1.** If the empirical P satisfies P ≤cx Y, where Y is its fitted mean-m truncated Poisson, then T′(r) > 0 for every finite r > 0. The supremum occurs only at the Poisson face, including comparison against the attained logarithmic endpoint.

**Proof.** The sequence aᵣ is strictly concave since its consecutive increments are `1/(r+x)`. The fixed-parameter model score identity gives

\[
 \mathbb E_{f_{r,t}}a_r(X)=\frac{t}{1-e^{-rt}}-\frac1r.
\]

Thus, by Lemma 1 and P ≤cx Y,

\[
 \frac{T'(r)}k
 =\mathbb E_P a_r-\mathbb E_{f_{r,t}}a_r
 \ge\mathbb E_Y a_r-\mathbb E_{f_{r,t}}a_r>0. \tag{5}
\]

Continuity at zero and infinity gives the stated endpoint comparison. ∎

**Corollary 1.** Every non-all-one sample supported on `{j,j+1}`, for any positive integer j, has no finite-size truncated-NB maximum. This includes constant samples greater than one.

**Proof.** At a given mean, the distribution supported on the two integers bracketing that mean is minimal in convex order among all integer distributions. Apply Theorem 1. ∎

The broader condition is finitely checkable. For maximum observed count M, compare the stop-loss expectations only at integer thresholds `l = 2,...,M-1`:

\[
 \mathbb E_P(X-l)_+
 \le\frac{\lambda\Pr\{Z\ge l\}-l\Pr\{Z\ge l+1\}}
 {1-e^{-\lambda}},\qquad Z\sim\operatorname{Pois}(\lambda). \tag{6}
\]

Thresholds at or below one agree by equal means; thresholds at or above M have zero empirical stop loss. Piecewise linear interpolation proves that integer thresholds suffice. This supplies a proof criterion; implementing it would require signed bounds for the mean root and comparisons.

For example, the integer sample `1(1), 2(18), 3(1)` is not supported on adjacent integers. It has m = 2; only threshold two needs checking. The empirical stop loss is 1/20 and the model stop loss is `Pr(Y=1) = 2-λ`. Since `exp(9/5) < exp(2) < 9 < 10`, the mean equation gives λ < 9/5, hence the comparison is strict. Numerically λ = 1.59362426004004 and the stop-loss margin is 0.35637573995996. Its profile therefore strictly increases globally by proof, rather than by a finite grid search.

Total convex ordering of the model family does **not** prove general profile unimodality: an arbitrary empirical histogram need not be ordered relative to the fitted Poisson. Separate concavity with either coefficient held also does not establish (3).

## 4. Sharp three-point reduction of inequality (3)

Fix r > 0 and write hₓ = hᵣ(x), bₓ = bᵣ(x). With `Δgₓ = gₓ₊₁-gₓ`,

\[
 \Delta h_x=\frac{x}{r+x},\qquad
 \Delta b_x=\frac{x}{(r+x)^2},\qquad
 \Delta^2h_x=\frac r{(r+x)(r+x+1)}>0.
\]

The decisive identity is

\[
 \frac{\Delta^2b_x}{\Delta^2h_x}
 =\frac1{r+x}+\frac1{r+x+1}-\frac1r,
 \tag{7}
\]

which strictly decreases with x.

**Lemma 2 (supporting plane).** For every integer j ≥ 2 there are unique αⱼ, βⱼ such that

\[
 b_x\le\alpha_jh_x+\beta_j(x-1),\qquad x=1,2,\ldots, \tag{8}
\]

with equality precisely at x ∈ `{1,j,j+1}`. Explicitly,

\[
 \alpha_j=
 \frac{b_j/(j-1)-j/(r+j)^2}{h_j/(j-1)-j/(r+j)},
 \qquad \beta_j=\frac j{(r+j)^2}-\alpha_j\frac j{r+j}.
 \tag{9}
\]

**Proof.** Strict convexity of h makes the interpolation denominator nonzero. Let `gₓ = bₓ-αⱼhₓ-βⱼ(x-1)`. Its three interpolation values vanish. By (7), Δ²g has at most one sign transition, from positive to negative, so Δg is unimodal. Also Δgⱼ = 0 and `Σₓ₌₁^{j-1} Δgₓ = 0`.

For j > 2, Δ²gⱼ₋₁ must be negative: otherwise all earlier second differences would be strictly positive, all preceding first differences would be negative, and their sum could not be zero. All later second differences are then negative too. Before j, the first differences must start negative and cross once to positive before falling to Δgⱼ = 0. Their proper partial sums are negative. After j+1 all first differences are negative. Hence g is negative at every other integer. For j = 2 the two consecutive zero first differences force Δ²g₁ = 0, and (7) makes every subsequent second difference negative. This proves (8), including strictness. ∎

Now prescribe the two moments `E X = m` and `E hₓ = H`, and put

\[
 c_j=\frac{h_j}{j-1},\qquad \eta=\frac H{m-1}.
\]

The cⱼ strictly increase from `1/(r+1)` to one. Choose j with `cⱼ ≤ η ≤ cⱼ₊₁`, and define

\[
 v=\frac{c_{j+1}-\eta}{c_{j+1}-c_j},\qquad
 p_j=\frac{(m-1)v}{j-1},\qquad
 p_{j+1}=\frac{(m-1)(1-v)}j,\qquad
 p_1=1-p_j-p_{j+1}. \tag{10}
\]

For feasible moments these probabilities are nonnegative. To see the only nontrivial part, if m ≤ j then `pⱼ+pⱼ₊₁ ≤ (m-1)/(j-1) ≤ 1`. If j < m ≤ j+1, nonnegativity of p₁ is equivalent to H being at least the straight-line interpolation of h at mean m, which follows from convexity. The alternative m > j+1 contradicts that same lower bound and η ≤ cⱼ₊₁.

**Theorem 2.** Among all positive-integer distributions with the prescribed feasible moments, the largest `E bₓ` is

\[
 B_{\max}(r,m,H)=p_jb_j+p_{j+1}b_{j+1}
 =\alpha_jH+\beta_j(m-1). \tag{11}
\]

It is attained by (10), supported on `{1,j,j+1}`; any distribution assigning mass elsewhere has a strictly smaller value.

**Proof.** Take expectations in (8), then use (10) to attain equality. ∎

At a stationary point put H = mJ(t), with t determined by (1). This moment pair is feasible because the mean-m truncated-NB model itself has those moments. Thus a sufficient scalar form of the entire conjecture is

\[
 \boxed{\quad
 \mathcal D(r,m)>
 \alpha_j(r)mJ(t(r,m))+\beta_j(r)(m-1),
 \quad r>0,\ m>1,\quad}
 \tag{12}
\]

where j is selected by (10). This is an exact reduction for the enlarged problem allowing arbitrary real frequencies. Any empirical counterexample to (3) forces a violation of (12). A strict violation of (12) constructs a real-weight stationary histogram with nonnegative curvature; rational empirical realizability would still need checking. Equality for real weights alone is not automatically an empirical counterexample.

For j = 2 the outstanding inequality simplifies to

\[
 \mathcal D(r,m)>
 \frac{(r^2-2)mJ(t)+2(m-1)}{r(r+1)(r+2)}. \tag{13}
\]

In particular, all stationary histograms with maximum count at most three fall into this case. **I have not proved (13) on its full feasible parameter region, nor (12) generally.** The supporting-plane identity and optimization theorem are proved; positivity of the remaining scalar expression is presently numerical.

## 5. Numerical investigation and reproducibility

[check.py](2026-11-07-ztnb-conjecture-data/check.py) evaluates (12) directly at its sharp extremal distribution. This improves on a histogram LP with an arbitrary maximum count: the proven optimizer (10) includes the unrestricted positive-integer support problem. Digamma/trigamma formulas for h and b, checked against direct sums where j ≤ 200, and bounded integer doubling/bisection locate j without enumerating millions of counts.

The workloads, with overlaps deliberately retained, are:

- 637 cells: thirteen means `{1.001,1.01,1.05,1.1,1.25,1.5,2,3,5,10,20,50,100}`, and sizes `10^(e/3)` for integer e from −24 to 24.
- 375 cells: means `1+10^a`, integer a from −8 to 6, and sizes `10^e`, integer e from −12 to 12.

There are 944 distinct parameter pairs. Every one of the 1,012 evaluations gave feasible extremal weights and a positive scaled margin

\[
 (1+r)^2\{\mathcal D-B_{\max}\}.
\]

The smallest margin was approximately **3.3333332611e-29**, at `m = 1.00000001`, `r = 1e-12`, `j = 2`. All comparisons required a positive margin above `1e-45`. Independent 85- and 110-digit runs agree in scaled margins to relative-to-`1+abs(value)` discrepancy below **3.21e-53**. The extreme-small-mean/size corner illustrates why ordinary double-precision curvature comparisons can be unreliable.

Additional checks passed:

| Check | Workload and outcome |
|---|---|
| Supporting-plane regression | 14,094 exact rational inequalities; r in `{1/10,1/2,1,7/3,10,100}`, j = 2,…,30, x = 1,…,81; correct equality set throughout. |
| Independent LP | Nine SciPy/HiGHS moment-constrained optimizations; maximum absolute discrepancy from the sharp formula below `5.90e-11`, tolerance `2e-7`. |
| Profile differentiation | Central differences with step `1e-12`, histogram weights `(1/2,1/4,1/4)` at counts `(1,2,5)`, size 2.3; both identity errors below `6e-28`, tolerance `1e-20`. |
| Non-adjacent convex-order example | Independent mean root and stop-loss margin for `1(1),2(18),3(1)`; agreement with the proved comparison. |

The LP emits SciPy's notice that `threads=1` is forwarded to HiGHS; this is not an optimization failure. BLAS/OpenMP were also pinned by the slot runner. Every mean root has checked endpoint signs and a width stopping criterion; work-budget exhaustion raises an error. Root, support-doubling and integer-bisection budgets are each `min(iterlim,100000)`, default iterlim 1000. No numerical MLE or capped candidate is returned.

The saved [85-digit results](2026-11-07-ztnb-conjecture-data/checks.json), [110-digit results](2026-11-07-ztnb-conjecture-data/checks-110.json) and [validation record](2026-11-07-ztnb-conjecture-data/validation.json) specify the environment, workloads, tolerances and checks. These point calculations do not bound every intervening parameter value, and cross-precision agreement is not an error theorem. No claim of a universal certificate, outward-rounded enclosure or certified counterexample is made.

## 6. Literature findings

The [search and reading ledger](2026-11-07-ztnb-conjecture-data/literature.json) records all 20 queries, scope, stopping rule, links and access limits. This was research reconnaissance for the iid two-parameter problem, not a formal manuscript bibliography audit. Search results about regression, continuous distributions constructed using truncated NB, right truncation and one-parameter fitting do not settle this problem.

**Sampford (1955), §8, pp. 65–66:** I read the primary scanned pages, including equations (15)–(17) and the endpoint/conjecture paragraph. The two endpoint conditions are sufficient for a positive-size likelihood solution. Necessity and uniqueness are explicitly conjectural. Both limiting distributions, Poisson and logarithmic series, are discussed. This confirms the scope of the historical conjecture, not its status in all subsequent literature. [Publisher record](https://doi.org/10.1093/biomet/42.1-2.58), [primary scan](https://amsr.contentdm.oclc.org/digital/api/collection/p21050coll5/id/892/download).

**Simon (1961), pp. 47–48 and 50–51:** I read the primary full text. Equations (11)–(18) eliminate a freely adjusted zero mass and yield the truncated-NB likelihood equations. Equations (35)–(38) and the conclusion give an iteration and numerical examples; they supply no necessity or uniqueness proof. The distortion model permits more than nonnegative zero inflation, so its elimination does not remove the ZINB feasibility restriction. [Primary paper](https://www.casact.org/sites/default/files/database/proceed_proceed61_61045.pdf).

**Wyshak (1974), Algorithm AS 68, pp. 87–91:** I read the full primary PDF text: score equations, starting values, observed second derivatives, failure indicators, examples and Fortran. This is a numerical algorithm, with explicit failure conditions, not a proof that a unique solution always exists under an iff sample condition. Its variance-based applicability statement is not a demonstrated criterion for every positive sample. [Primary record and PDF access](https://academic.oup.com/jrsssc/article/23/1/87/6953447), [DOI](https://doi.org/10.2307/2347059).

**Brass (1958): unresolved full-text gap.** Publisher metadata were accessible; the PDF route led to a purchase page or HTTP 403. I did not inspect its proof text and do not infer its contents from the title or later citations. [Publisher DOI](https://doi.org/10.1093/biomet/45.1-2.59).

**Sanathanan (1977): unresolved full-text gap.** The preview advertises asymptotic population-size theory and a truncated-NB corollary. The full-text request returned HTTP 403; no finite-sample uniqueness theorem was verified. [Publisher DOI](https://doi.org/10.1080/01621459.1977.10480634).

A recent regression-robustness paper's search extraction discusses model equivalence and corrected Hessian calculations; direct full-text opening met a CAPTCHA. It was not treated as a proof reading or evidence for the finite-sample conjecture. [Primary repository page](https://pmc.ncbi.nlm.nih.gov/articles/PMC13134751/). A search hit advertising uniqueness for an inverse truncated-NB Weibull distribution concerned a different continuous model and was excluded. [Primary article](https://jds-online.org/journal/JDS/article/86/file/pdf).

**Literature conclusion:** no post-1955 settlement was verified in this bounded search. It would overstate the evidence to say that no such theorem exists. The inaccessible Brass text and uninspected later sources remain concrete coverage gaps. Ordinary-NB existence/uniqueness papers cannot be transferred to the truncated likelihood by omitting the normalizing term.

## 7. Consequences for ZINB and the certificates

**Unconditional consequence of Theorem 1.** Let z be the zero fraction and let the positive empirical distribution satisfy P ≤cx Y. If

\[
 z\ge e^{-\lambda}
 \quad\text{equivalently}\quad 1-z\le1-e^{-\lambda}, \tag{14}
\]

the saturated ZIP fit is admissible. Every finite ZINB likelihood is bounded above by the saturated indicator likelihood plus T(r). Theorem 1 makes this strictly smaller than the admissible ZIP limiting value. Thus there is **no finite ZINB maximum** in this region. This covers arbitrary adjacent positive counts and the non-adjacent example above, with any zero fraction satisfying (14). It extends the constant-positive-count result in the prior report. The zero-deficit region continues to have the separate ordinary-NB criterion already given there.

**Conditional full consequence if (3) is proved.** Combining strict downward crossings of the truncated profile with the ordinary-NB theorem and the continuously differentiable branch join in the prior report would establish the all-free ZINB result: for a sample containing positives, a finite maximum exists iff

\[
 C_{\rm ZINB}=A-S\lambda_*/2>0,
\]

where λ* comes from the **admissible** maximized ZIP fit. When attained, the nondegenerate maximum is unique, including a possible `pi = 0` boundary. All-zero coefficient nonidentification remains separate. In a zero deficit this is `n(variance-mean)/2`; in a zero excess it is k times C above. This conditional implication continues to depend on a valid ordinary-NB theorem; this job does not repair or reassess the proof under TASK-116.19.

**Current certificate implications.** The proof of the general inequality is still missing, so the existing sample-specific subdivision certificates in `ztnbinom` and `zinb` remain necessary. No universal one-root shortcut is justified by this report. Theorem 1 offers a rigorous early Poisson-face test after certified convex-order comparisons. Theorem 2 offers a sharp curvature bound at a stationary point and an explicit target for analytic or interval-wise work; a sign checked at one parameter pair cannot certify a whole size interval. Neither result changes the current floating-point margins, budgets, near-face conditioning, held-parameter guarantees or default coefficients. No implementation or settled d/p/q/r changes are needed for these research results.

## 8. Validation and precise open entries

The tested source revision is **`2fb7558010a91dab4d2c4172263cdbdaa99bb8f9`**, branch `codex/ztnb-conjecture`. [Source hashes](2026-11-07-ztnb-conjecture-data/source-sha256.json) freeze the relevant family files and engineering instructions. All computations ran on the **workstation**, through the shared slot locks, under `nice -n 10`, with BLAS/OpenMP pinned to one thread. No simbox run was used.

| Completed final command | Result |
|---|---|
| `run4`: `just cpp-test`, then `just test` | C++ **106/106 dev shards**; R **FAIL 0, WARN 0, SKIP 154, PASS 20983**. |
| `run`: `check.py`, at 85 and 110 digits | All 1,012 evaluations and auxiliary checks passed in each run. |
| `run`: `validate.py` | Evidence, cross-precision comparisons, source hashes, query count and completed suite summaries passed. |

Exact reproduction commands, Python/SciPy/mpmath versions, primary-source download digests, margins and logs are retained in [validation.json](2026-11-07-ztnb-conjecture-data/validation.json), [cpp-dev.txt](2026-11-07-ztnb-conjecture-data/cpp-dev.txt) and [r-dev.txt](2026-11-07-ztnb-conjecture-data/r-dev.txt). Compiler warnings are retained in the C++ log and are distinct from test failures. The full tier, no-long-double suite, JS build/tests and site check were not required or run: this job changes only research files, with no fitting, d/p/q/r, test infrastructure, binding message, dossier or public-fitter changes.

The remaining items are specific:

1. **Open — general mathematics:** prove (12), or a weaker inequality sufficient for rational empirical histograms, or construct an exact/certified counterexample. The supporting-plane reduction (7)–(11) is proved; positivity of its scalar remainder is not. Even its j = 2 specialization (13), beyond the convex-order cases, remains unproved here.
2. **Open — numerical coverage:** the saved grid supplies no cover of all r > 0 and m > 1. A computational proof would need validated parameter-region bounds and analytic treatments of r → 0, r → ∞, m → 1 and m → ∞; arbitrary caps or pointwise positive signs do not supply those bounds.
3. **Open — literature:** inspect Brass (1958) and obtain readable full text of the most relevant later candidates before claiming historical closure. The 20-query search found no verified settlement, but does not establish exhaustive absence.

These open entries limit the general conjecture and its full ZINB consequence. They do not qualify the proofs of the convex-order non-existence class or the sharp three-point extremal reduction above.
