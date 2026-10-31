# Open existence questions: research report

31 October 2026. Source revision: `b54a221c4ee3a1e8f015272b28057ac426fe1fbb`. Research only; no fitter, dossier or tests were changed. Here “MLE exists” means a parameter in the stated model attains the **global** likelihood supremum. A limiting model can be useful without being a member. An upper bound, a score root, a grid peak, and a proof of global attainment are different claims. Grades below attach to individual statements, not to a whole family.

The task called dgamma “double gamma”, but this repository's `dgamma` is the **discretized gamma**: there is no location parameter. The following addresses the actual fitter. All results concern the indicated free or held subset; they do not establish results for other subsets.

## 1. Beta-binomial, fixed size: exact algebraic decision

**Proved:** the following is a necessary and sufficient, finite, mechanically checkable sample condition for every integer size N ≥ 2 with both beta shapes free. It is a general real-algebraic decision, not a simple inequality in sample mean and variance, and its practical cost can be high. It also distinguishes an exact tie from an unattained binomial limit.

Let the sample be `x_1,...,x_n` in `{0,...,N}`, let `T = sum(x_i)`, and put `p0 = T/(nN)`. If all observations are 0 or all are N, the supremum is an unattained point mass. If observations are only 0 and N and both occur, the supremum is the unattained two-point law. Otherwise there is an observation strictly between 0 and N, `0<p0<1`, and the MLE exists **if and only if** the following explicit polynomial formula is true for real `p,s`:

```
0 < p < 1, s > 0, and
(nN)^(nN) * product_i [ product_{j=0}^{x_i-1}(p+j*s)
                         * product_{j=0}^{N-x_i-1}(1-p+j*s) ]
  - T^T * (nN-T)^(nN-T) * product_{j=0}^{N-1}(1+j*s)^n >= 0.
```

Empty products are one. The binomial coefficients have canceled. This is an existential sentence over the reals with integer coefficients derived directly from the sample; quantifier elimination or a cylindrical algebraic decomposition decides it exactly, including equality. This is **not** a claim that the repository implements such an algorithm. It is less convenient than an explicit statistic, but it does extend the existence rule to every fixed N.

**Proof.** Write `alpha=p/s`, `beta=(1-p)/s`; every positive finite pair is represented uniquely by `0<p<1,s>0`. The likelihood apart from binomial coefficients is the numerator product divided by `D(s)=product_j(1+j*s)^n`. At `s=0` it is `p^T(1-p)^(nN-T)`, uniquely maximized at `p0`, with value `B=T^T(nN-T)^(nN-T)/(nN)^(nN)`. Thus the displayed polynomial is exactly `L(p,s)>=B` after multiplying by positive quantities.

It remains to justify attainment when an interior point reaches B. For bounded s, likelihood tends to zero as `p` tends to either endpoint: at least one observed count is positive and at least one is less than N, and the corresponding rising factorial has a factor p or 1-p. For s tending to infinity, the probability of the observed interior count tends to zero uniformly in p: its rising-factorial expression contains one factor p and one factor 1-p in the numerator, while every factor with index `j>=1` is at most `1+js` and there is one more such denominator factor than numerator factors; it is bounded by `1/(1+s)`, up to the fixed binomial coefficient. Hence sequences with likelihood bounded away from zero are precompact in `p in [0,1], s in [0,infinity)`; the only non-interior positive-likelihood boundary is `s=0`, whose supremum is B. If no interior point reaches B, the binomial sequence approaches B and no MLE exists. If some interior point exceeds B, compactness yields an interior global maximum. If an interior point equals B, either no point exceeds it and it is itself a global maximum, or some point exceeds it and the previous argument applies. This proves both directions.

For endpoint-only samples, the beta-binomial is a mixture of binomials. The total probability of the endpoints is strictly less than one for a nondegenerate beta mixing distribution, whereas `alpha,beta -> 0` with their ratio fixed tends to an arbitrary mixture of the two endpoint masses. The saturated two-category likelihood is therefore a strict upper bound, approached by the correct endpoint proportions. The all-one-endpoint cases are analogous point-mass limits. For N=1 the two shapes are not jointly identified; the Bernoulli distribution itself is attained when both sample categories occur. N=0 is likewise unidentified.

**Proved examples:** for N=3 and sample `(0,1,2,3)`, the finite uniform beta-binomial (`alpha=beta=1`) has likelihood `1/256`, while the best binomial likelihood is `9/4096`, so their ratio is exactly `16/9` and a finite MLE exists. For `(1,1,2,2)` those two particular likelihoods have ratio `16/81`; this single comparison alone does not decide existence.

**Numerical evidence:** `checks.py` evaluates three size-3 samples at `s=1/2` and `p=p0`, compared with the face. These points illustrate, but do not decide, the polynomial criterion. The finite grid certificate in the current fitter is a floating-point likelihood bound, not exact algebra; its tolerance-based preference for `BinomialLimit` can select the limit when an exact finite maximizer ties or wins by less than the tolerance.

**Fitter consequence:** retain `PointMass`, `TwoPointLimit`, `Unidentified`; return `Ok` only after certifying a finite global maximum. Return `BinomialLimit` only after certifying that no finite maximizer attains the face, and use `IterationFailure` for unresolved comparisons, including unresolved equality. Existing tolerance policy is a numerical convention, not the exact existence rule. Holds remain structural and unchanged.

> **Proposed Details.** For fixed integer size at least two, endpoint-only data escape to a point mass or a two-point law. Otherwise, the existence question is exactly decidable by a polynomial inequality in the finite-shape coordinates `p = alpha/(alpha+beta)` and `s = 1/(alpha+beta)`: a finite MLE exists if and only if some `0<p<1,s>0` has likelihood at least the best binomial likelihood. Clearing the positive denominators gives an integer-coefficient polynomial; exact real quantifier elimination decides this condition, including ties. This criterion is not implemented by the numerical fitter. A likelihood comparison within floating-point tolerance does not prove exact non-existence.
>
> **Fields:** keep `limits` and structural `holds`; qualify the binomial `detect` as a tolerance-based numerical convention rather than an exact non-existence test. No `fit_domain` change.

Reference for the classical likelihood, not for the above decision theorem: D. M. Smith (1983), “Maximum Likelihood Estimation of the Parameters of the Beta Binomial Distribution,” *Journal of the Royal Statistical Society: Series C (Applied Statistics)* 32(2), 196–204, [doi:10.2307/2347299](https://doi.org/10.2307/2347299).

## 2. Tukey lambda: the U branch and the uniform junction

**Proved:** the density, and hence the sample likelihood when an observation is on a support endpoint, is not upper semicontinuous at lambda = 1 when approached from above. This is a property of the specified density version at support endpoints; densities that differ at single points define the same distributions but produce different likelihood values for an observation exactly at such a point.

The standardized quantile is `Q_lambda(u)=(u^lambda-(1-u)^lambda)/lambda`, and, for positive lambda, the support endpoints are `mu±sigma/lambda`. For `1<lambda<2`, `Q'_lambda(0)=Q'_lambda(1)=1`; hence each endpoint density is `1/sigma`. At lambda 1, `Q'_1(u)=2` including the endpoint convention in the dossier, so the endpoint density is `1/(2 sigma)`. Fix `mu=0,sigma=lambda`, so support is always `[-1,1]`. For the sample `(-1,1)` the likelihood is `lambda^(-2)` along lambda decreasing to 1 from above, while its value at the limiting parameters is `1/4`. This gives a jump by a factor four. An endpoint multiplicity k multiplies the corresponding one-sided limit by `2^k` relative to the lambda-1 likelihood at the same limiting support, with interior observations contributing their uniform density. The script checks the single-endpoint factor `2/lambda` for a fixed support.

**Proved, branch-specific nonattainment:** for data consisting of both endpoints `-1,1` and model restricted to `1<lambda<=2`, with free location and scale, the supremum is 1 and is not attained. Indeed `u^(lambda-1)+(1-u)^(lambda-1)>=1`, so every density is at most `1/sigma`; support containment requires `sigma/lambda>=1`, hence `L<=1/lambda^2<1`. The preceding path approaches 1. This example does not assert nonattainment after adding other shape branches; those must be compared separately.

**Argued:** a fitter could include `1<lambda<2` as a separately constrained branch. Its search must include support-boundary optima (one or both sample extrema on an endpoint), the interior, the lambda-2 uniform alias, and the nonattained lambda-down-to-1 boundary. For fixed limiting support `[a,b]` containing the sample, the latter boundary likelihood is `(2/(b-a))^k * (1/(b-a))^(n-k)`, where k is the total multiplicity at its endpoints. Optimize over feasible supports and compare that supremum with all attained candidates and other escape faces, respecting held parameters. If a finite candidate ties the global supremum it is an MLE; a uniquely superior unattained junction needs a dedicated non-existence status, not `Ok`. Bounds on the remaining shape/location/scale search and exact/toleranced tie policy still need proofs; unresolved cases must return `IterationFailure`. No claim is made that every held-subset problem is solved.

A full unconstrained shape fit remains unbounded as lambda tends to infinity, as already proved in the dossier. Therefore adding the U branch requires a defined upper restriction, not simply removing the current upper bound. The current held-shape path already accepts U shapes, but its numerical search is not globally certified.

> **Proposed Details.** The U-shaped branch `1<lambda<2` can be considered under a separate bounded-shape fit, with endpoint cases included. As lambda approaches 1 from above with a fixed support, the endpoint density tends to twice its value at the lambda-1 uniform member. Its one-sided likelihood boundary must be compared with attained candidates; it is not itself the uniform likelihood. The result depends on the adopted density convention at endpoints. A certified fitter must stop on unresolved comparisons and report an unattained junction when it dominates.
>
> **Fields:** retain the current principal-branch `fit_domain` until that search and all supported held subsets are certified. A future expanded domain requires a junction-limit entry and a dedicated status; no `holds` change is established here.

Reference: no literature source was found establishing this particular endpoint MLE claim in the searches performed. The derivation above is direct.

## 3. Slash: exactly half tied

**Proved sufficient condition:** suppose the largest multiplicity is exactly `n/2`. At a half-mass value v put `S_v=sum_{x_i != v} 1/(x_i-v)`. If its collapse value `B_v` is the largest of the half-mass collapse values and `S_v != 0`, a finite MLE exists. If there are two half-mass values, there are no other distinct data values, their collapse values agree, and the condition always holds. This is an explicit sample check, but does not resolve `S_v=0`.

**Proof.** Put `k=n/2`, `m=v+h`, and let `s` tend to zero. The standard slash density is `c(1-exp(-t))/(2t)` with `t=(x-m)^2/(2s^2)` and `c=(2*pi)^(-1/2)`, multiplied by `1/s`. For `h=d*s^2`, the k tied observations contribute a log correction `-k*d^2*s^2/4+O(s^4)` relative to their central density. The non-tied observations contribute `2*d*s^2*S_v+O(s^4)` relative to their tail factors; their exponential remainders are smaller than any power. Thus

```
log(L(v+d*s^2,s)/B_v) = s^2*(2*d*S_v - k*d^2/4) + O(s^4).
```

Choose `d=4*S_v/k`; the coefficient is `4*S_v^2/k>0`. This finite likelihood is greater than every collapse value by the hypothesis. The likelihood tends to zero for the other escaping paths, so compactness gives an attained maximum. If there are two half-mass points v and w, `B_v=B_w` by symmetry and `S_v=k/(w-v) != 0`.

**Numerical and open:** the zero-reciprocal case cannot be classified by this local test alone. The dossier's sample `0` ten times, `-1` once, `9` nine times has zero S but the finite point `(m,s)=(4,3)` beats collapse; `checks.py` independently evaluates the ratio at 40 decimal digits. The opposite configuration `0,0,-1,1` is a numerical collapse example in the dossier, not proved here. A necessary-and-sufficient closed sample inequality remains open; a proof would need a global bound on the two-dimensional likelihood or a finite exact reduction. A generic root search is not such a condition.

**Fitter consequence:** the reciprocal-sum check proves existence only under the stated comparison of collapse values; it does not locate or certify the global maximum. Return `Unbounded` for more than half tied; an unattained winning collapse should use `CollapseLimit`, and unresolved profile/face comparisons should use `IterationFailure`. Exact equality with a finite global maximizer implies an MLE exists; selecting collapse within a tolerance is a numerical convention.

> **Proposed Details.** At exactly half tied, if a largest-collapse point v has nonzero reciprocal sum `sum_{x_i != v} 1/(x_i-v)`, an interior likelihood strictly exceeds all collapse values, so an MLE exists. In particular this holds for a sample consisting of two distinct values each repeated half the time. A zero reciprocal sum does not decide the question. The general exact criterion remains a comparison of the full interior supremum with the collapse values, and numerical searches do not certify this comparison.
>
> **Fields:** keep `limits` but distinguish exact nonattainment from tolerance-based tie selection; no established `fit_domain` or `holds` change.

Reference: no source was found proving this exact sample criterion. The calculation above is direct.

## 4. Generalized beta: closure versus maximizing escapes

The endpoint `b` is held, all three shapes `p,q,a` are free, and observations lie strictly inside `(0,b)`. Write `Y=-log(X/b)`; then `U=exp(-aY)` is beta(p,q).

**Proved counterexample to completeness as a list of all proper weak limit distributions:** set `a=t`, `p=q=1/t`, and send `t` to infinity. For each y>0,

```
P(Y>y) = P(U<exp(-t*y)) -> exp(-y)/2.
```

To check this, `B(1/t,1/t) ~ 2t`, while the unnormalized incomplete beta integral to `exp(-ty)` is asymptotic to `t*exp(-y)`: on that integration interval `(1-u)^(1/t-1)` tends uniformly to 1. The limiting probability measure is one half a point mass at `Y=0` and one half an exponential distribution of rate one. On the X scale it is a half atom at b plus half the uniform law on `(0,b)`. It is neither the listed unit-gamma limit nor an ordinary power distribution. The dossier already mentions this mixed face in prose, but its `limits` list does not list it. `checks.py` independently evaluates three incomplete-beta probabilities.

This is **weak convergence**, not Hellinger convergence: each finite model assigns zero mass to `{b}`, while the limit assigns one half, so their total variation distance is at least one half and they cannot converge in Hellinger distance. Different closure notions therefore give different catalogues.

**Proved only for this path, not all mixed paths:** for any fixed data strictly inside `(0,b)`, the limiting density at each observation is one half of the corresponding attained uniform density. The path's limiting likelihood is `2^(-n)` times the likelihood at `q=1, p*a=1`, so it cannot be a maximizing sequence. The density convergence follows directly from the beta density: `a/B(p,q)->1/2`, `exp(-ap*y)=exp(-y)`, and `(1-exp(-ay))^(q-1)->1` for fixed y>0; transform back with the common Jacobian. This does **not** answer whether every maximizing escape is dominated by, or converges to, one of the listed faces.

**Conjecture, unproved:** for nonconstant strictly interior data with b held and all shapes free, any escaping sequence whose likelihood tends to the global supremum has a subsequence whose relevant likelihood limit is represented by the unit-gamma or power comparison, or by an already attained member. To prove it one must classify joint asymptotics of `(p,q,a)`, including rates of `log(q)/a`, `pa`, and small-p/small-q mixtures, establish density convergence at every observation rather than weak convergence alone, and rule out collapse or missing mass that could improve the likelihood. Held shapes require separate analysis. A path may have multiple subsequential weak limits; literal convergence of every escaping sequence is too strong if different winning faces tie and one alternates between them.

**Fitter consequence:** retain `UnitGammaBoundary` and `PowerBoundary` when their global superiority is certified; use `IterationFailure` when a candidate or face comparison is unresolved. Do not claim completeness of the maximizing-sequence catalogue on this evidence. The mixed face cannot win on the displayed path, but its general parameterization still needs analysis.

> **Proposed Details.** Besides the gamma and power limits, some joint shape escapes have mixed weak limits with an endpoint atom and an interior continuous component. For example `a=t`, `p=q=1/t` gives half an atom at the held endpoint and half a uniform law. These weak limits are not Hellinger limits of the continuous models. This particular mixed face has a limiting interior-data likelihood strictly below the attained uniform likelihood. Completeness of the face comparisons for all maximizing sequences remains unproved.
>
> **Fields:** add a descriptive mixed-weak-limit entry only if `limits` is intended to catalogue all proper weak limits; do not give it an unconditional fitter detection/status. Keep the existing structural `holds` and `fit_domain` pending separate held-subset proofs.

Reference: no source was found that proves the stated joint maximizing-escape classification. The exhibited path is calculated directly.

## 5. Skellam: joint uniqueness

**Proved reduction:** for a joint interior stationary point, write `s=sqrt(mu1*mu2)>0`, `theta=log(mu1/mu2)/2` and `d=mean(x)`. Strict concavity in theta at fixed s gives the unique theta with `2*s*sinh(theta)=d`; the joint question reduces to the scalar profile derivative

```
  (1/n) sum_i I'_{|x_i|}(2s)/I_{|x_i|}(2s)
       - sqrt(1 + d^2/(4s^2)).
```

Every interior maximum is a zero. The attained positive/negative Poisson faces and `(0,0)` must also be compared. This reduction alone does not prove at most one zero, or exclude ties between a face and an interior point. The held-one-rate strict concavity in the dossier does not imply joint uniqueness.

**Numerical:** the script scans 81 logarithmic equal-rate values for each of three symmetric samples, where `d=0` and the theta optimizer is zero. It records grid-local peaks, not roots or a certificate; it does not cover asymmetric data or a possible face tie. **Conjecture:** the joint global maximum is unique for each finite integer sample. A proof would need a valid monotonicity/single-crossing theorem for the displayed Bessel-ratio combination, together with equality and face cases; alternatively one counterexample with two certified equal global maxima would disprove it. This investigation did not establish either.

**Fitter consequence:** existence on the closed nonnegative-rate quadrant is supported by the dossier's coercivity argument, but existence does not certify the numerical maximum. Return `Ok` for a certified interior or attained face maximizer, otherwise `IterationFailure`; do not claim uniqueness on the strength of a grid or a score root.

> **Proposed Details.** The joint likelihood has a finite maximizer on the closed nonnegative-rate quadrant. Profiling the rate ratio leaves a one-dimensional Bessel-ratio equation. Uniqueness of the joint maximizer, including possible ties with attained Poisson faces, remains unproved; a finite grid is not a certificate.
>
> **Fields:** no `limits`, `fit_domain`, or `holds` change follows.

Reference for the distribution, not the uniqueness conjecture: J. G. Skellam (1946), “The Frequency Distribution of the Difference between Two Poisson Variates Belonging to Different Populations,” *Journal of the Royal Statistical Society* 109(3), 296, [doi:10.1111/j.2397-2335.1946.tb04670.x](https://doi.org/10.1111/j.2397-2335.1946.tb04670.x). No reference proving joint uniqueness was found.

## 6. Discretized gamma: profiled shape

**Proved reduction, not unimodality:** for a fixed shape `a`, the fitter profiles the rate `r` by a unique inner score root when the sample has a positive count. The pmf is `P(k<=Y<k+1)` for `Y~Gamma(a,r)`; there is no free location. The resulting profile `P(a)=max_{r>0} sum_i log P(x_i<=Y<x_i+1)` has an attained maximum for sample range at least two by the dossier's compactness argument. Existence and the unique inner maximizer do not establish a unique outer mode.

**Numerical:** `checks.py` profiles the rate by logarithmic bisection and evaluates 17 shapes `exp(i/3)`, `-8<=i<=8`, for three small samples with range at least two. A local grid peak is neither a proved stationary point nor a global certificate; modes narrower than the mesh and behavior beyond its edges can be missed. **Conjecture:** this profile is unimodal for every finite sample with range at least two. We found no proof or counterexample. To prove it, one would need a global single-crossing or suitable concavity result after rate profiling for gamma probabilities over the unequal log-cell intervals; joint concavity of the underlying ungrouped exponential-family likelihood cannot simply be transferred through interval integration when shape and rate both vary.

**Fitter consequence:** `PointMass` and `TwoPointLimit` are justified for the stated sample configurations, but a finite mesh and local refinements do not certify `Ok` for the remaining samples. A certified interval bound, or a proof of unimodality with a certified root bracket, is needed; unresolved outer search should yield `IterationFailure`. Held-rate and held-shape fits are distinct questions.

> **Proposed Details.** With both parameters free and sample range at least two, a finite maximum exists. The rate profile is uniquely solved at each positive shape, but unimodality or uniqueness of the outer shape profile has not been proved. The current mesh and refinements are numerical evidence, not a global-search certificate.
>
> **Fields:** retain the existing limit and hold declarations; no `fit_domain` change.

Reference: no reference proving this discretized-gamma profile unimodality was found in the searches performed.

## Reproduction and scope

Run from the repository root, with a single CPU slot and temporary files inside this worktree:

```sh
mkdir -p build/tmp build/scratch
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 MKL_NUM_THREADS=1
/tmp/umvl-cpu/run python3 dev/research/evaluations/open-existence/checks.py
/tmp/umvl-cpu/run python3 dev/research/evaluations/open-existence/verify.py
```

Both scripts are deterministic and use no random numbers, so no seed is applicable. It uses Python 3.14.5 and mpmath 1.3.0 at 40 decimal digits. Its numerical grids and finite checks do not prove unimodality, uniqueness, global optimality, or the truth of any universal statement; analytical proofs above do not depend on their outputs. No C++ or R suite is required for this research-only task. Literature searching was targeted, not a systematic review; absence statements mean no supporting reference was found, not that none exists.

**Recorded run (31 October 2026):** both commands exited 0. `verify.py` passed all four assertions. The slash witness ratios were `1.45605346346`, `1.00015892754`, and `1.06613262632`; the generalized-beta tail probabilities at t = 10, 100, 1000 were `0.186602858201`, `0.183969544518`, `0.183940022713`, approaching `0.183939720586`. The three beta-binomial point-to-face ratios were `1.77777777778`, `10.4853700944`, and `0.197530864198`. Each of the three discretized-gamma profiles and each of the three symmetric Skellam profiles had one grid-local peak. These are floating-point numerical checks, not interval-certified bounds or universal proofs. The gamma inner bisections used 65 iterations on the stated fixed bracket; the scans do not bound the regions between or beyond the grid points.

## Review notes (Claude, 31 October 2026)

- **Beta-binomial, practical sufficient condition.** The "exact decision" in
  section 1 is the definition of existence plus the compactness argument; its
  value is that argument (the only positive-likelihood boundary with a
  non-endpoint observation is the binomial face $s = 0$). The usable
  consequence is the score on that face. With $p_0 = T/(nN)$, $\bar x = T/n$
  and the biased sample variance $S^2 = n^{-1}\sum_i (x_i - \bar x)^2$,

  $$
  \left.\frac{\partial}{\partial s} \log L(p_0, s)\right|_{s=0}
  = \frac{n\,\{S^2 - N p_0 (1 - p_0)\}}{2 p_0 (1 - p_0)} .
  $$

  (Differentiate the rising-factorial form term by term; checked numerically
  on 2000 random samples, $N \le 8$, no mismatch.) Hence **overdispersion,
  $S^2 > N p_0(1-p_0)$, implies that a finite MLE exists** (proved: the
  likelihood rises into the interior above the face supremum $B$, and the
  compactness argument gives attainment). Necessity for $N \ge 3$ is open:
  an underdispersed sample whose profile dips and then rises above $B$ would
  be a counterexample. See the scan below.
- **Necessity scan (numerical).** `open-existence/bbinom_necessity.py`
  enumerates three-point samples $\{0^a, m^b, N^c\}$, $3 \le N \le 10$,
  $1 \le m < N$, $0 \le a, c \le 8$, $1 \le b \le 8$, keeps the 1,778
  underdispersed ones, and maximises the log-likelihood ratio to the
  binomial face over a grid ($p = i/30$, $\log s = k/3$, $-24 \le k < 24$).
  None exceeds the face. **Conjecture:** for fixed size, a finite MLE with
  both shapes free exists if and only if the sample is overdispersed
  ($S^2 > N p_0 (1 - p_0)$), apart from the endpoint-only escapes. If true,
  the current tolerance-based `BinomialLimit` detection could be replaced by
  this exact test; until then the overdispersed case can return `Ok`
  without a face comparison, and only underdispersed or equidispersed
  samples need the numerical comparison.
- The other sections stand as graded. New results worth carrying into the
  dossiers: the slash half-tied sufficient condition (section 3), the
  Tukey lambda U-branch non-attainment and endpoint jump (section 2), the
  generalised beta mixed weak limit (section 4, already in prose). dgamma
  and Skellam remain conjectures with reductions.
