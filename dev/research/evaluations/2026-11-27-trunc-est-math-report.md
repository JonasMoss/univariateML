# Maximum likelihood with estimated truncation bounds

TASK-448, source revision `0838570e`. Each of the 30 reviewed family and held-mask combinations is treated under all three estimated-bound designs. The conditional likelihood uses the repository's closed-window convention.

## Correction to the known-window transfer

**Proved.** The estimated-bound paragraph of the [known-window report](2026-11-27-trunc-expfam-math-report.md#all-applicable-truncateestimated) requires one common parent density or mass function at the compared windows. The repository's positive-truncation extension changes the admissible law at exactly `[1, Inf]`. For the exact sample `(1,2)`, held NB size zero and `truncate = c(1, NA)`, the tight window is `[1,2]`. Every size-zero ordinary NB law gives this event probability zero. Taking the estimated upper bound to infinity instead gives the admitted logarithmic law, with a finite unique probability fit. Thus optimizing only at the sample maximum can miss an attained joint MLE if the alias is admitted as part of the design space. The same failure occurs with sample `(2,3)`, held size zero and `truncate = c(NA, Inf)`: the tight window is `[2,Inf]`, and a finite looser lower cut at one gives the logarithmic fit. This second example needs only finite estimated cuts. The issue also affects cardinality when a tight fit exists. For Poisson data `(1)` and the same bound specification, every positive rate on `[1,1]` attains, and rate zero on `[1,Inf]` is an additional maximizer.

The known-window review already corrected the held-probability binomial tail claim for singleton windows. With sample `(2)`, held probability `1/2` and window `[2,2]`, every size at least two gives likelihood one, including as size tends to infinity. The corrected nonsingleton tail is used below. [Independent review](2026-11-27-mod-rev1-report.md#proof-errors-and-grading-corrections)

## Joint profiling lemma

**Proved.** Let $m=\min x_i$, $h=\max x_i$ and let $A_*$ be the nominal window with each estimated cut set to its respective extreme. A candidate window must contain every observation. Thus $A_*\subseteq A$ before and after intersecting with the support $S_\theta$, for **each** fixed $\theta$, even when the support depends on $\theta$. Write $C_\theta=\prod_i f_\theta(x_i)$, with the parent convention at support endpoints. On a compatible positive-likelihood slice,
$$
 L(\theta,A)=C_\theta P_\theta(A)^{-n}
 \le C_\theta P_\theta(A_*)^{-n}=L(\theta,A_*).
$$
The proof uses the same numerator on both windows and requires $P_\theta(A_*)>0$. Taking suprema over coefficients in both directions, since $A_*$ itself is feasible, gives equality of the joint and tight-window suprema. If the supremum is attained, its complete joint maximizing set consists of all tight-window maximizing coefficients paired with every admissible looser window satisfying $P_\theta(A\setminus A_*)=0$. In particular a coefficient escape on a positive-mass tight window cannot become an attained ordinary fit by loosening a cut. A sequence witnessing the tight-window supremum already witnesses the joint supremum. Bound sequences can also wander on zero-mass plateaus and need not have uniquely determined limits.

For a continuous law positive throughout its support interior, a lower cut can tie strictly below $m$ only when $m$ is the support endpoint. The corresponding upper statement uses $h$. On an ordinary count law with positive mass at each retained integer and at the adjacent excluded integer, an estimated lower cut ranges over $(m-1,m]$ and an estimated upper cut over $[h,h+1)$. If the adjacent count is outside the parent support, the corresponding plateau extends indefinitely. Equivalently use the zero-removed-mass condition above, which also works for parent atoms and parameter-dependent supports. Every ordinary count estimate in this report therefore has nonunique **nominal real bounds**. The CSV cardinality claims cover both the parent coefficients and this distinction. A representative at the integer extreme is canonical. Repeated extreme observations impose exactly the same cut constraints as one copy.

If the continuous tight event has zero width at a constant sample $c$, hold any compatible finite coefficient with positive density at $c$ and positive carrier on a side into which the design permits expansion. As compatible intervals shrink to $c$, $P_\theta(A)\to0$ with unchanged positive numerator. In the families here $L^{1/n}$ is asymptotic to inverse effective width at such a coefficient. All conditional distributions on the shrinking intervals converge weakly to $\delta_c$. There is no admissible zero-width density. If no compatible positive-mass expansion exists, the design is invalid. Closed count bounds can coincide and give a genuine atom at $c$. A zero-probability conditioning event for a parent atom is invalid even on a nominal interval of positive length. A zero observation density throughout the parameter domain gives no positive-likelihood fit, so the division argument supplies no strictness there.

All results below require valid holds, observations and fixed cuts. Beta observations must be in $(0,1)$. Gamma, inverse Gaussian, lognormal, inverse gamma, inverse Weibull and Weibull observations must be strictly positive. An estimated cut cannot admit an observation on an open support end. Exponential observations may equal zero, with positive density there and a lower-bound plateau through every nominal cut at or below zero. Rayleigh has density zero at zero for every scale. With an observed zero its likelihood is identically zero on every valid window. Every such parameter ties at zero on the literal likelihood scale, so there is no positive-likelihood fit or strict bound profile. The Pareto endpoint is positive and included with positive density, subject to the endpoint analysis below. Count observations must be on the stated lattice within the retained support. The criteria concern positive likelihood.

## Continuous likelihood arguments

**Proved.** Write the profiled effective interval as $[a,b]$, where $a$ takes the maximum of the nominal lower cut and the support lower endpoint, and $b$ takes the minimum on the upper side. On positive width, a monotone transformation changes log likelihood only by its sample Jacobian. For each scalar row below use a tilt $e^{tT}$ over the stated carrier. On the interior of the tilt domain the score is $\bar T-E_t T$ and the derivative is $-\operatorname{Var}_t T<0$. This follows by differentiation on a neighborhood of each tilt where the exponential moments are finite. Thus its unique root exists iff the observed statistic lies strictly between the two limiting model means. A finite integrable endpoint law with the score pointing out of the domain, including equality, wins by concavity. A negatively tilted statistic at its essential minimum, or positively tilted statistic at its maximum, gives unbounded likelihood if all data are at that extremum and its carrier mass is zero. Dividing the normalizer by the exponent at the extremum and applying dominated convergence proves this. The normalized law concentrates at the unique extremizer in the rows below, by the exponential weight gap away from it. The same comparisons prove convergence of the means to the essential extrema at infinite tilt. For the finite tilt ends below with divergent statistic means, the divergence follows from the carrier integrals indicated in the family rules.

For compact windows, let $P_k(a,b)$ denote the mean of the density proportional to $x^{k-1}$ for $k>0$, and $P_1=(a+b)/2$. Put $P_k(a,\infty)=\infty$. Integration gives $P_k=\frac{k}{k+1}(b^{k+1}-a^{k+1})/(b^k-a^k)$ on finite windows. The negative tilt in $x$ has score range $(a,P_k)$ against this carrier. On unbounded windows the mass in every bounded interval tends to zero at zero rate because the carrier integral diverges. On a positive-width interval with an exponential or power transform, equality with the low transformed end requires a constant sample. For inverse gamma and inverse Weibull apply the rule with lower transformed cut $1/b$ or $b^{-k}$, interpreting infinity as mapping to zero, and with upper transformed cut from $a$. The pulled-back zero-rate densities are $x^{-\alpha-1}$ and $x^{-k-1}$, respectively.

**Two free coordinates.** In natural coordinates normal has quadratic coefficient $<0$. Gamma has shape and rate positive against $dx/x$, beta has both shapes positive against $dx/[x(1-x)]$, and inverse Gaussian has $(u,v)=(\lambda/(2\mu^2),\lambda/2)>0$ against $x^{-3/2}dx$. All have strictly concave log likelihood on positive width because no nonzero linear combination of their two statistics is constant there. For nonconstant data the likelihood is coercive along the closure of each allowed natural domain. In every unit escape direction the leading tilt is a nonconstant concave quadratic or linear function for normal, $s\log x-tx$ for gamma, $s\log x+t\log(1-x)$ for beta, or $-sx-t/x$ for inverse Gaussian, with $s,t\ge0$ in the last three cases. Every such tilt is strictly monotone or has a single maximum. The sample average is strictly below its supremum even when both extreme observations are at the cuts. Hence some compact positive-carrier interval improves on the empirical tilt by a positive margin. Applying this bound in a neighborhood of each unit direction and taking a finite cover gives uniform coercivity. Fatou gives upper semicontinuity of the extended likelihood with value $-\infty$ at divergent integrals. An integrable finite edge is approachable along a segment from the interior by log-convexity and Fatou, so it supplies a genuine supremum. Dominated convergence of the minimum of the densities gives total variation convergence to its normalized law. For constant data each family has a feasible tilt with unique peak at the sample point, even at a retained endpoint, and multiplying the tilt yields divergence and a weak atom by the scalar argument. In normal coordinates take $-(x-c)^2$, in gamma $\log x-x/c$, in beta $c\log x+(1-c)\log(1-x)$ and in inverse Gaussian $-x-c^2/x$.

The following tests completely specify the finite faces used in the regime statements. On each eligible noncorner face solve its tangential moment equation at a strictly interior remaining coefficient. Skip the face if that equation has no root. Then check the inward score $\le0$. At a corner check both inward scores. Necessity follows by feasible coordinate changes, sufficiency by concavity and Jensen's tangent inequality. Infinite inward derivatives count with their signs by monotone convergence. For example when the inverse Gaussian Lévy face has infinite $EX$, the inward rate score is $+\infty$. The indicated integrability restrictions account for every finite boundary. If none wins, the unique maximum is interior.

| Family | Finite candidate | Eligibility and test |
| --- | --- | --- |
| Normal | Zero quadratic coefficient, linear tilt $e^{rx}$ | On a bounded interval match $\bar x$ with a signed truncated exponential, uniform at $r=0$. On a lower-only interval use the translated exponential with mean distance $\bar x-a$ and on an upper-only interval its reflection with distance $b-\bar x$. There is no integrable face on the full real line. In each eligible case the face wins iff sample variance (divisor $n$) is at least the face variance. This is the inward second-moment sign after mean matching. |
| Gamma | Shape zero, rate $v>0$ | $a>0$. Match $\bar x$ in $x^{-1}e^{-vx}$ and require $\overline{\log x}\le E\log X$. |
| Gamma | Rate zero, shape $k>0$ | $b<\infty$. Match $\overline{\log x}$ in $x^{k-1}$ and require $\bar x\ge EX$. |
| Gamma | Both zero | $a>0$, $b<\infty$. In $dx/x$ require $\overline{\log x}\le E\log X$ and $\bar x\ge EX$. |
| Beta | Shape 1 zero or shape 2 zero | Cut away zero or one respectively. In $x^{-1}(1-x)^{\beta-1}$ or $x^{\alpha-1}(1-x)^{-1}$ match the statistic for the positive shape. The empirical mean of the missing log statistic must be at most its face mean. |
| Beta | Both zero | Cut away both endpoints. In $dx/[x(1-x)]$ both empirical log means must be at most the respective face means. |
| Inverse Gaussian | $u=0, v>0$ | The truncated Lévy density $x^{-3/2}e^{-v/x}$ matches $\overline{1/x}$ and requires $\bar x\ge EX$. Infinite $b$ rules it out. |
| Inverse Gaussian | $v=0, u>0$ | $a>0$. In $x^{-3/2}e^{-ux}$ match $\bar x$ and require $\overline{1/x}\ge E(1/X)$. |
| Inverse Gaussian | Both zero | $a>0$, $b<\infty$. In $x^{-3/2}$ require both empirical moments to be at least their corresponding means. |

For the scalar holds with finite zero-tilt faces the following integral details are useful. Normal at held mean tilts $(x-\mu)^2$ negatively and has uniform boundary on compact intervals. Its lower mean is the squared distance from $\mu$ to the interval and its upper mean is infinite on an unbounded interval by mass leaving every compact set. Normal at held SD tilts $x$ by the full real line against a Gaussian carrier. Its mean ranges strictly between the cuts. Both proofs apply to logged lognormal data, where linear-tilt faces pull back to powers and the uniform pulls back to log-uniform.

Gamma at held scale uses positive shape tilt on $\log x$ against $x^{-1}e^{-vx}dx$ for $v=1/\mathrm{scale}$. The lower mean is the zero-shape carrier mean if $a>0$ and $-\infty$ otherwise, by mass concentrating at zero with exponentially damped upper tail. Its upper mean is $\log b$. With one beta shape held, use a positive tilt on $\log x$ for free shape 1, or $\log(1-x)$ for free shape 2, keeping the other shape in the carrier. The lower mean is the mean of that statistic under the zero-free-shape carrier when the corresponding singular endpoint is cut away. Otherwise the mass concentrates at the uncut singular endpoint and the log mean tends to $-\infty$, since the integrated mass diverges there and the statistic is bounded above. The upper mean is respectively $\log b$ or $\log(1-a)$. Both are attained as score-range endpoints only in the limiting sense.

Inverse Gaussian with held shape has a negative $x$ tilt on the Lévy carrier with $v=\lambda/2$, hence thresholds $a$ and the Lévy mean, infinite when $b=\infty$. With held mean use negative tilt on $T=x/\mu^2+1/x$. The low threshold is $T$ at the projected mean, since its derivative changes sign only at $\mu$. The high threshold is the carrier mean for cuts $a>0,b<\infty$, otherwise infinite. If $a=0$ the mass tends to zero outside every neighborhood of zero at zero tilt, and $1/x$ grows without bound there. If $a>0$ and $b=\infty$, the normalized carrier has infinite first moment and Fatou gives the same infinite threshold.

## Pareto endpoint and count exceptions

**Proved.** Pareto density is proportional to $x^{-k-1}$ on its support $x\ge b_0>0$, where $b_0$ is the repository's coefficient `b` and $k$ is `a`. In the conditional density the factor $k b_0^k$ cancels. For a free endpoint, holding every other coordinate fixed, increasing $l=\max(a_{\rm nominal},b_0)$ up to $m$ strictly improves likelihood whenever it changes $l$ on a positive-width interval. With no estimated lower cut, the maximizing endpoints are exactly $b_0=m$ if the fixed cut is below $m$, and $0<b_0\le m$ if it equals $m$. With an estimated lower cut, the complete locus is $0<b_0\le m$, nominal lower cut $\le m$, and $\max(a_{\rm nominal},b_0)=m$. With endpoint held at $m$, the estimated lower cut is flat down to $-\infty$. At the optimal effective cut, apply the exponential tilt to $\log X$, with upper cut from the window and rate $k$, or evaluate it if $k$ is held. If the upper cut equals $m=h$, collapse can use a descending free lower cut or ascending free upper cut, or a parent endpoint tending up to $m$ where the lower cut is fixed strictly below it. With the endpoint held at a fixed coinciding upper cut and only the lower cut estimated, every window has zero parent mass.

**Ordinary count model.** Use $L=\max(s_0,\lceil a_{\rm nominal}\rceil)$ and $U=\lfloor b_{\rm nominal}\rfloor$, with $s_0=0$ except zeta where it is 1. Binomial size $N$ caps the upper count at $V=\min(N,U)$. For a nonsingleton fixed carrier the scalar log-odds tilt gives a strictly increasing conditional mean, with its range and limits obtained by finite sums or domination of the extremal terms. Poisson and binomial allow every real natural tilt. A finite unbounded-side mean threshold for Poisson cannot arise as its rate tends to infinity, since the weight ratio of any higher fixed count to a lower one tends to infinity. Geometric and positive-size NB have tilt $\log(1-p)<0$. In finite windows their upper thresholds are the mean of uniform and of the rising-factorial carrier weights, respectively. In unbounded windows the sums of those weights diverge by the geometric or binomial series, so as $p\downarrow0$ mass leaves every bounded set and the means diverge. Each lower threshold is $L$, with a weak and total variation atom there. The upper Poisson/binomial atom on a finite window has the same convergence as the lower one. A finite zero-tilt count face is a proper conditional law with pointwise convergence and convergence in total variation by normalization. A singleton admits every compatible parent coefficient and yields likelihood one. Binomial atoms are at zero or at $N$ and Poisson, geometric and NB ordinary atoms at zero. An NB with size zero is always the zero atom in ordinary windows.

**Declared alias.** The separate `[1, Inf]` model is available when any fixed lower cut is exactly 1 and any fixed upper cut is infinity, and $m\ge1$. It adds an atom at 1 at Poisson rate zero, geometric probability one, binomial probability zero with size at least one, and NB probability one at any allowed size. At NB size zero and $0<p<1$ it also has the logarithmic law with weights $(1-p)^j/j$, $j\ge1$. The regular alias law agrees with conditioning the positive parent. Thus any regular alias fit is dominated by ordinary tightening and can tie only on its mass plateau. The extra atoms are compatible only for all ones and then attain one. For positive-size NB, Poisson, geometric and binomial with the reviewed free shape or probability, a compatible extra atom can either supply the unique coefficient on the resolved alias or add a coefficient-window pair when tighter ordinary windows are allowed. The binomial held-probability-zero and NB size-zero cases can have no compatible ordinary fit even though the looser alias fits. In the latter case the log-series score has full mean range $(1,\infty)$ by the diverging harmonic series at $p=0$ and the dominant first term at $p=1$. This establishes a unique positive-probability fit for every positive sample, with $p=1$ at all ones. The joint bounds for these alias-only fits are exactly `[1, Inf]`. Joint cardinality is obtained by taking the union of the ordinary pairs from the profiling lemma and these alias-only pairs. In the text below when the tight window itself equals the alias, apply its extensions directly.

**Free binomial size.** For nonconstant data every $N\ge h$ has a positive-width count support and exactly one fitted interior probability by mean matching. Compare its likelihood with the same-window Poisson likelihood at the matched mean. The binomial profile tends to the Poisson value as $N\to\infty$. With rescaled odds fixed, the binomial weights converge termwise to the Poisson weights and are bounded by them, including the first moment. Bracketing the unique Poisson root bounds the rescaled odds of the binomial roots and gives convergence. If any finite slice equals or exceeds the face it follows that some finite slice attains, because a strict witness leaves a finite prefix of potential winners. Otherwise the supremum escapes to the Poisson. The exact set is all winning integer sizes paired with their matched probabilities. The [finite comparison proof](2026-11-25-tbinom-math-report.md#a-finite-decision-certificate) supplies a tail decision and equality comparison on bounded or positive lower-only windows, and the ordinary binomial criterion applies on $[0,\infty)$, where the sample variance must be strictly below its mean. On $[1,\infty)$ a strict sign of the factorial-moment comparison also decides attainment [by the positive-binomial result](2026-11-25-tbinom2-math-report.md). If $U=L+1$ and both counts occur, every $N\ge h$ attains the empirical Bernoulli maximum by matching the odds. If $h=L+1<U$, only $N=h$ attains that bound, since every larger size also puts mass above $h$.

For constant data at $c$, the free-binomial ordinary maximizing set comprises $(N,p)=(c,1)$, all $(N,0)$ for $c=0$, and all $N\ge c$ and $0<p<1$ such that $L=\min(N,U)=c$. Include the alias pairs when eligible. This follows because likelihood one requires the conditional law to be an atom at $c$. With fixed interior $p$, constant data with $L=c$ attain at $N=c$, and at every larger $N$ if $U=c$. With $L<c$ and $U=c$ no finite size attains and $N\to\infty$ gives the top atom. In the remaining cases $L<c$ and either some observation lies below finite $U$ or $U=\infty$. The likelihood tends to zero: consecutive retained ratios below finite $U$ go to infinity, or in the unbounded case the probability of each observed count tends to zero while the conditioning probability tends to one. The same proof works for any nonconstant sample. These arguments bound the maximizing sizes by a finite prefix using a witness and the ratio bound or a binomial tail bound. At held $p=0$ every ordinary size is optimal for all zeros and no ordinary size fits positive data. At held $p=1$ the only compatible size is the constant count. Incompatible endpoint holds may admit a positive-probability event but yield zero likelihood. The alias exception at $p=0$ follows the preceding paragraph.

**Zeta scope.** On any finite $[L,U]$ a signed-exponent model is mathematically well defined, with unique nonconstant-data tilt by $-s\log j$ and with both excluded endpoint atoms. The declared signed extension and implementation cover only effective $L=1$. If only the parent domain $s>1$ is admitted on a finite window, replace the upper log-mean threshold by the mean under weights $1/j$ at $s=1$. For a nonsingleton the endpoint law at $s=1$ is then excluded, including at equality. On unbounded $[L,\infty)$ and $s>1$, the upper log-mean limit is infinite, since the harmonic sum diverges and mass leaves each bounded set as $s\downarrow1$. The lower limit as $s\to\infty$ is $\log L$, by scalar tilt dominance. On a singleton every admissible exponent attains. In the declared signed *submodel*, if a free lower cut resolves to $m>1$, the tight window within that submodel still starts at 1. It uses the same tests with $L=1$ and an estimated upper cut at $h$ if present. The three scopes are the parent domain, a signed extension to any finite window, and the declared route.

## Regime statements

Each entry is **proved** by the indicated scalar, two-coordinate or count argument above applied to its displayed window. For every continuous entry the coincident-cut rule applies before any positive-width test. Coefficient and likelihood conclusions concern exact arithmetic.

### beta free truncate:lower-estimated

Here the lower cut is the minimum $m$ and the upper cut is fixed.

**Criterion.** Nonconstant data attain iff none of the eligible beta faces passes the moment tests. The shape-one-zero edge is eligible. The shape-two-zero edge and corner require a fixed upper cut below 1.

**Failure.** The winning excluded law is the corresponding zero-shape edge or the carrier corner. On positive width a constant sample collapses to its observed point, with unbounded likelihood. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### beta free truncate:upper-estimated

Here the upper cut is the maximum $h$ and the lower cut is fixed.

**Criterion.** Nonconstant data attain iff none of the eligible beta faces passes the moment tests. The shape-two-zero edge is eligible. The shape-one-zero edge and corner require a fixed lower cut above 0.

**Failure.** The winning excluded law is the corresponding zero-shape edge or the carrier corner. On positive width a constant sample collapses to its observed point, with unbounded likelihood. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### beta free truncate:both-estimated

Here the cuts are the minimum $m$ and maximum $h$.

**Criterion.** Nonconstant data attain iff none of the eligible beta faces passes the moment tests. Both zero-shape edges and their corner are eligible.

**Failure.** The winning excluded law is the corresponding zero-shape edge or the carrier corner. On positive width a constant sample collapses to its observed point, with unbounded likelihood. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### beta shape1 truncate:lower-estimated

Here the lower cut is the minimum $m$ and the upper cut is fixed.

**Criterion.** On positive width, the empirical mean of log(1-X) must lie strictly between the zero-free-shape threshold and log(1-a). The lower threshold is finite iff the fixed upper cut is below 1.

**Failure.** At or below a finite lower threshold the zero-free-shape beta carrier wins and is excluded. Every constant sample hits the upper statistic threshold at the estimated lower cut. At that upper equality the likelihood diverges to a weak atom. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### beta shape1 truncate:upper-estimated

Here the upper cut is the maximum $h$ and the lower cut is fixed.

**Criterion.** On positive width, the empirical mean of log(1-X) must lie strictly between the zero-free-shape threshold and log(1-a). The lower threshold is finite because the singular end is cut.

**Failure.** At or below a finite lower threshold the zero-free-shape beta carrier wins and is excluded. The upper statistic equality requires a constant sample at the fixed lower cut. At that upper equality the likelihood diverges to a weak atom. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### beta shape1 truncate:both-estimated

Here the cuts are the minimum $m$ and maximum $h$.

**Criterion.** On positive width, the empirical mean of log(1-X) must lie strictly between the zero-free-shape threshold and log(1-a). The lower threshold is finite because the singular end is cut.

**Failure.** At or below a finite lower threshold the zero-free-shape beta carrier wins and is excluded. Every constant sample hits the upper statistic threshold at the estimated lower cut. At that upper equality the likelihood diverges to a weak atom. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### beta shape2 truncate:lower-estimated

Here the lower cut is the minimum $m$ and the upper cut is fixed.

**Criterion.** On positive width, the empirical mean of log X must lie strictly between the zero-free-shape threshold and log b. The lower threshold is finite because the singular end is cut.

**Failure.** At or below a finite lower threshold the zero-free-shape beta carrier wins and is excluded. The upper statistic equality requires a constant sample at the fixed upper cut. At that upper equality the likelihood diverges to a weak atom. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### beta shape2 truncate:upper-estimated

Here the upper cut is the maximum $h$ and the lower cut is fixed.

**Criterion.** On positive width, the empirical mean of log X must lie strictly between the zero-free-shape threshold and log b. The lower threshold is finite iff the fixed lower cut is above 0.

**Failure.** At or below a finite lower threshold the zero-free-shape beta carrier wins and is excluded. Every constant sample hits the upper statistic threshold at the estimated upper cut. At that upper equality the likelihood diverges to a weak atom. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### beta shape2 truncate:both-estimated

Here the cuts are the minimum $m$ and maximum $h$.

**Criterion.** On positive width, the empirical mean of log X must lie strictly between the zero-free-shape threshold and log b. The lower threshold is finite because the singular end is cut.

**Failure.** At or below a finite lower threshold the zero-free-shape beta carrier wins and is excluded. Every constant sample hits the upper statistic threshold at the estimated upper cut. At that upper equality the likelihood diverges to a weak atom. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### binom free truncate:lower-estimated

Here the lower cut is the minimum $m$ and the upper cut is fixed.

**Criterion.** Constants attain. For nonconstant data mean-match each finite size N at least h on the retained window. Attainment holds iff at least one finite profile equals or exceeds the matching Poisson face. Here L=m; use the positive lower-only comparison when U is infinite and m>0.

**Failure.** For nonconstant data a strictly winning same-window Poisson is the excluded face.

**Uniqueness.** For nonconstant data all winning sizes and their unique mean-matched probabilities maximize. If U=L+1 every size wins, whereas if h=L+1<U only size h wins. Constants have the atom and singleton union specified above. Nominal bounds have the mass plateaus and any eligible alias pairs.

### binom free truncate:upper-estimated

Here the upper cut is the maximum $h$ and the lower cut is fixed.

**Criterion.** Constants attain. For nonconstant data mean-match each finite size N at least h on the retained window. Attainment holds iff at least one finite profile equals or exceeds the matching Poisson face. Here U=h; use the bounded comparison.

**Failure.** For nonconstant data a strictly winning same-window Poisson is the excluded face.

**Uniqueness.** For nonconstant data all winning sizes and their unique mean-matched probabilities maximize. If U=L+1 every size wins, whereas if h=L+1<U only size h wins. Constants have the atom and singleton union specified above. Nominal bounds have the mass plateaus and any eligible alias pairs.

### binom free truncate:both-estimated

Here the cuts are the minimum $m$ and maximum $h$.

**Criterion.** Constants attain. For nonconstant data mean-match each finite size N at least h on the retained window. Attainment holds iff at least one finite profile equals or exceeds the matching Poisson face. Here U=h; use the bounded comparison.

**Failure.** For nonconstant data a strictly winning same-window Poisson is the excluded face.

**Uniqueness.** For nonconstant data all winning sizes and their unique mean-matched probabilities maximize. If h=m+1, every size wins. Constants have the atom and singleton union specified above. Nominal bounds have the mass plateaus and any eligible alias pairs.

### binom prob truncate:lower-estimated

Here the lower cut is the minimum $m$ and the upper cut is fixed.

**Criterion.** At interior held probability every compatible sample attains. Constants attain because L equals the observed count. At probability zero require all zeros or all ones with the alias available. At probability one require constant data.

**Failure.** Incompatible endpoint holds have no positive likelihood.

**Uniqueness.** With interior held probability a constant has maximizing size h only, unless U=h, in which case every size at least h wins.  Otherwise compare the exact finite prefix for all maximizing sizes. At probability zero all compatible sizes maximize, and probability one forces size h. Nominal bounds follow the mass rule, with only the alias window for positive data at probability zero.

### binom prob truncate:upper-estimated

Here the upper cut is the maximum $h$ and the lower cut is fixed.

**Criterion.** At interior held probability every compatible nonconstant sample attains and a constant attains iff L equals h. At probability zero require all zeros or all ones with the alias available. At probability one require constant data.

**Failure.** At interior held probability a constant at U=h>L gives the excluded top atom as size tends to infinity. Incompatible endpoint holds have no positive likelihood.

**Uniqueness.** An attained constant with interior probability has every size at least h. Otherwise compare the exact finite prefix for all maximizing sizes. At probability zero all compatible sizes maximize, and probability one forces size h. Nominal bounds follow the mass rule, with only the alias window for positive data at probability zero.

### binom prob truncate:both-estimated

Here the cuts are the minimum $m$ and maximum $h$.

**Criterion.** At interior held probability every compatible sample attains. Constants attain because L equals the observed count. At probability zero require all zeros or all ones with the alias available. At probability one require constant data.

**Failure.** Incompatible endpoint holds have no positive likelihood.

**Uniqueness.** With interior held probability a constant has maximizing size h only, unless U=h, in which case every size at least h wins.  Otherwise compare the exact finite prefix for all maximizing sizes. At probability zero all compatible sizes maximize, and probability one forces size h. Nominal bounds follow the mass rule, with only the alias window for positive data at probability zero.

### binom size truncate:lower-estimated

Here the lower cut is the minimum $m$ and the upper cut is fixed.

**Criterion.** Require held N at least h. A compatible singleton attains. Every nonconstant sample then has a unique interior probability. A nonsingleton constant is at the lower cut and attains only at zero or at one on the alias.

**Failure.** A nonsingleton excluded lower atom away from zero and the alias.

**Uniqueness.** One parent coefficient on a nonsingleton when a root or admitted atom wins. On a singleton every compatible parent coefficient attains. Ordinary nominal bounds have the mass plateaus of the profiling lemma. Include the alias pairs described above.

### binom size truncate:upper-estimated

Here the upper cut is the maximum $h$ and the lower cut is fixed.

**Criterion.** Require held N at least h. A compatible singleton attains. Every nonconstant sample then has a unique interior probability. A nonsingleton constant is at the upper cut and attains iff its value equals N.

**Failure.** A nonsingleton excluded upper atom when the top observed count is below N.

**Uniqueness.** One parent coefficient on a nonsingleton when a root or admitted atom wins. On a singleton every compatible parent coefficient attains. Ordinary nominal bounds have the mass plateaus of the profiling lemma. Include the alias pairs described above.

### binom size truncate:both-estimated

Here the cuts are the minimum $m$ and maximum $h$.

**Criterion.** Require held N at least h. A compatible singleton attains. Every nonconstant sample then has a unique interior probability. Every constant gives a compatible singleton.

**Failure.** No escape on compatible data.

**Uniqueness.** One parent coefficient on a nonsingleton when a root or admitted atom wins. On a singleton every compatible parent coefficient attains. Ordinary nominal bounds have the mass plateaus of the profiling lemma. Include the alias pairs described above.

### exp free truncate:lower-estimated

Here the lower cut is the minimum $m$ and the upper cut is fixed.

**Criterion.** On positive width, the mean of X must strictly exceed the lower endpoint of the increasing transform of [a,b] and be strictly below the transformed midpoint. The lower transformed cut is an observed extreme, so equality there occurs exactly for constant samples. Use the infinite threshold if the upper transformed endpoint is infinite.

**Failure.** The lower equality gives an unbounded collapse at the lower X cut. At or above a finite upper threshold the excluded uniform wins. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** The rate and effective cuts are unique. An estimated nominal lower bound may be anywhere at or below zero when m=0.

### exp free truncate:upper-estimated

Here the upper cut is the maximum $h$ and the lower cut is fixed.

**Criterion.** On positive width, the mean of X must strictly exceed the lower endpoint of the increasing transform of [a,b] and be strictly below the transformed midpoint. Equality with the lower transformed cut requires all observations at the corresponding fixed X cut. The upper transformed endpoint is finite.

**Failure.** The lower equality gives an unbounded collapse at the lower X cut. At or above a finite upper threshold the excluded uniform wins. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### exp free truncate:both-estimated

Here the cuts are the minimum $m$ and maximum $h$.

**Criterion.** On positive width, the mean of X must strictly exceed the lower endpoint of the increasing transform of [a,b] and be strictly below the transformed midpoint. The lower transformed cut is an observed extreme, so equality there occurs exactly for constant samples. The upper transformed endpoint is finite.

**Failure.** The lower equality gives an unbounded collapse at the lower X cut. At or above a finite upper threshold the excluded uniform wins. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** The rate and effective cuts are unique. An estimated nominal lower bound may be anywhere at or below zero when m=0.

### gamma free truncate:lower-estimated

Here the lower cut is the minimum $m$ and the upper cut is fixed.

**Criterion.** Nonconstant data attain iff no eligible gamma edge or corner passes its moment tests. The shape-zero edge is eligible; a finite fixed upper cut also admits the rate-zero edge and corner.

**Failure.** The winning excluded law is the shape-zero carrier, the rate-zero power law, or the log-uniform corner. On positive width a constant sample collapses to its observed point, with unbounded likelihood. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### gamma free truncate:upper-estimated

Here the upper cut is the maximum $h$ and the lower cut is fixed.

**Criterion.** Nonconstant data attain iff no eligible gamma edge or corner passes its moment tests. The rate-zero edge is eligible; a positive fixed effective lower cut also admits the shape-zero edge and corner.

**Failure.** The winning excluded law is the shape-zero carrier, the rate-zero power law, or the log-uniform corner. On positive width a constant sample collapses to its observed point, with unbounded likelihood. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### gamma free truncate:both-estimated

Here the cuts are the minimum $m$ and maximum $h$.

**Criterion.** Nonconstant data attain iff no eligible gamma edge or corner passes its moment tests. Both edges and the log-uniform corner are eligible.

**Failure.** The winning excluded law is the shape-zero carrier, the rate-zero power law, or the log-uniform corner. On positive width a constant sample collapses to its observed point, with unbounded likelihood. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### gamma scale truncate:lower-estimated

Here the lower cut is the minimum $m$ and the upper cut is fixed.

**Criterion.** On positive width the empirical log mean must strictly exceed the zero-shape carrier log mean and be strictly below log b. The zero-shape threshold is finite since a=m>0. The top log equality requires every observation at the finite fixed upper cut.

**Failure.** At or below the finite lower threshold the shape-zero density proportional to exp(-x/scale)/x wins. Top log equality gives unbounded collapse at the upper cut. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### gamma scale truncate:upper-estimated

Here the upper cut is the maximum $h$ and the lower cut is fixed.

**Criterion.** On positive width the empirical log mean must strictly exceed the zero-shape carrier log mean and be strictly below log b. The zero-shape threshold is finite iff the fixed effective a is positive. The top log equality requires exactly a constant sample.

**Failure.** At or below the finite lower threshold the shape-zero density proportional to exp(-x/scale)/x wins. Top log equality gives unbounded collapse at the upper cut. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### gamma scale truncate:both-estimated

Here the cuts are the minimum $m$ and maximum $h$.

**Criterion.** On positive width the empirical log mean must strictly exceed the zero-shape carrier log mean and be strictly below log b. The zero-shape threshold is finite since a=m>0. The top log equality requires exactly a constant sample.

**Failure.** At or below the finite lower threshold the shape-zero density proportional to exp(-x/scale)/x wins. Top log equality gives unbounded collapse at the upper cut. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### gamma shape truncate:lower-estimated

Here the lower cut is the minimum $m$ and the upper cut is fixed.

**Criterion.** On positive width, the mean of X must strictly exceed the lower endpoint of the increasing transform of [a,b] and be strictly below the held-shape carrier mean. The lower transformed cut is an observed extreme, so equality there occurs exactly for constant samples. Use the infinite threshold if the upper transformed endpoint is infinite.

**Failure.** The lower equality gives an unbounded collapse at the lower X cut. At or above a finite upper threshold the excluded power density with the held shape wins. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### gamma shape truncate:upper-estimated

Here the upper cut is the maximum $h$ and the lower cut is fixed.

**Criterion.** On positive width, the mean of X must strictly exceed the lower endpoint of the increasing transform of [a,b] and be strictly below the held-shape carrier mean. Equality with the lower transformed cut requires all observations at the corresponding fixed X cut. The upper transformed endpoint is finite.

**Failure.** The lower equality gives an unbounded collapse at the lower X cut. At or above a finite upper threshold the excluded power density with the held shape wins. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### gamma shape truncate:both-estimated

Here the cuts are the minimum $m$ and maximum $h$.

**Criterion.** On positive width, the mean of X must strictly exceed the lower endpoint of the increasing transform of [a,b] and be strictly below the held-shape carrier mean. The lower transformed cut is an observed extreme, so equality there occurs exactly for constant samples. The upper transformed endpoint is finite.

**Failure.** The lower equality gives an unbounded collapse at the lower X cut. At or above a finite upper threshold the excluded power density with the held shape wins. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### geom free truncate:lower-estimated

Here the lower cut is the minimum $m$ and the upper cut is fixed.

**Criterion.** A compatible singleton attains. On a nonsingleton a unique interior probability fits iff the mean is above L and below the finite-window midpoint, with infinity used for the midpoint on an unbounded upper side. The sample minimum is L. Constant data at L attain only for zero or the admitted alias.

**Failure.** The lower atom at L is excluded unless it is the ordinary zero atom or admitted at one by the alias. At or above the finite-window midpoint the excluded discrete uniform wins on a nonsingleton.

**Uniqueness.** One parent coefficient on a nonsingleton when a root or admitted atom wins. On a singleton every compatible parent coefficient attains. Ordinary nominal bounds have the mass plateaus of the profiling lemma. Include the alias pairs described above.

### geom free truncate:upper-estimated

Here the upper cut is the maximum $h$ and the lower cut is fixed.

**Criterion.** A compatible singleton attains. On a nonsingleton a unique interior probability fits iff the mean is above L and below the finite-window midpoint, with infinity used for the midpoint on an unbounded upper side. The sample maximum is U. A constant at U on a nonsingleton cannot fit.

**Failure.** At or above the finite-window midpoint the excluded discrete uniform wins on a nonsingleton.

**Uniqueness.** One parent coefficient on a nonsingleton when a root or admitted atom wins. On a singleton every compatible parent coefficient attains. Ordinary nominal bounds have the mass plateaus of the profiling lemma. Include the alias pairs described above.

### geom free truncate:both-estimated

Here the cuts are the minimum $m$ and maximum $h$.

**Criterion.** A compatible singleton attains. On a nonsingleton a unique interior probability fits iff the mean is above L and below the finite-window midpoint, with infinity used for the midpoint on an unbounded upper side. For nonconstant data this reduces to the sample mean being strictly below (m+h)/2.

**Failure.** At or above the finite-window midpoint the excluded discrete uniform wins on a nonsingleton.

**Uniqueness.** One parent coefficient on a nonsingleton when a root or admitted atom wins. On a singleton every compatible parent coefficient attains. Ordinary nominal bounds have the mass plateaus of the profiling lemma. Include the alias pairs described above.

### invgamma alpha truncate:lower-estimated

Here the lower cut is the minimum $m$ and the upper cut is fixed.

**Criterion.** On positive width, the mean of reciprocal X must strictly exceed the lower endpoint of the decreasing transform of [a,b] with endpoint order reversed and be strictly below the held-shape carrier mean. Equality with the lower transformed cut requires all observations at the corresponding fixed X cut. The upper transformed endpoint is finite.

**Failure.** The lower equality gives an unbounded collapse at the upper X cut. At or above a finite upper threshold the excluded density proportional to x to the power minus alpha minus one wins. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### invgamma alpha truncate:upper-estimated

Here the upper cut is the maximum $h$ and the lower cut is fixed.

**Criterion.** On positive width, the mean of reciprocal X must strictly exceed the lower endpoint of the decreasing transform of [a,b] with endpoint order reversed and be strictly below the held-shape carrier mean. The lower transformed cut is an observed extreme, so equality there occurs exactly for constant samples. Use the infinite threshold if the upper transformed endpoint is infinite.

**Failure.** The lower equality gives an unbounded collapse at the upper X cut. At or above a finite upper threshold the excluded density proportional to x to the power minus alpha minus one wins. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### invgamma alpha truncate:both-estimated

Here the cuts are the minimum $m$ and maximum $h$.

**Criterion.** On positive width, the mean of reciprocal X must strictly exceed the lower endpoint of the decreasing transform of [a,b] with endpoint order reversed and be strictly below the held-shape carrier mean. The lower transformed cut is an observed extreme, so equality there occurs exactly for constant samples. The upper transformed endpoint is finite.

**Failure.** The lower equality gives an unbounded collapse at the upper X cut. At or above a finite upper threshold the excluded density proportional to x to the power minus alpha minus one wins. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### invgauss free truncate:lower-estimated

Here the lower cut is the minimum $m$ and the upper cut is fixed.

**Criterion.** Nonconstant data attain iff no eligible inverse Gaussian edge or corner passes its moment tests. The v=0 edge is eligible. The Levy u=0 edge and corner can win only with finite fixed upper cut.

**Failure.** The excluded laws are the truncated Levy edge, the inverse-power exponential edge and the inverse-power corner. On positive width a constant sample collapses to its observed point, with unbounded likelihood. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### invgauss free truncate:upper-estimated

Here the upper cut is the maximum $h$ and the lower cut is fixed.

**Criterion.** Nonconstant data attain iff no eligible inverse Gaussian edge or corner passes its moment tests. The Levy u=0 edge is eligible. A positive fixed effective lower cut also admits the v=0 edge and corner.

**Failure.** The excluded laws are the truncated Levy edge, the inverse-power exponential edge and the inverse-power corner. On positive width a constant sample collapses to its observed point, with unbounded likelihood. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### invgauss free truncate:both-estimated

Here the cuts are the minimum $m$ and maximum $h$.

**Criterion.** Nonconstant data attain iff no eligible inverse Gaussian edge or corner passes its moment tests. Both edges and the inverse-power corner are eligible.

**Failure.** The excluded laws are the truncated Levy edge, the inverse-power exponential edge and the inverse-power corner. On positive width a constant sample collapses to its observed point, with unbounded likelihood. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### invgauss mean truncate:lower-estimated

Here the lower cut is the minimum $m$ and the upper cut is fixed.

**Criterion.** On positive width require the sample mean of X/mean squared plus 1/X strictly above its value at the projected held mean and strictly below the inverse-power carrier expectation. The upper threshold is finite only when the fixed upper cut is finite. The lower equality requires all data at that projection.

**Failure.** The lower equality collapses at the projection of the held mean. At or above a finite upper threshold the excluded density proportional to x to the power -3/2 wins. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### invgauss mean truncate:upper-estimated

Here the upper cut is the maximum $h$ and the lower cut is fixed.

**Criterion.** On positive width require the sample mean of X/mean squared plus 1/X strictly above its value at the projected held mean and strictly below the inverse-power carrier expectation. The upper threshold is finite only when the fixed effective lower cut is positive. The lower equality requires all data at that projection.

**Failure.** The lower equality collapses at the projection of the held mean. At or above a finite upper threshold the excluded density proportional to x to the power -3/2 wins. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### invgauss mean truncate:both-estimated

Here the cuts are the minimum $m$ and maximum $h$.

**Criterion.** On positive width require the sample mean of X/mean squared plus 1/X strictly above its value at the projected held mean and strictly below the inverse-power carrier expectation. The upper threshold is finite. The lower equality requires all data at that projection.

**Failure.** The lower equality collapses at the projection of the held mean. At or above a finite upper threshold the excluded density proportional to x to the power -3/2 wins. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### invgauss shape truncate:lower-estimated

Here the lower cut is the minimum $m$ and the upper cut is fixed.

**Criterion.** On positive width require the mean strictly above a and strictly below the truncated Levy mean. The lower equality is exactly constant data. The upper threshold is infinite for an infinite fixed upper cut.

**Failure.** Lower equality gives unbounded collapse at a. At or above a finite upper threshold the excluded truncated Levy law wins. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### invgauss shape truncate:upper-estimated

Here the upper cut is the maximum $h$ and the lower cut is fixed.

**Criterion.** On positive width require the mean strictly above a and strictly below the truncated Levy mean. The lower equality requires every observation at the fixed lower cut. The upper threshold is finite.

**Failure.** Lower equality gives unbounded collapse at a. At or above a finite upper threshold the excluded truncated Levy law wins. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### invgauss shape truncate:both-estimated

Here the cuts are the minimum $m$ and maximum $h$.

**Criterion.** On positive width require the mean strictly above a and strictly below the truncated Levy mean. The lower equality is exactly constant data. The upper threshold is finite.

**Failure.** Lower equality gives unbounded collapse at a. At or above a finite upper threshold the excluded truncated Levy law wins. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### invweibull shape truncate:lower-estimated

Here the lower cut is the minimum $m$ and the upper cut is fixed.

**Criterion.** On positive width, the mean of X to the negative held shape must strictly exceed the lower endpoint of the decreasing transform of [a,b] with endpoint order reversed and be strictly below the transformed midpoint. Equality with the lower transformed cut requires all observations at the corresponding fixed X cut. The upper transformed endpoint is finite.

**Failure.** The lower equality gives an unbounded collapse at the upper X cut. At or above a finite upper threshold the excluded density proportional to x to the power minus shape minus one wins. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### invweibull shape truncate:upper-estimated

Here the upper cut is the maximum $h$ and the lower cut is fixed.

**Criterion.** On positive width, the mean of X to the negative held shape must strictly exceed the lower endpoint of the decreasing transform of [a,b] with endpoint order reversed and be strictly below the transformed midpoint. The lower transformed cut is an observed extreme, so equality there occurs exactly for constant samples. Use the infinite threshold if the upper transformed endpoint is infinite.

**Failure.** The lower equality gives an unbounded collapse at the upper X cut. At or above a finite upper threshold the excluded density proportional to x to the power minus shape minus one wins. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### invweibull shape truncate:both-estimated

Here the cuts are the minimum $m$ and maximum $h$.

**Criterion.** On positive width, the mean of X to the negative held shape must strictly exceed the lower endpoint of the decreasing transform of [a,b] with endpoint order reversed and be strictly below the transformed midpoint. The lower transformed cut is an observed extreme, so equality there occurs exactly for constant samples. The upper transformed endpoint is finite.

**Failure.** The lower equality gives an unbounded collapse at the upper X cut. At or above a finite upper threshold the excluded density proportional to x to the power minus shape minus one wins. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### lnorm free truncate:lower-estimated

Here the lower cut is the minimum $m$ and the upper cut is fixed.

**Criterion.** For nonconstant data, the variance of log X with divisor n must be strictly below the variance of the matched linear-tilt face in those coordinates. If the opposite effective endpoint is infinite, use the lower-only face.

**Failure.** At or above equality the excluded power face wins, including log-uniform when the linear tilt vanishes. On positive width a constant sample collapses to its observed point, with unbounded likelihood. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### lnorm free truncate:upper-estimated

Here the upper cut is the maximum $h$ and the lower cut is fixed.

**Criterion.** For nonconstant data, the variance of log X with divisor n must be strictly below the variance of the matched linear-tilt face in those coordinates. If the opposite effective endpoint is infinite, use the upper-only face.

**Failure.** At or above equality the excluded power face wins, including log-uniform when the linear tilt vanishes. On positive width a constant sample collapses to its observed point, with unbounded likelihood. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### lnorm free truncate:both-estimated

Here the cuts are the minimum $m$ and maximum $h$.

**Criterion.** For nonconstant data, the variance of log X with divisor n must be strictly below the variance of the matched linear-tilt face in those coordinates. The face has two finite cuts.

**Failure.** At or above equality the excluded power face wins, including log-uniform when the linear tilt vanishes. On positive width a constant sample collapses to its observed point, with unbounded likelihood. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### lnorm meanlog truncate:lower-estimated

Here the lower cut is the minimum $m$ and the upper cut is fixed.

**Criterion.** On positive width, the mean squared distance of log X from the held mean must lie strictly between the squared distance to its interval projection and the uniform squared-distance mean in these coordinates, infinite on unbounded windows. A positive-width constant sample at the lower cut is the projection iff the held mean is at or below it in these coordinates.

**Failure.** Lower equality gives unbounded projection collapse. At or above the finite upper threshold the excluded log-uniform wins. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### lnorm meanlog truncate:upper-estimated

Here the upper cut is the maximum $h$ and the lower cut is fixed.

**Criterion.** On positive width, the mean squared distance of log X from the held mean must lie strictly between the squared distance to its interval projection and the uniform squared-distance mean in these coordinates, infinite on unbounded windows. A positive-width constant sample at the upper cut is the projection iff the held mean is at or above it in these coordinates.

**Failure.** Lower equality gives unbounded projection collapse. At or above the finite upper threshold the excluded log-uniform wins. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### lnorm meanlog truncate:both-estimated

Here the cuts are the minimum $m$ and maximum $h$.

**Criterion.** On positive width, the mean squared distance of log X from the held mean must lie strictly between the squared distance to its interval projection and the uniform squared-distance mean in these coordinates, infinite on unbounded windows. On positive width the observations are nonconstant, so the lower equality is impossible.

**Failure.** Lower equality gives unbounded projection collapse. At or above the finite upper threshold the excluded log-uniform wins. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### lnorm sdlog truncate:lower-estimated

Here the lower cut is the minimum $m$ and the upper cut is fixed.

**Criterion.** On positive width, attainment is equivalent to nonconstant data, since the sample mean of log X must be strictly between the cuts and at least one cut is an observed extreme.

**Failure.** A positive-width constant gives an unbounded location escape at the estimated cut and a weak point mass there. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### lnorm sdlog truncate:upper-estimated

Here the upper cut is the maximum $h$ and the lower cut is fixed.

**Criterion.** On positive width, attainment is equivalent to nonconstant data, since the sample mean of log X must be strictly between the cuts and at least one cut is an observed extreme.

**Failure.** A positive-width constant gives an unbounded location escape at the estimated cut and a weak point mass there. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### lnorm sdlog truncate:both-estimated

Here the cuts are the minimum $m$ and maximum $h$.

**Criterion.** On positive width, attainment is equivalent to nonconstant data, since the sample mean of log X must be strictly between the cuts and at least one cut is an observed extreme.

**Failure.** A constant already has coincident cuts. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### nbinom size truncate:lower-estimated

Here the lower cut is the minimum $m$ and the upper cut is fixed.

**Criterion.** At positive size a compatible singleton attains. On a nonsingleton the mean must be strictly above L and strictly below the rising-factorial carrier mean at zero probability, with infinity used on an unbounded upper side. The sample minimum is L, with the ordinary zero and the positive-alias one as attained lower atoms. At size zero only all-zero data or positive data admitting the alias fit.

**Failure.** At positive size the lower atom is excluded away from zero and the alias. On a nonsingleton at or above the finite upper threshold the excluded rising-factorial law wins, uniform at size one. At size zero incompatible samples have no positive likelihood.

**Uniqueness.** One parent coefficient on a nonsingleton when a root or admitted atom wins. On a singleton every compatible parent coefficient attains. Ordinary nominal bounds have the mass plateaus of the profiling lemma. Include the alias pairs described above. At size zero all probabilities attain for all zeros. Positive data at size zero have a unique joint fit when the alias is available, with its exact bounds.

### nbinom size truncate:upper-estimated

Here the upper cut is the maximum $h$ and the lower cut is fixed.

**Criterion.** At positive size a compatible singleton attains. On a nonsingleton the mean must be strictly above L and strictly below the rising-factorial carrier mean at zero probability, with infinity used on an unbounded upper side. The sample maximum is U, and a nonsingleton constant is above the carrier mean. At size zero only all-zero data or positive data admitting the alias fit.

**Failure.** On a nonsingleton at or above the finite upper threshold the excluded rising-factorial law wins, uniform at size one. At size zero incompatible samples have no positive likelihood.

**Uniqueness.** One parent coefficient on a nonsingleton when a root or admitted atom wins. On a singleton every compatible parent coefficient attains. Ordinary nominal bounds have the mass plateaus of the profiling lemma. Include the alias pairs described above. At size zero all probabilities attain for all zeros. Positive data at size zero have a unique joint fit when the alias is available, with its exact bounds.

### nbinom size truncate:both-estimated

Here the cuts are the minimum $m$ and maximum $h$.

**Criterion.** At positive size a compatible singleton attains. On a nonsingleton the mean must be strictly above L and strictly below the rising-factorial carrier mean at zero probability, with infinity used on an unbounded upper side. For nonconstant data the mean automatically exceeds L=m. At size zero only all-zero data or positive data admitting the alias fit.

**Failure.** On a nonsingleton at or above the finite upper threshold the excluded rising-factorial law wins, uniform at size one. At size zero incompatible samples have no positive likelihood.

**Uniqueness.** One parent coefficient on a nonsingleton when a root or admitted atom wins. On a singleton every compatible parent coefficient attains. Ordinary nominal bounds have the mass plateaus of the profiling lemma. Include the alias pairs described above. At size zero all probabilities attain for all zeros. Positive data at size zero have a unique joint fit when the alias is available, with its exact bounds.

### norm free truncate:lower-estimated

Here the lower cut is the minimum $m$ and the upper cut is fixed.

**Criterion.** For nonconstant data, the variance of X with divisor n must be strictly below the variance of the matched linear-tilt face in those coordinates. If the opposite effective endpoint is infinite, use the lower-only face.

**Failure.** At or above equality the excluded exponential face wins, including uniform when the linear tilt vanishes. On positive width a constant sample collapses to its observed point, with unbounded likelihood. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### norm free truncate:upper-estimated

Here the upper cut is the maximum $h$ and the lower cut is fixed.

**Criterion.** For nonconstant data, the variance of X with divisor n must be strictly below the variance of the matched linear-tilt face in those coordinates. If the opposite effective endpoint is infinite, use the upper-only face.

**Failure.** At or above equality the excluded exponential face wins, including uniform when the linear tilt vanishes. On positive width a constant sample collapses to its observed point, with unbounded likelihood. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### norm free truncate:both-estimated

Here the cuts are the minimum $m$ and maximum $h$.

**Criterion.** For nonconstant data, the variance of X with divisor n must be strictly below the variance of the matched linear-tilt face in those coordinates. The face has two finite cuts.

**Failure.** At or above equality the excluded exponential face wins, including uniform when the linear tilt vanishes. On positive width a constant sample collapses to its observed point, with unbounded likelihood. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### norm mean truncate:lower-estimated

Here the lower cut is the minimum $m$ and the upper cut is fixed.

**Criterion.** On positive width, the mean squared distance of X from the held mean must lie strictly between the squared distance to its interval projection and the uniform squared-distance mean in these coordinates, infinite on unbounded windows. A positive-width constant sample at the lower cut is the projection iff the held mean is at or below it in these coordinates.

**Failure.** Lower equality gives unbounded projection collapse. At or above the finite upper threshold the excluded uniform wins. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### norm mean truncate:upper-estimated

Here the upper cut is the maximum $h$ and the lower cut is fixed.

**Criterion.** On positive width, the mean squared distance of X from the held mean must lie strictly between the squared distance to its interval projection and the uniform squared-distance mean in these coordinates, infinite on unbounded windows. A positive-width constant sample at the upper cut is the projection iff the held mean is at or above it in these coordinates.

**Failure.** Lower equality gives unbounded projection collapse. At or above the finite upper threshold the excluded uniform wins. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### norm mean truncate:both-estimated

Here the cuts are the minimum $m$ and maximum $h$.

**Criterion.** On positive width, the mean squared distance of X from the held mean must lie strictly between the squared distance to its interval projection and the uniform squared-distance mean in these coordinates, infinite on unbounded windows. On positive width the observations are nonconstant, so the lower equality is impossible.

**Failure.** Lower equality gives unbounded projection collapse. At or above the finite upper threshold the excluded uniform wins. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### norm sd truncate:lower-estimated

Here the lower cut is the minimum $m$ and the upper cut is fixed.

**Criterion.** On positive width, attainment is equivalent to nonconstant data, since the sample mean of X must be strictly between the cuts and at least one cut is an observed extreme.

**Failure.** A positive-width constant gives an unbounded location escape at the estimated cut and a weak point mass there. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### norm sd truncate:upper-estimated

Here the upper cut is the maximum $h$ and the lower cut is fixed.

**Criterion.** On positive width, attainment is equivalent to nonconstant data, since the sample mean of X must be strictly between the cuts and at least one cut is an observed extreme.

**Failure.** A positive-width constant gives an unbounded location escape at the estimated cut and a weak point mass there. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### norm sd truncate:both-estimated

Here the cuts are the minimum $m$ and maximum $h$.

**Criterion.** On positive width, attainment is equivalent to nonconstant data, since the sample mean of X must be strictly between the cuts and at least one cut is an observed extreme.

**Failure.** A constant already has coincident cuts. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### pareto free truncate:lower-estimated

Here the lower cut is the minimum $m$ and the upper cut is fixed.

**Criterion.** The optimized effective lower cut equals m. On positive width, the empirical log mean must strictly exceed the effective log lower cut and be below the log midpoint if the upper cut is finite.

**Failure.** At the lower statistic equality the shape tends to infinity and the likelihood diverges to a lower point mass. At or above the finite log midpoint the excluded log-uniform wins. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** The fitted shape and effective cuts are unique. Any parent endpoint at or below m is allowed, and the nominal lower cut must equal m unless the parent endpoint equals m.

### pareto free truncate:upper-estimated

Here the upper cut is the maximum $h$ and the lower cut is fixed.

**Criterion.** The optimized effective lower cut equals m. On positive width, the empirical log mean must strictly exceed the effective log lower cut and be below the log midpoint if the upper cut is finite.

**Failure.** At the lower statistic equality the shape tends to infinity and the likelihood diverges to a lower point mass. At or above the finite log midpoint the excluded log-uniform wins. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** The fitted shape and effective cuts are unique. The parent endpoint equals m when the fixed lower cut is below m. Otherwise any positive endpoint at or below m maximizes.

### pareto free truncate:both-estimated

Here the cuts are the minimum $m$ and maximum $h$.

**Criterion.** The optimized effective lower cut equals m. On positive width, the empirical log mean must strictly exceed the effective log lower cut and be below the log midpoint if the upper cut is finite.

**Failure.** At the lower statistic equality the shape tends to infinity and the likelihood diverges to a lower point mass. At or above the finite log midpoint the excluded log-uniform wins. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** The fitted shape and effective cuts are unique. Any parent endpoint at or below m is allowed, and the nominal lower cut must equal m unless the parent endpoint equals m.

### pareto a truncate:lower-estimated

Here the lower cut is the minimum $m$ and the upper cut is fixed.

**Criterion.** The optimized effective lower cut equals m. The held shape fits iff this effective window has positive width.

**Failure.** A constant at the upper cut gives unbounded shrinking-window likelihood when the effective lower or upper cut can approach the sample. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** The upper cut is unique when estimated. Any parent endpoint at or below m is allowed, and the nominal lower cut must equal m unless the parent endpoint equals m.

### pareto a truncate:upper-estimated

Here the upper cut is the maximum $h$ and the lower cut is fixed.

**Criterion.** The optimized effective lower cut equals m. The held shape fits iff this effective window has positive width.

**Failure.** A constant at the upper cut gives unbounded shrinking-window likelihood when the effective lower or upper cut can approach the sample. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** The upper cut is unique when estimated. The parent endpoint equals m when the fixed lower cut is below m. Otherwise any positive endpoint at or below m maximizes.

### pareto a truncate:both-estimated

Here the cuts are the minimum $m$ and maximum $h$.

**Criterion.** The optimized effective lower cut equals m. The held shape fits iff this effective window has positive width.

**Failure.** A constant at the upper cut gives unbounded shrinking-window likelihood when the effective lower or upper cut can approach the sample. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** The upper cut is unique when estimated. Any parent endpoint at or below m is allowed, and the nominal lower cut must equal m unless the parent endpoint equals m.

### pareto b truncate:lower-estimated

Here the lower cut is the minimum $m$ and the upper cut is fixed.

**Criterion.** Use the maximum of the held endpoint b and the nominal lower cut. The effective lower cut equals m. On positive width, the empirical log mean must strictly exceed the effective log lower cut and be below the log midpoint if the upper cut is finite.

**Failure.** At the lower statistic equality the shape tends to infinity and the likelihood diverges to a lower point mass. At or above the finite log midpoint the excluded log-uniform wins. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** The fitted shape and effective cuts are unique. The estimated lower cut is flat down to minus infinity if m equals the held endpoint, and unique otherwise.

### pareto b truncate:upper-estimated

Here the upper cut is the maximum $h$ and the lower cut is fixed.

**Criterion.** Use the maximum of the held endpoint b and the nominal lower cut. On positive width, the empirical log mean must strictly exceed the effective log lower cut and be below the log midpoint if the upper cut is finite.

**Failure.** At the lower statistic equality the shape tends to infinity and the likelihood diverges to a lower point mass. At or above the finite log midpoint the excluded log-uniform wins. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** The fitted shape and effective cuts are unique.

### pareto b truncate:both-estimated

Here the cuts are the minimum $m$ and maximum $h$.

**Criterion.** Use the maximum of the held endpoint b and the nominal lower cut. The effective lower cut equals m. On positive width, the empirical log mean must strictly exceed the effective log lower cut and be below the log midpoint if the upper cut is finite.

**Failure.** At the lower statistic equality the shape tends to infinity and the likelihood diverges to a lower point mass. At or above the finite log midpoint the excluded log-uniform wins. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** The fitted shape and effective cuts are unique. The estimated lower cut is flat down to minus infinity if m equals the held endpoint, and unique otherwise.

### pois free truncate:lower-estimated

Here the lower cut is the minimum $m$ and the upper cut is fixed.

**Criterion.** A compatible singleton attains. On a nonsingleton nonconstant data attain at the unique positive rate. Constant data attain only at zero or at the admitted positive-alias atom at one.

**Failure.** A constant positive sample on a nonsingleton lower-tight window gives the excluded lower atom, except at the admitted alias.

**Uniqueness.** One parent coefficient on a nonsingleton when a root or admitted atom wins. On a singleton every compatible parent coefficient attains. Ordinary nominal bounds have the mass plateaus of the profiling lemma. Include the alias pairs described above. The [1, Inf] alias admits rate zero when all observations are one.

### pois free truncate:upper-estimated

Here the upper cut is the maximum $h$ and the lower cut is fixed.

**Criterion.** A compatible singleton attains. Nonconstant data attain at the unique positive rate. A constant on a nonsingleton upper-tight window escapes.

**Failure.** A constant on a nonsingleton upper-tight window gives the excluded infinite-rate top atom.

**Uniqueness.** One parent coefficient on a nonsingleton when a root or admitted atom wins. On a singleton every compatible parent coefficient attains. Ordinary nominal bounds have the mass plateaus of the profiling lemma. Include the alias pairs described above. The [1, Inf] alias can add a looser rate-zero atom at one.

### pois free truncate:both-estimated

Here the cuts are the minimum $m$ and maximum $h$.

**Criterion.** Every sample attains. A constant gives a singleton. For nonconstant data the mean is strictly between the retained cuts and the positive rate is unique.

**Failure.** There is no escape on a valid design.

**Uniqueness.** One parent coefficient on a nonsingleton when a root or admitted atom wins. On a singleton every compatible parent coefficient attains. Ordinary nominal bounds have the mass plateaus of the profiling lemma. Include the alias pairs described above. The [1, Inf] alias can add a looser rate-zero atom at one.

### rayleigh free truncate:lower-estimated

Here the lower cut is the minimum $m$ and the upper cut is fixed.

**Criterion.** On positive width, the mean of X squared must strictly exceed the lower endpoint of the increasing transform of [a,b] and be strictly below the transformed midpoint. The lower transformed cut is an observed extreme, so equality there occurs exactly for constant samples. Use the infinite threshold if the upper transformed endpoint is infinite.

**Failure.** The lower equality gives an unbounded collapse at the lower X cut. At or above a finite upper threshold the excluded density proportional to x wins. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### rayleigh free truncate:upper-estimated

Here the upper cut is the maximum $h$ and the lower cut is fixed.

**Criterion.** On positive width, the mean of X squared must strictly exceed the lower endpoint of the increasing transform of [a,b] and be strictly below the transformed midpoint. Equality with the lower transformed cut requires all observations at the corresponding fixed X cut. The upper transformed endpoint is finite.

**Failure.** The lower equality gives an unbounded collapse at the lower X cut. At or above a finite upper threshold the excluded density proportional to x wins. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### rayleigh free truncate:both-estimated

Here the cuts are the minimum $m$ and maximum $h$.

**Criterion.** On positive width, the mean of X squared must strictly exceed the lower endpoint of the increasing transform of [a,b] and be strictly below the transformed midpoint. The lower transformed cut is an observed extreme, so equality there occurs exactly for constant samples. The upper transformed endpoint is finite.

**Failure.** The lower equality gives an unbounded collapse at the lower X cut. At or above a finite upper threshold the excluded density proportional to x wins. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### weibull shape truncate:lower-estimated

Here the lower cut is the minimum $m$ and the upper cut is fixed.

**Criterion.** On positive width, the mean of X to the held shape must strictly exceed the lower endpoint of the increasing transform of [a,b] and be strictly below the transformed midpoint. The lower transformed cut is an observed extreme, so equality there occurs exactly for constant samples. Use the infinite threshold if the upper transformed endpoint is infinite.

**Failure.** The lower equality gives an unbounded collapse at the lower X cut. At or above a finite upper threshold the excluded density proportional to x to the power shape minus one wins. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### weibull shape truncate:upper-estimated

Here the upper cut is the maximum $h$ and the lower cut is fixed.

**Criterion.** On positive width, the mean of X to the held shape must strictly exceed the lower endpoint of the increasing transform of [a,b] and be strictly below the transformed midpoint. Equality with the lower transformed cut requires all observations at the corresponding fixed X cut. The upper transformed endpoint is finite.

**Failure.** The lower equality gives an unbounded collapse at the lower X cut. At or above a finite upper threshold the excluded density proportional to x to the power shape minus one wins. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### weibull shape truncate:both-estimated

Here the cuts are the minimum $m$ and maximum $h$.

**Criterion.** On positive width, the mean of X to the held shape must strictly exceed the lower endpoint of the increasing transform of [a,b] and be strictly below the transformed midpoint. The lower transformed cut is an observed extreme, so equality there occurs exactly for constant samples. The upper transformed endpoint is finite.

**Failure.** The lower equality gives an unbounded collapse at the lower X cut. At or above a finite upper threshold the excluded density proportional to x to the power shape minus one wins. A coincident cut gives shrinking-window divergence if approachable, and an invalid design otherwise.

**Uniqueness.** One parent coefficient vector and one pair of bounds.

### zeta free truncate:lower-estimated

Here the lower cut is the minimum $m$ and the upper cut is fixed.

**Criterion.** With signed exponents on finite windows starting at m, a singleton attains and any nonconstant sample fits. A constant on a nonsingleton gives the lower atom. With unbounded upper cut and parent s>1, the unique root exists iff the empirical log mean exceeds log m. The declared signed route requires L=1. If a free lower cut would resolve above 1, apply the same signed rule at L=1 to optimize within the declared window submodel. For finite windows under the parent domain alone use the stricter s=1 mean threshold above.

**Failure.** On a nonsingleton signed window any endpoint-only equality selects its excluded point mass. The parent-domain lower equality also selects its lower atom, and at or above the finite s=1 threshold the excluded harmonic law wins.

**Uniqueness.** One exponent at an interior root and all allowed exponents on a singleton. Ordinary nominal real bounds have rounding or support plateaus, restricted to eligible windows in the declared submodel.

### zeta free truncate:upper-estimated

Here the upper cut is the maximum $h$ and the lower cut is fixed.

**Criterion.** With signed exponents a singleton attains and any nonconstant sample fits. A constant on a nonsingleton gives the upper atom. The declared signed route requires L=1. If a free lower cut would resolve above 1, apply the same signed rule at L=1 to optimize within the declared window submodel. For finite windows under the parent domain alone use the stricter s=1 mean threshold above.

**Failure.** On a nonsingleton signed window any endpoint-only equality selects its excluded point mass. The parent-domain lower equality also selects its lower atom, and at or above the finite s=1 threshold the excluded harmonic law wins.

**Uniqueness.** One exponent at an interior root and all allowed exponents on a singleton. Ordinary nominal real bounds have rounding or support plateaus, restricted to eligible windows in the declared submodel.

### zeta free truncate:both-estimated

Here the cuts are the minimum $m$ and maximum $h$.

**Criterion.** With signed exponents every finite tight window fits. Nonconstant data have interior log mean, and a constant gives a singleton. The declared signed route requires L=1. If a free lower cut would resolve above 1, apply the same signed rule at L=1 to optimize within the declared window submodel. For finite windows under the parent domain alone use the stricter s=1 mean threshold above.

**Failure.** On a nonsingleton signed window any endpoint-only equality selects its excluded point mass. The parent-domain lower equality also selects its lower atom, and at or above the finite s=1 threshold the excluded harmonic law wins.

**Uniqueness.** One exponent at an interior root and all allowed exponents on a singleton. Ordinary nominal real bounds have rounding or support plateaus, restricted to eligible windows in the declared submodel.

## Summary

All continuous rows first test positive effective width. Every attained ordinary continuous coefficient is unique except for the free Pareto endpoint and the stated bound plateaus. For count rows, the table uses retained lattice cuts. An ordinary count fit always has nominal bound plateaus. Eligibility and equality are specified in the individual sections.

| Family | Reviewed masks | Lower estimated | Upper estimated | Both estimated |
| --- | --- | --- | --- | --- |
| beta | free, shape1, shape2 | Cuts off the singularity of shape 1; shape 2 may hit its upper tilt endpoint | Cuts off the singularity of shape 2; shape 1 may hit its upper tilt endpoint | Both zero-shape edges are eligible; constants have coincident cuts |
| binom | free, size, prob | Lower atom at constant data with fixed size, subject to singleton and extension; free size can shrink support to a singleton | Upper atom escape possible with fixed size or fixed interior probability | Saturated size ties on two counts; constants give a singleton |
| exp | free | Lower collapse on constants; zero gives nominal lower plateau | Finite uniform threshold | Finite uniform threshold or coincident cuts |
| gamma | free, scale, shape | Shape-zero edge always eligible; rate can diverge for held shape at constant data | Rate-zero edge always eligible; free shape can diverge at constant data with scale held | Both edges and corner eligible |
| geom | free | Lower atom or discrete uniform threshold | Discrete uniform threshold on nonsingletons | Mean strictly below midpoint for nonconstant data |
| invgamma | alpha | Finite inverse-power threshold | Upper collapse at constant data | Finite threshold or coincident cuts |
| invgauss | free, mean, shape | Inverse-power exponential edge allowed; finite upper cut can admit Lévy boundary | Lévy edge eligible; fixed lower cut governs the other edge | Both edges and corner eligible |
| invweibull | shape | Finite inverse-power threshold | Upper collapse at constant data | Finite threshold or coincident cuts |
| lnorm | free, meanlog, sdlog | Lower log cut; power faces and location escape with held SD | Upper log cut; power faces and location escape with held SD | Finite log interval, power or log-uniform faces |
| nbinom | size | Lower atom or rising-factorial threshold; size-zero alias may be feasible | Rising-factorial threshold; check alias separately | Singleton at positive size for constants; weighted face on nonsingletons; size-zero alias may be feasible |
| norm | free, mean, sd | Lower cut; translation face if upper side unbounded | Upper cut; reflection face if lower side unbounded | Finite-window exponential or uniform face; constants have coincident cuts |
| pareto | free, a, b | Parent and nominal lower cut can jointly supply the cut | Free parent endpoint can supply the unestimated lower cut | Joint endpoint plateau; log-uniform possible with free shape |
| pois | free | Excluded lower atom except zero or positive alias, or singleton | Excluded top atom on nonsingleton constants | Always attains on compatible data |
| rayleigh | free | Lower collapse on positive constants; zero density at zero | Finite linear-density threshold | Finite threshold or coincident cuts |
| weibull | shape | Lower collapse on constants | Finite power threshold | Finite threshold or coincident cuts |
| zeta | free | Signed finite and parent unbounded criteria differ; declared route requires effective lower count 1 | Signed finite fit or endpoint atom; declared route requires effective lower count 1 | Signed general window attains; current route requires minimum 1 |

**Proved examples.** Free gamma on data `(1,4)` with both cuts estimated reaches the log-uniform corner. The log mean score is zero and the corner's mean is $3/\log 4<5/2$ by strict Jensen, since $\log 2>2/3$. Free beta for `(1/4,3/4)` selects the double-zero carrier. By symmetry the carrier means of the two log statistics are equal. On this window $x(1-x)$ is strictly greater in the interior than at its ends, so both model log means exceed their sample counterparts. Free inverse Gaussian for `(1,4)` selects the inverse-power corner. The corner means of $X$ and $1/X$ are $2$ and $7/12$, below the sample means $5/2$ and $5/8$ by direct integration. Free normal for `(0,2)` with only the lower bound estimated and no upper cut is exactly on the translated exponential boundary, since sample variance and squared mean distance both equal 1. Reflection gives the upper-only example. Free normal at `(-1, -1/3, -1/3, -1/3, 1/3, 1/3, 1/3, 1)` and two estimated cuts reaches the uniform at exact equality because its mean is zero and its variance is $1/3$.

## Implementation audit

The route conclusions below follow by source inspection. The numerical replay checks likelihoods and moments independently in Python. It does not run either binding. The supported families and domains come from the [modifier design](../../design/modifiers.md#availability-and-refusals). The modifier seam resolves count bounds before choosing a route. For normal it passes estimated bounds to the normal core. The R layer records the nominal representative and adds estimated-bound degrees of freedom. Its `unique` and structured sets describe parent coefficients under the resolved window. Interpret real-bound plateaus separately in every count route. [Binding seam](../../../bindings/common/modifiers.hpp) · [R boundary resolution and metadata](../../../bindings/r/R/modifiers.R)

| Route | Status and coverage under estimated bounds |
| --- | --- |
| Poisson, free rate | The ordinary singleton, lower and upper point-mass decisions and the positive alias's attained atom agree with their resolved-window theorems. The singleton representative does not enumerate additions from looser alias windows. |
| Geometric, free probability | The ordinary lower point mass and finite discrete-uniform face agree with theorems, including the upper-estimated constant on a nonsingleton. The positive alias admits its atom at one. |
| Binomial, size held | The resolved-window atom and singleton cases agree. The conditional interval is capped at the held size before the score comparison. The alias can supply additional boundary coefficients. |
| Binomial, probability held | The interior-probability finite search and upper-atom condition agree with the mathematical alternatives when the search closes. The zero-probability alias exception can be missed by tightening. |
| Binomial, both parent coefficients free | A strict same-window Poisson victory is the correct escape. A finite candidate within a practical margin does not certify a Poisson win. A conditional Bernoulli plateau can have all sizes optimal even though the fit exposes one representative and unknown cardinality. |
| NB, size held | At positive size, ordinary endpoints and the positive alias agree when the face comparison closes. Size one exposes the discrete uniform. For other sizes a proved zero-probability face uses the generic upper-boundary diagnostic. At size zero the alias exception can be missed by tightening. |
| Normal, free or one held | Unbounded coincident windows, positive-width collapse, location escape and the named faces agree in exact arithmetic. Source uses the additional practical likelihood margin for free SD, including with the mean held. The dyadic example below separates the mathematical answer from that policy. |
| Zeta, signed exponent | On effective `[1,N]`, the singleton, finite estimate and the two infinite-exponent atoms agree. Windows starting above one, or with infinite upper cut, fail the declared implementation scope even when the broader mathematical model attains. |
| Fully held parent, where supported | Normal evaluates positive width and recognizes estimated-window collapse. The count routes evaluate compatible events, rejecting zero-event endpoints. Held zeta uses signed exponents on its declared finite support. Fully held positive aliases can also have admissible boundary laws on a looser design, since the same alias selection is applied. |

### Exact audit inputs

- **NB, an attained fit lost by tightening.** Use `x=c(1,2), size=0, truncate=c(1,NA)`, or use `c(NA,NA)` instead. The core sees `[1,2]`, where size zero has ordinary support `{0}` and returns invalid input. Joint search including `[1,Inf]` attains at the unique logarithmic probability with mean $3/2$. The same happens for lower-estimated `x=c(2,3), size=0, truncate=c(NA,Inf)`, since the tight lower cut is 2 and the feasible looser alias retains the sample.
- **Binomial, an attained endpoint lost by tightening.** Use `x=c(1), prob=0, truncate=c(1,NA)` or `c(NA,NA)`. The resolved `[1,1]` conditions the ordinary zero atom on a zero-probability event and is rejected. Under the alias, every size at least one attains. With size fixed at 2 and probability free the ordinary singleton attains at each $0<p<1$, and the alias adds $p=0$. The binomial $p=1, N=1$ and ordinary zero atom remain valid in windows containing their support.
- **Poisson, geometric and positive-size NB, a looser maximizing pair.** Use `x=c(1), truncate=c(1,NA)` or `c(NA,NA)`, with NB size 2. The resolved singleton reports an ordinary representative and a parent-parameter interval. The full joint set also admits the alias atom at rate zero for Poisson, or probability one for geometric and NB, with upper bound infinity. The analogous fully held calls fixing those boundary coefficients are rejected on the resolved ordinary singleton. They fit on the alias if that looser window is part of the estimation model.
- **Normal, exact interior with public comparison margin.** Let $d=349525/1048576$ and use the dyadic sample `(-1,-d,-d,-d,d,d,d,1)`. All of `c(NA,1)`, `c(-1,NA)` and `c(NA,NA)` give `[-1,1]`. With both parent coefficients free, or with mean held at zero, symmetry gives zero linear tilt. The inward quadratic score has the interior sign because $\delta=1/3-(1+3d^2)/4$ is strictly positive. For tilt in $[-1/100,0]$ the density ratio to uniform is at least $e^{-1/100}$, so the variance of $X^2$ is at least $e^{-1/100}(4/45)>0.08$. The root lies in this interval and its gain over uniform is at most $\delta^2/0.16<10^{-8}$ per observation by strong concavity. A successful iteration still takes the public comparison branch on this gap. That policy can withhold a genuine unique MLE. It is an unresolved comparison, rather than a mathematical escape. [Core comparison](../../../univariateML-cpp/include/cppunivariate/distributions/norm/tnorm_mle.hpp) · [Binding diagnostics](../../../bindings/common/distributions/norm/tnorm_fit.hpp)
- **NB, weighted boundary payload.** Use `x=c(1,2,2), size=2` on `c(NA,2)`, `c(1,NA)` or `c(NA,NA)`. The weights at excluded zero probability on `[1,2]` are $(2,3)$, whose mean $8/5$ is below $5/3$. The boundary is a proper law with masses $2/5,3/5$. The mapper has a named uniform branch only for size one. At size two its upper-boundary result has no corresponding weighted-limit model payload. The nonattainment classification is correct. [Count status mapping](../../../univariateML-cpp/include/cppunivariate/distributions/geom/truncation.hpp)
- **Binomial, cardinality coverage.** Use `x=c(1,1,2), truncate=c(NA,NA)`, or hold one cut at its resolved value. All $N\ge2$ with $p=1/N$ attain. Their conditional odds of count 2 against 1 are $(N-1)p/[2(1-p)]=1/2$. The generic free-size two-count shortcut returns just the first size with unknown cardinality. This is a coverage gap, and the representative itself is a maximizer. [Binomial profile and outcome](../../../univariateML-cpp/include/cppunivariate/distributions/binom/tbinom_mle.hpp)
- **Zeta, scope refusal.** Use `x=c(2,3)` and `truncate=c(NA,NA)` or `c(NA,3)`. The resolved window is `[2,3]` and the extended signed fit is $s=0$ by equal odds. The route requires $L=1$. Even within the declared design space, `[1,3]` is a valid looser window for these specifications and has a unique signed fit. With `truncate=c(2,NA)` the declared scope has no available window, while the general finite signed fit still exists. With `x=c(1,2), truncate=c(NA,Inf)` the parent-domain root exists on `[1,Inf]`, outside the finite route. In contrast, both estimated cuts for sample `(1)` give the implemented singleton with every signed exponent optimal. [Zeta guard and statuses](../../../univariateML-cpp/include/cppunivariate/distributions/zeta/truncation.hpp)

## What this changes for the fitter

1. Profile ordinary feasible windows before invoking the scalar or joint solver. Check continuous effective width, observation density and support first. Preserve the distinct outcomes for an approachable shrinking-window singularity, an invalid event, an excluded finite carrier and a count singleton.
2. Decide the semantics of coefficient domains that depend on the window. If the API promises joint estimation over the current known-window classes, include the feasible `[1,Inf]` extensions as separate candidates. Alternatively define these extensions as available only for the resolved window or for an explicitly fixed design. Under that rule the examples above describe a difference between the resolved-window estimator and a joint MLE over the available known-window classes.
3. Use empirical mean thresholds for the one-parameter faces, and inward scores after tangential matching for the two-coordinate faces. A close likelihood gap is insufficient to prove a face victory. Preserve a successful interior search as uncertified when only its comparison is unresolved, subject to the separate search accuracy check.
4. Track effective and nominal lower cuts separately for Pareto, and the design plateaus separately from parent-coefficient cardinality for count models. In the saturated binomial case the complete parent maximizing set may be an integer-indexed relation rather than a finite list.
5. Extend zeta only with an explicit domain. A signed bounded model, a parent-domain fit and a restricted signed window design have different criteria. A support check at 1 alone cannot replace this choice.

## Open

- **Alias and zeta API intent.** Decide whether window-dependent coefficient extensions participate in joint bound estimation, and whether signed zeta should extend to all finite windows. The separate mathematical criteria and exact counterexamples above apply before that interface decision.
- **Binomial profile cardinality without enumeration.** The exact finite-prefix decision describes all maximizers. A general lower-only integer unimodality theorem and a proof excluding or constructing a nonconstant lower-only tie are still missing. One would need an ordering of successive-difference signs, or exact comparison of feasible adjacent profile roots in a candidate tie. Neither is needed for the bounded or lower-only finite-prefix criterion.
- **Practical face and moment comparisons.** The exact inequalities require accurate opposing moment signs near equality. Terminating numerical coefficient and face enclosures under the public work budgets remain to be established, especially near gamma and beta corners and with divergent inverse Gaussian edge moments. For supported normal fits, a reproducible route can use a verified inward sign and a separate enclosure for the interior root.
- **Masks outside the reviewed rows.** General-window free NB size beyond the positive alias, and free Weibull, inverse Weibull and inverse-gamma shape outside their reviewed slices, need separate profile and boundary analysis. In particular, the Weibull shape changes the statistic itself. The scalar held-shape proof supplies no control of joint escaping sequences.

## Validation

The workstation replay uses exact rational checks and 70-decimal-digit mpmath on the explicit inputs above, with no numerical grid used to establish a theorem. It checks the normal interior gap, the three continuous corner examples, the weighted NB boundary, the alias log-series roots, and the binomial plateau at selected sizes. The JSON records the inputs and computed values. The replay also checks all 90 anchors and 270 claims against the reviewed known-mask inventory. The independent atlas validator checks the proposed CSV schema, scope and evidence references. Each final run used one workstation CPU slot. [Replay](2026-11-27-trunc-est-math-data/check.py) · [Numerical results](2026-11-27-trunc-est-math-data/checks.json) · [Proposed rows](2026-11-27-trunc-est-math-data/proofs.csv)

From the worktree root, run:

```sh
data=dev/research/evaluations/2026-11-27-trunc-est-math-data
/tmp/umvl-cpu/run python3 -B "$data/check.py"
/tmp/umvl-cpu/run python3 -B dev/tools/existence_atlas.py --check-modifier-proofs "$data/proofs.csv"
```
