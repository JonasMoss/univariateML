---
id: bnbinom
inference_numeric:
  - regime: "identifiable interior submodel"
    available: true
    condition: "finite size > 0, alpha > 0 and beta > 0; any nonempty supported free subset except size = beta with both size and beta free"
    classification: "dev/research/evaluations/2026-11-04-dqm-c-report.md#b-beta-negative-binomial-exact-singular-set"
  - regime: "singular information"
    available: false
    condition: "finite size > 0, alpha > 0 and beta > 0; size = beta with both size and beta free"
    reason: "DQM holds but the free information has a null direction; ordinary observed or sandwich covariance is unavailable"
    classification: "dev/research/evaluations/2026-11-04-dqm-c-report.md#b-beta-negative-binomial-exact-singular-set"
name: Beta negative binomial
support: {type: integer, lower: 0, upper: inf, closed: [true, false]}
params:
- name: size
  symbol: 'r'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: alpha
  symbol: '\alpha'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: beta
  symbol: '\beta'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dbnbinom
  sampler: rbnbinom
  control:
    gradtol: "Requested relative coefficient accuracy for all-free fits (default 1e-10); checked by the final observed-information Newton correction."
    iterlim: "Work limit: 1 * min(iterlim, 100000) solver iterations per solve (scalar refinement, or BFGS per deterministic start); 128 * min(iterlim, 100000) certificate evaluations per scalar, pair, all-free stationary cover, alpha-interval cover, negative-binomial face, or supporting-polynomial certificate (including each Bernstein coefficient update). Pair root proposals and isolation have the same solver-iteration limit."
log_density: "loggamma(size+x)-loggamma(size)-loggamma(x+1)+loggamma(alpha+size)+loggamma(beta+x)-loggamma(alpha+size+beta+x)-loggamma(alpha)-loggamma(beta)+loggamma(alpha+beta)"
limits:
  - model: nbinom
    firmness: proved
    when: 'The shapes satisfy $\alpha,\beta\to\infty$ with their ratio and $r>0$ fixed. Exchanging $r$ and $\beta$ gives the corresponding path with $\beta>0$ fixed.'
    detect: 'With $r$ or $\beta$ held and the other shapes free, a supporting-polynomial test proves the negative binomial face wins. Otherwise the pair must beat the face and complete its finite cover, or stop numerically. With all shapes free, a signed concentration bound and complete stationary cover can prove the negative binomial face wins. An unresolved comparison retains the legacy numerical face or finite result, explicitly uncertified'
    status: NegativeBinomialLimit
  - model: pois
    firmness: proved
    when: 'All three shapes tend to infinity with $r\beta/\alpha$ tending to a finite positive mean.'
    detect: 'All coefficients are free. Supporting-polynomial tests, the compound-Poisson bound for equal frequencies at 0,...,4, or a signed mixed-Poisson bound with a complete stationary cover prove the Poisson face wins for some samples. Other unresolved all-free faces retain explicitly uncertified legacy comparisons'
    status: PoissonLimit
  - model: pois
    firmness: proved
    when: 'A free shape satisfies $r\to0$, $\beta\to0$, or $\alpha\to\infty$, while the other two coefficients stay fixed and positive.'
    detect: 'Every observation is zero and at least one coefficient is free.'
    status: PointMass
references:
- Irwin, J. O. (1975). The Generalized Waring Distribution. Part I. JRSS A 138(1), 18-31. doi:10.2307/2345247.
- Xekalaki, E. (1984). The Bivariate Generalized Waring Distribution and its Application to Accident Theory. JRSS
  A 147(3), 488-498. doi:10.2307/2981580.
- Johnson, N. L., Kemp, A. W. and Kotz, S. (2005). Univariate Discrete Distributions, third edition. Wiley. doi:10.1002/0471715816.
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mlbnbinom())
  mlbnbinom(x)

group: Poisson, binomial and negative binomial
construction:
- compounding
relations:
  mixture_of:
  - family: nbinom
    at:
      size: size
      prob: mix
    mixed: prob
    mixing:
      family: beta
      at:
        shape1: alpha
        shape2: beta
fit:
  method: multistart
  certified: 'partial'
  note: 'bnbinom/README.md, Scalar certificates and Finite all-free certificate; bnbinom/mle.hpp: Every scalar and pair subset retains its global certificate or structured stop. All-free finite results are certified when the compact superlevel and full stationary cover close to coefficient accuracy; a uniform inner size/beta enclosure over alpha intervals is retained when its certificate closes. Supporting-polynomial, equal-frequency compound-Poisson, and signed NB/Poisson face decisions with completed stationary covers are proved. Other all-free finite and NB/Poisson face results retain the legacy score-checked search and likelihood comparison, explicitly uncertified. All-zero point-mass decisions are proved; all-held evaluation is excluded from fitting certification.'
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model clustered event counts with power-law tails from beta mixing.
summary: Beta negative binomial provides a model for clustered event counts with power-law tails from beta mixing.
aliases:
- "generalized Waring"
- "inverse Markov–Pólya"
key_references:
- 3
existence:
  uniqueness: unknown
  statement: >-
    With all coefficients free, no general sample criterion for a finite
    maximum is asserted here. Negative binomial and Poisson limits can
    compete with finite fits. Exchanging $r$ and $\beta$ leaves the law
    unchanged, so an off-diagonal maximizer cannot give a unique labeled
    coefficient vector. All-zero samples have no finite MLE whenever any
    coefficient is free. Holding coefficients removes some limiting paths,
    and every one-free-shape fit has a unique finite maximum if at least one
    observation is positive. With alpha held and both other shapes free, a
    finite maximum exists for such samples, but labeled uniqueness need not
    hold. All two-free-shape subsets now return certified finite maxima or structured stops. For the alpha-free pairs a supporting-polynomial condition can prove the negative binomial supremum; other unresolved faces stop numerically. All-free finite fits can complete a global stationary-cover certificate. Otherwise the score-checked legacy finite or face comparison is retained as uncertified, including all-free negative binomial faces and Poisson faces outside the proved sufficient conditions. Holding all coefficients leaves only
    likelihood evaluation, including for all-zero data.
  reason: >-
    The probability mass is symmetric in $r$ and $\beta$ after expanding
    the beta and gamma factors. Its mass at zero is strictly below one
    at every finite positive parameter vector. It tends to one as a free
    $r$ or $\beta$ tends to zero, or as a free $\alpha$ tends to infinity,
    with the other coefficients fixed and positive. Comparing numerical
    stationary points with reachable limiting models does not establish
    that all finite maxima have been found. The scalar score and the
    conditional existence proofs appear below.
---

The beta negative binomial mixes a negative binomial success probability with a beta distribution. Also called the generalized Waring distribution, it models counts with power-law tails. The shape $r$ is the underlying negative binomial size. The shapes $\alpha$ and $\beta$ govern the beta mixing distribution for its success probability.

## d/p/q/r

For a near-Poisson mixture, a mixed Taylor difference of log Gamma avoids subtracting increments of order size when the zero-count log mass is of order one. It is used for alpha at least $10^6$ and $(r+\beta)/\alpha\le10^{-3}$. The degree-12 remainder is bounded geometrically; the omitted mixed Stirling correction is of relative order $\alpha^{-4}$. For $r,\beta\ge1$, the positive mass ratios satisfy $R_j\le(j+D)/(j+E)$ for $j\ge k$, where $D=r+\beta-1+(r-1)(\beta-1)/(k+1)$ and $E=\alpha+r+\beta$. If $E>D+1$, telescoping bounds the uncomputed upper tail by $p_kR_k(k+E)/(E-D-1)$. The sum closes at relative remainder $2\times10^{-14}$ or returns NaN after 100000 cells. The new route covers cuts below 10000; the existing far-count restrictions still apply.

For near-Poisson masses beyond the prefix, the rising-factorial form retains extended precision through the final subtraction. Its practical cancellation margin is $32\epsilon_{\rm ext}$ times the sum of absolute terms, plus four binary64 rounding units on the result and an $8\times10^{-15}$ Stirling allowance. If this cannot meet the density contract, the far-count gamma-ratio form is used only when its analogous $32\epsilon_{64}$ margin closes. Otherwise the mass is NaN. This closes, for example, the large-alpha cancellation at counts 10000 and $10^6$ without claiming accuracy in unresolved subtraction regions.

### Far-count beta normalization

The beta factor in the far-count tail prefactor is evaluated through a shifted gamma ratio, avoiding cancellation of large log-gamma values when a mixing shape diverges. This addresses the normalizing factor only; the extreme mixing-tail and quantile limitations recorded by the audit remain separate.

On the uniform mixing submodel (alpha=beta=1), the mass is exactly r/((r+x)(r+x+1)); evaluating this rational identity removes gamma cancellation at arbitrarily large r. Interchangeability of r and beta gives the corresponding symmetric case. The remaining beta=1 path uses a short shifted gamma ratio when its relative shift is at most one half.

### Extreme d/p/q arithmetic

When one count-shape is one, the survival function is the beta ratio B(x+1+r,alpha)/B(r,alpha). Computing it directly avoids complementing a lower sum whose rounding error can exceed the entire upper tail. Positive small gamma increments use log1p in their recurrence. For large-count masses in this submodel, NaN is returned if the estimated subtraction margin 16 epsilon times the sum of absolute log-beta terms exceeds the log-density tolerance.

When alpha and one count-shape are one, the two tails are the exact rational weights (x+1)/(r+x+1) and r/(r+x+1). Their log-odds form avoids subtracting two gamma increments near log(r), whose cancellation can spoil a small natural CDF even when the log-tail absolute error is tiny.

The unit count-shape CDF at zero uses alpha/(alpha+r) directly, including when alpha+r overflows. For counts below the existing 100000-term structural guard, the unit count-shape bridge sums the exact product hazard in scaled, compensated coordinates; its mass uses the preceding survival times alpha/(alpha+r+x). For larger counts the beta-ratio bridge returns NaN if x+1+r rounds back to r, if the practical log-beta subtraction margin exceeds the log-tail tolerance, or if a nonpositive/subnormal computed hazard cannot meet its resolution margin. Mass evaluations whose shape sum overflows the original rising-factorial kernel also return NaN. These named precision regions prevent a rounded beta-ratio difference from being reported as an exact zero probability.

## Details

The all-free local accuracy check uses the full three-dimensional observed
information in log shape coordinates, preserving the size/beta exchange convention.
It differentiates cancellation-resistant gamma increments and zero masses. Pair and
all-free global covers keep their previous enclosure target; the separate local
correction supplies the requested coefficient accuracy.
A scalar certificate whose rounding margin prevents the tighter enclosure retains
its previous bracket proposal only after a checked local Newton correction,
marked uncertified. See the
[accuracy validation](../../../../../dev/research/evaluations/2026-11-10-acc-allfree-report.md).

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-discrete2.md) describes the limiting paths and the scope of the boundary comparisons.

Any subset of $r$, $\alpha$ and $\beta$ may be held fixed. When $r$ and $\beta$ are both free, swapping them gives exactly the same distribution. The returned representative satisfies $r\le\beta$. No ordering is imposed when either is held.

On the diagonal $r=\beta$ the swap fixes the law, so the score along $(1,0,-1)$ in $(r,\alpha,\beta)$ vanishes and the Fisher information has rank 2 of 3. The diagonal is therefore a singular boundary of the identified set $r\le\beta$: Wald intervals for $r$ and $\beta$ separately are unreliable near it. Elsewhere in the interior the information is nonsingular ([classification](../../../../../dev/research/evaluations/2026-11-04-dqm-c-report.md)).

Multistart runs propose finite witnesses. Closed certificates verify their global coefficient enclosures or prove a limit decision. An all-free certificate that cannot close retains the legacy score-checked finite result or face comparison as uncertified; actual search failures stop. With all coefficients supplied, the likelihood is evaluated directly. For all-zero data, any free coefficient allows the likelihood to approach one only at an excluded boundary. With every coefficient held, the all-zero likelihood is evaluated at the supplied positive values.

## Scalar certificates

Hold two shapes at any finite positive values. For a sample containing a
positive count, the remaining shape has a unique finite global maximizer.
This is a single-crossing result for the score, not a claim of joint concavity.
Let $A(t)=n^{-1}\sum_i(1-e^{-x_i t})$. It is increasing and concave, with
$A(0)=0$ and $A'(0)>0$. The ordinary alpha score divided by $n$ is

$$
\int_0^\infty \frac{e^{-(\alpha+r+\beta)t}}{1-e^{-t}}
\left[(e^{rt}-1)(e^{\beta t}-1)-A(t)\right]dt.
$$
The bracket is strictly convex, starts at zero with negative derivative,
and tends to infinity. It changes sign exactly once, from negative to positive.
For the size score the corresponding integral is

$$
\int_0^\infty \frac{e^{-(r+\alpha)t}}{1-e^{-t}}
\left[(e^{\alpha t}-e^{-\beta t})A(t)-(1-e^{-\beta t})\right]dt.
$$
Its bracket has the sign of
$[1+(e^{\alpha t}-1)/(1-e^{-\beta t})]A(t)-1$.
The ratio is strictly increasing: its numerator is positive and strictly
convex from zero, and its denominator positive and concave from zero.
Thus this bracket also changes sign once from negative to positive.
The beta score follows by exchanging $r$ and $\beta$.

For any of these integrals, at a zero of the score, increasing the free shape
multiplies the integrand by a strictly decreasing exponential. Relative to
its value at the bracket's sign change, this increases the negative part
and decreases the positive part, so the score becomes strictly negative.
Decreasing the shape makes it strictly positive. There can be only one root.
The alpha score is asymptotic to $n/\alpha$ at zero and
$-\sum_i x_i/\alpha$ at infinity. The size score is asymptotic to
$\#\{i:x_i>0\}/r$ at zero and $-n\beta/r$ at infinity; beta is symmetric.
These endpoint signs prove root existence and global optimality. All-zero
samples instead have only the excluded point-mass supremum described above.

The implementation expands a bracket in log shape, then bisects while
preserving positive and negative score endpoints. It accepts only a bracket
of width at most $\min(\mathtt{gradtol},10^{-8})$ and returns its midpoint.
A sign is used only beyond $128\epsilon$ times the average sum of absolute
digamma terms. An unresolved midpoint triggers quarter-point comparisons;
unresolved progress or an unrepresentable shape stops numerically.
Expansion and refinement share at most
$128\min(\mathtt{iterlim},100000)$ score evaluations; refinement also has
$\min(\mathtt{iterlim},100000)$ steps. Exhaustion stops with a budget condition.
No small-score or optimizer fallback returns an unchecked estimate.

The certificate is exact-arithmetic conditional on these computed comparisons,
with the stated practical floating-point margin; it is not outward-rounded
interval arithmetic. Diagnostic witness likelihood bounds use
$10^{-10}n(1+|\ell|/n)$ around the evaluated value and leave the global upper
bound infinite. They do not certify a stopped candidate. Extreme cancellation
can produce a numerical stop even though a unique mathematical maximizer exists.

With alpha held and both other shapes free, a positive count also ensures
existence. If either shape tends to zero, its positive-count masses tend to
zero uniformly in the other shape, since
$p(x)\le (r)_x/x!$ and $p(x)\le(\beta)_x/x!$ for $x>0$.
If a shape tends to infinity while the other stays bounded below, the count
law stochastically escapes to infinity (use its beta-mixture representation
and symmetry). Every observed finite-count probability then tends to zero.
These alternatives exhaust escapes from compact subsets of the positive
quadrant, so every positive likelihood superlevel set is compact.
This proves attainment, without establishing uniqueness. On the golden sample with $\alpha=2$, the
finite witness $(r,\alpha,\beta)=(1,2,5)$ has log likelihood
$-1276.3456581776477$, while every diagonal point has log likelihood below
$-1276.4458273656892$. Compactness and symmetry therefore give at least two
distinct labeled global maximizers. The
[diagonal-exclusion reproducer](../../../../../dev/research/evaluations/2026-11-06-bnbinom-cert-data/nonuniqueness.py)
uses monotone mass-ratio bounds on 861 log-shape segments and bounds both
exterior tails. Its comparisons use 60-digit arithmetic with a $10^{-20}$
absolute log-likelihood margin, conditional on computed comparisons rather
than outward rounding. The witness need not itself be a maximizer. The
[certification report](../../../../../dev/research/evaluations/2026-11-06-bnbinom-cert-report.md)
records validation and the multivariate certification gaps.

## Alpha-held pair certificate

Write $b=\beta$, fix $a=\alpha>0$, and assume a positive count. The mixed
ordinary score derivative is
$\ell_{rb}/n=-n^{-1}\sum_i\psi_1(a+r+b+x_i)<0$.
Together with scalar single crossing, this makes the sign of either
coordinate score monotone in both coordinates: after a negative score,
increasing either coordinate preserves negativity; before a positive score,
decreasing either preserves positivity. The score itself need not decrease
in its own coordinate away from its root.

Here is an effective enclosure stronger than a qualitative compactness
argument for maximization. Let $L$ be any evaluated finite log likelihood.
Choose $l>0$ by halving until

$$
 S(l)=\sum_{x_i>0}\min\{0,\log (l)_{x_i}-\log(x_i!)\}<L.
$$

This bounds every likelihood with $r\le l$ or $b\le l$, uniformly in the
other shape. Next double $U$ until $\ell_r(U,a,l)<0$, with a resolved
negative sign. Scalar single crossing and the negative mixed derivative
then give $\ell_r(r,a,b)<0$ for $r\ge U,b\ge l$, and the symmetric result
for $b\ge U,r\ge l$. Thus clipping a large coordinate to $U$ strictly
increases likelihood. Every global maximum lies inside $[l,U]^2$; every
exterior point is either below $L$ or dominated by a point in that rectangle.
This avoids computing a potentially much larger superlevel rectangle.
If a superlevel enclosure itself is needed, the stochastic bound
$\ell\le n\log F_M(V;a,l)$ when either shape exceeds $V$ and the other
exceeds $l$ supplies it, where $M=\max x_i$ and
$F_M(V;a,l)=\sum_{k=0}^M p_k(V,a,l)\to0$.
Its terms obey
$p_{k+1}/p_k=(V+k)(l+k)/((a+V+l+k)(k+1))$;
doubling $V$ until this bound is below $L$ is effective in exact arithmetic.
The implementation uses the smaller stationary rectangle and the clipping
argument, so it does not need that finite-count sum.

The rectangle is covered in log coordinates. Symmetry removes boxes lying
strictly in $r>b$; boxes crossing the diagonal remain intact. A box
$[r_l,r_h]\times[b_l,b_h]$ has the likelihood upper bound

$$
 n\log p_0(r_l,a,b_l)+\sum_i\log
 \frac{(r_h)_{x_i}(b_h)_{x_i}}{(a+r_h+b_h)_{x_i}x_i!}.
$$

Here $p_0$ decreases in either shape and each mass ratio increases.
A bound below a finite witness excludes the box. Alternatively, a negative
coordinate score at its lower corner or a positive score at its upper
corner excludes every stationary point in the box, by the scalar theorem.
Remaining boxes are bisected, including any unexcluded part of the diagonal.

Local Newton and multistart calculations only propose roots; their parameter
window never restricts the cover. For a proposed center $z=(r,b)$, interval
Hessian entries follow from decreasing trigamma in

$$
 H_{rr}=\psi_1(a+r)-\psi_1(r)
 +n^{-1}\sum_i[\psi_1(r+x_i)-\psi_1(a+r+b+x_i)],\qquad
 H_{rb}=-n^{-1}\sum_i\psi_1(a+r+b+x_i),
$$

with $H_{bb}$ symmetric. On a box $B$ centered at $z$, an approximate inverse
$C=H(z)^{-1}$ defines $T(y)=y-C\nabla\ell(y)/n$.
The implementation bounds $I-CH(B)$ componentwise, then verifies a weighted
infinity-norm contraction and $T(B)\subset\operatorname{int}B$, including
the scalar-score uncertainty. The contraction theorem proves existence and
uniqueness of a stationary point. Negative definiteness throughout $B$
proves that it is its strict maximum. A second contraction box has relative
radius $\min(\mathrm{tol}_{\rm cover},10^{-8})/4$, with
$\mathrm{tol}_{\rm cover}=\max(\mathtt{gradtol},10^{-8})$; both boxes share the center, so
uniqueness identifies the same root and proves the coarse coefficient enclosure.
Only boxes contained in these certified root neighborhoods can bypass the
likelihood/score exclusions. Once the entire rectangle is covered, all global
maxima have been isolated and their likelihoods are compared. The likelihood
enclosure at each root includes the center evaluation margin and the Taylor
bound $n\sum_j(|g_j|+e_j)d_j+(n/2)\sum_{jk}\sup|H_{jk}|d_jd_k$ over
its narrow coefficient box, where $g$ and $e$ are average scores and margins
and $d$ contains its half-widths.

A unique winning root in $r\le b$ returns the canonical representative and
retains its swapped counterpart. Swapping is an exact identity, so it does
not cause an unresolved numerical tie. Competing distinct identified roots
whose likelihood intervals overlap stop with `ComparisonTie`. A diagonal
root must pass the same two-dimensional isolation and cover tests as any
other root; a diagonal scalar solve supplies only a diagnostic seam witness.
The information singularity there does not prevent the observed Hessian from
being strictly negative definite. Degenerate stationary points that cannot
be isolated remain numerical or budget stops, never unchecked estimates.

Likelihood comparisons use $10^{-10}n(1+|\ell|/n)$ plus $512\epsilon$ times the accumulated absolute log-gamma-ratio terms; Hessian intervals receive
$512\epsilon$ times the accumulated absolute trigamma terms, and score
comparisons use the scalar $128\epsilon$ margin above. The certificate is
exact in exact arithmetic conditional on the computed comparisons, with
these practical floating-point margins, rather than outward rounding.

## Alpha intervals for the inner profile

The alpha-held cover extends to a closed positive interval $[a_l,a_h]$.
Both size/beta scores increase in alpha, so their extremal corner signs use
$a_h$ at the lower size/beta corner and $a_l$ at the upper corner. A
uniform stationary rectangle follows from the same small-shape bound and
a negative size score at $(U,a_h,l)$. Its finite witness lower bound uses
$p_0$ at $a_l$ and count-ratio denominators at $a_h$; this bounds the
witness likelihood throughout the alpha interval.

The midpoint's certified global inner mode proposes a root neighborhood.
Decreasing trigamma bounds its Hessian uniformly in all three coordinates.
The fixed inverse Hessian at the center gives a contraction for every alpha
in the interval when both endpoint score residuals and all interval Hessians
map the same rectangle strictly into itself. Negative definiteness makes
the enclosed root a strict conditional maximum. The stationary cover then
excludes every other box uniformly, including the diagonal. Closure proves
that every conditional global maximum in the entire alpha interval is in
this neighborhood or its swap. Unresolved additional branches, wide
intervals and seams stop rather than being interpolated from endpoint fits.
Hessian margins are $512\epsilon$ absolute-term sums as for fixed alpha;
the witness lower bound uses $10^{-9}n$ times its recorded likelihood and
zero-mass magnitudes.

All-free finite proposals attempt this additional inner certificate on
$a[1-w,1+w]$, where $w=\min(10^{-6},\sqrt{\mathrm{tol}})$.
The binding-neutral result exposes `profile_alpha_interval`,
`profile_mode_lower` and `profile_mode_upper` (coefficient order size,
alpha, beta). The fixed-alpha certificate and the interval cover each have
$128\min(\mathrm{iterlim},100000)$ evaluations; nested root solves keep
the solver-iteration allowance. A closed slab certifies the inner profile enclosure.
The separate three-dimensional cover below certifies finite all-free fits even
when this additional slab cannot close; general winning NB faces remain unresolved.

## Alpha-free pair certificates

Fix $r>0$, write $a=\alpha$, $b=\beta$, $s=a+b$ and $p=a/s$.
The beta-mixture representation gives, with $\delta=\min(r,1)$,

$$
 p_x\le\min\{1,(r)_xp^\delta/x!\},\qquad
 P(X>0)\le\min\{1,\max(r,1)(1-p)\},\qquad
 p_x\le\min\{1,(b)_x/x!\}.
$$

The first bound uses concavity of $q^\delta$ under the beta mixing law;
the second uses $1-q^r\le\max(r,1)(1-q)$. Given a finite likelihood
witness with lower bound $L$, these inequalities give effective exclusions
$b\le l$, $p\le p_{\min}$ and $1-p\le q_{\min}$. The bounds are uniform
in the other free coefficient. Integrating
$|\psi(z+t)-\log z|\le(t+1)/z$ gives
$|\log(z)_c-c\log z|\le c(c+2)/(2z)$. Therefore

$$
 |\ell-\ell_{\rm NB}(r,p)|\le C/s,\qquad
 C=\frac12\sum_x n_x\left\{
 \frac{r(r+2)}{p_{\min}}+\frac{x(x+2)}{q_{\min}}+(r+x)(r+x+2)\right\}.
$$

If $L$ exceeds the certified fixed-size NB face upper bound $F$, all global
maxima lie in $[p_{\min}l,S]\times[l,S]$, where $S=2C/(L-F)$.
This also excludes every unbounded-alpha neighborhood outside the rectangle.
The cover uses increasing $p_0$ in $a$, decreasing $p_0$ in $b$, and the
increasing mass ratio $(b)_x/(a+b+r)_x$ in $b$ and decreasing ratio in $a$.
The mixed derivative
$\ell_{ab}/n=\psi_1(a+b)-n^{-1}\sum_xn_x\psi_1(a+b+r+x)>0$
orders scalar-score signs at opposite corners. These signs and the scalar
single-crossing theorem exclude boxes without stationary points.

For these coordinates the diagonal Hessian entries are

$$
 H_{aa}=\psi_1(a+r)-\psi_1(a)+H_{ab},\qquad
 H_{bb}=H_{ab}-\psi_1(b)+n^{-1}\sum_xn_x\psi_1(b+x).
$$

Decreasing trigamma supplies their interval bounds. Root isolation, relative
coefficient enclosures and likelihood comparisons use the contraction argument
and floating-point margins in the alpha-held section, now in $(a,b)$.
Every root surviving the completed cover is compared; unresolved identified
ties stop. A witness that does not beat the face cannot supply this
compactness certificate and is never returned as an estimate.

For a winning-face certificate, let $p^*=r/(r+\bar x)$ and $q^*=1-p^*$.
The derivative of

$$
 D(t)=\frac{(1-t)^r}{(p^*)^r}\sum_xn_x(t/q^*)^x
$$

has polynomial coefficient signs
$(k+1)n_{k+1}-(k+r)q^*n_k$. A single change from positive to negative
proves its unique positive stationary point is $q^*$ and $D(t)\le n$.
Concavity of log likelihood in the mixing distribution then proves the NB
face is an upper bound for every mixture. A nondegenerate beta mixing law
has strictly smaller likelihood, so the supremum is not attained. The first test checks sparse coefficients without enumerating the count range.
If it fails, a bounded quotient-polynomial test below can still prove the face. This sufficient condition
includes positive constant and binary samples; it is not a complete
classification of winning NB faces.

Holding $\beta=h$ instead transports the actual parameter triple by
$(r,a,b)\mapsto(b,a,r)$, with canonical held size $h$. The NB face thus has
size $h$, and the two free coefficients are transported back without sorting.
Holding size and holding beta at different values are different fitting
problems.

## All-free face bounds

Every BNB law is mixed Poisson. A supporting-polynomial test with coefficient
signs $(k+1)n_{k+1}-\bar x n_k$ proves a winning Poisson face when those
signs change once from positive to negative. It uses the same concavity
argument with $D(\lambda)=e^{\bar x-\lambda}\sum_xn_x(\lambda/\bar x)^x$.
The single-sign-change condition is sufficient but not necessary. Write either
face derivative in the normalized variable $y$ with its known fitted root at
$y=1$. For Poisson, its polynomial is
$P(y)=\sum_k[(k+1)n_{k+1}-\bar x n_k]y^k$; for held size $r$, replace
$\bar x n_k$ by $(k+r)q^*n_k$. In exact arithmetic $P(1)=0$.
For $Q(y)=P(y)/(1-y)$, the coefficient of $y^j$ is the prefix sum of
these derivative coefficients. If $Q>0$ on $[0,\infty)$, the face directional
likelihood increases up to $y=1$ and decreases afterwards. This proves the
same strict mixture bound; for NB the required domain is the smaller
$0<y<1/q^*$.

For degree $m$ quotient coefficients $Q_j$, the transformed polynomial
$(1-z)^m Q(z/(1-z))$ has Bernstein coefficients
$Q_j/\binom{m}{j}$ on $[0,1]$. Positive coefficient lower bounds certify
positivity throughout an interval; de Casteljau subdivision can establish
this even when the original coefficients have mixed signs. The test stops
without a proof on unresolved endpoints, unrepresentable coefficients or
exhaustion of its certificate allowance. Failure of this sufficient test
says nothing about which face wins. The existing search then continues.

Coefficient prefix sums use a $(1024+16h)\epsilon$ absolute-term margin,
where $h$ bounds the number of accumulated support terms; inverse binomial
weights accumulate an additional $8\epsilon$ relative margin per update;
subdivision adds $8\epsilon$ times the absolute parent endpoints at each
update. These are practical floating-point margins under the conditional
exact-arithmetic contract, not outward rounding. Dense storage is bounded
before allocation by the same $128\min(\mathrm{iterlim},100000)$ allowance
that counts construction, subdivision nodes and individual coefficient updates.

For example, counts $n_0=3,n_1=10,n_3=1$ give
$Q(y)=(101-29y+13y^2)/14$ for Poisson. Its negative discriminant
proves strict positivity despite three derivative coefficient sign changes.
Both the all-free Poisson face and the held-size-one NB face are certified
for this sample. Winning all-free faces not passing the sufficient conditions below or these tests
retain the legacy comparison, explicitly marked uncertified.

Equal positive frequencies at $0,1,2,3,4$ have a separate exact sufficient
condition. Every BNB is compound Poisson because its Poisson mixing variable
is a product and ratio of independent gamma variables, hence has an HCM
density and a generalized-gamma-convolution law
([Behme and Bondesson, Propositions 1 and 3](https://arxiv.org/abs/1507.04017)).
Over compound-Poisson laws the equal-frequency likelihood is uniquely
maximized by Poisson(2). After removing jumps above four and optimizing an
exponential tilt, the mean-two simplex has a radial dispersion derivative
whose negative numerator is a degree-11 polynomial. Homogenization and four
degree elevations give 759 positive coefficients and 57 zero coefficients,
proving its nonnegativity on the entire simplex. Strictness near the Poisson
endpoint excludes every nonzero dispersion ray. The exact algebra and its
runtime frequency condition are documented in the
[derivation](../../../../../dev/research/evaluations/2026-11-08-bnbinom-allfree-data/derivations.md#equal-frequencies-through-four-a-stronger-poisson-theorem).
This proves the Poisson face even though its mixed-Poisson supporting test
fails: at zero that test gives $D(0)=e^2>5$.

Uniform exterior bounds are nevertheless available. Put
$m=rb/(a+r+b)$ and $\lambda=rb/a$. The trigamma integral for $\log p_0$
and the count recurrence give

$$
 \log p_x\le-m+\log(m)_x-\log x!,\qquad
 \ell\le\ell_{\rm Pois}(\lambda)+
 \left(n\lambda^2+\tfrac12\sum_xn_xx(x-1)\right)(1/r+1/b).
$$

The first upper bound decreases to minus infinity once $m>\bar x$, so a
witness gives an effective $m<M$. If $r,b\ge U\ge4M$, then
$1/m=1/\lambda+1/r+1/b$ gives $\lambda\le2M$. The second remainder is
then bounded by $2[4nM^2+\sum_xn_xx(x-1)/2]/U$, uniformly as alpha varies.
A witness beating Poisson therefore bounds $\min(r,b)$ above. After ordering
$r\le b$, the small-shape bound and this cutoff put $r$ in a compact
positive interval. The pair bounds above can then be made uniform in $r$,
using its upper endpoint for rising factorials and its lower endpoint in
$\delta$. A witness beating the globally fitted NB face supplies an effective
finite superlevel box. These inequalities supply the exterior exclusions for the all-free
stationary cover; they do not resolve a winning NB face. The
[derivations and independent checks](../../../../../dev/research/evaluations/2026-11-08-bnbinom-joint-data/derivations.md)
state the bounds and remaining obligations.

## Finite all-free certificate

A finite witness strictly above certified upper bounds for both fitted faces
makes the effective canonical superlevel box compact. The NB face is
evaluated on its stable mean profile; its height bound includes the delegated
size-root enclosure error, not just likelihood evaluation error. The cover retains
$r\le b$ without discarding boxes crossing the diagonal. Monotone corner
score signs, uniform alpha-mode brackets, the mass-ratio bounds and local
NB-neighborhood remainders exclude boxes. Every global maximum is an
interior stationary point and survives those exclusions.

In small boxes the full three-coordinate average-log-likelihood Hessian is bounded using decreasing
trigamma. For any fixed matrix $M$ and center $c$, a stationary point $p$
satisfies

$$
 p=c-MS(c)+(I-M\bar H)(p-c),
$$

where $\bar H$ is an average Hessian along the segment from $c$ to $p$.
The computed inverse center Hessian merely chooses $M$; the identity and
root exclusions do not require that inverse to be accurate. The enclosure
is also intersected with each box before splitting it further. Interval bounds
on the right side exclude boxes or shrink their stationary-point enclosures.
Hessian bounds use $1024\epsilon$ absolute-term margins. Matrix products
receive $64\epsilon$ absolute-term margins and the resulting mapping
receives $4096\epsilon$ times its accumulated absolute terms.

Newton proposes a center only. The cover must close with every remaining
canonical global maximum in a box of relative radius
$10^{-8}/4$ about that center. This preserves the global cover's enclosure target independently of the tighter local accuracy control. Compactness and a
nonempty witness superlevel ensure that a global maximum exists there.
The final observed-information Newton correction supplies the requested
local coefficient accuracy. The exported global coefficient box retains the
cover center and radius; it is not recentered on the polished estimate. The
swapped box supplies the other label. If this cover cannot close, the successful
legacy result is retained as uncertified. A proved strict advantage overrides a
legacy face choice; an actually failed legacy solve still stops. The diagonal
witness remains available independently of selection.

The Newton proposal and each coarse alpha bracket have
$\min(\mathrm{iterlim},100000)$ solver steps. Cutoff tests and covering
boxes share $128\min(\mathrm{iterlim},100000)$ certificate evaluations.
Bounds for a winning face require a different argument when the strict
witness gap vanishes. These unresolved comparisons retain the legacy likelihood
comparison with its $10^{-8}n$ face tolerance and Poisson tie preference,
explicitly uncertified. Proven strict finite or face inequalities override that
preference. A finite legacy candidate must pass the existing finite-difference
average-score gate and lie strictly inside the search range.

`global_loglik_interval` records the certified maximum-likelihood bounds.
`global_coefficient_lower` and `global_coefficient_upper` enclose every
canonical global maximizer; reflecting size and beta gives the other labels.
These fields are available through the binding-neutral result, public JS fit
and internal R adapter result. The proof is conditional exact arithmetic with
the stated practical floating-point margins, rather than outward rounding. The
[derivations and reproducible checks](../../../../../dev/research/evaluations/2026-11-08-bnbinom-allfree-data/derivations.md)
include the independent high-precision roots and nonuniqueness witness.

## Signed face comparison

Put $s=\alpha+\beta$, $p=\alpha/s$, $q=1-p$, and
$F_2=\sum_i x_i(x_i-1)$. The exact log-gamma expression yields

$$
 \ell_{\rm BNB}(r,sp,sq)-\ell_{\rm NB}(r,p)
 =\frac{C(r,p)}s+E,\qquad
 C(r,p)=\frac12\left\{nr(r-1)\frac qp+F_2\frac pq-2r\sum_i x_i\right\}.
$$

For $H(u)=u^3/6+u^2/4+u/12$,

$$
 |E|\le\frac1{s^2}\sum_i\left\{
 \frac{H(r)}{p^2}+\frac{H(x_i)}{q^2}+H(r+x_i)\right\}.
$$

This holds for all positive shapes, uniformly on any bounded size interval
and any closed subinterval of $0<p<1$. At the fitted NB law the coefficient
of $1/\alpha$ is $c_1=pC$. If $c_1<0$, continuity supplies a neighbourhood
where the signed bound is negative for sufficiently large concentration.
Outside it, the strict NB likelihood deficit dominates the absolute
remainder. The certified NB size profile and its exact fitted probability
supply the deficit bound. Exchanging $r$ and $\beta$ proves the other
labelling without an additional asymptotic assumption.

For the Poisson corner, put $\lambda=r\beta/\alpha$ and
$h=1/r+1/\beta$. For $0<h\le h_0$,

$$
 \ell_{\rm BNB}\le\ell_{\rm Pois}(\lambda)+B(\lambda)h+R(\lambda,h_0)h^2,
$$

where

$$
 B(\lambda)=\frac n2\{(\lambda-\bar x)^2+v-\bar x\},\qquad
 R(\lambda,h_0)=\frac{\lambda^2}{2}\sum_i x_i
 \left(1+\frac{(x_i-1)_+h_0}{4}\right)^2.
$$

Thus strictly underdispersed samples have a signed exclusion near the fitted
Poisson mean. A zero or unresolved dispersion sign does not establish one.

The global comparison orders $r\le\beta$ and covers the compactified
coordinates $(r,\log(p/q),1/s)$. Small-shape, extreme-probability and
Poisson-corner inequalities exclude the exterior. Signed bounds handle the
NB boundary $1/s=0$. Every remaining finite global maximizer must be
stationary; likelihood bounds, monotone score signs and the full Hessian
root exclusion cover its remaining region. A certificate succeeds only when
every box is excluded. In particular a negative asymptotic coefficient
alone never certifies a global face win. The examples with equal frequencies
at zero and five pass this complete comparison.

Each signed cover has $128\min(\mathrm{iterlim},100000)$ evaluations.
Failure to close, including a zero sign, retains the existing uncertified
result. The signed cover does not change the finite search or its face tolerance.
The additional signed arithmetic uses $4096\epsilon$ absolute-term or relative
margins; the inherited likelihood, score and Hessian margins also apply.
The [derivation and independent checks](../../../../../dev/research/evaluations/2026-11-09-bnbinom-face-data/derivations.md)
state the conditional exact-arithmetic claim and the limits of numerical evidence.

## R notes

Supply `size`, `alpha` or `beta` to hold that coefficient. A detected negative binomial limit recommends `mlnbinom`, holding its size at the supplied `size` or `beta` when either was held. A Poisson limit or all-zero free fit recommends `mlpois`. All-free fits attempt the global stationary cover and retain uncertified legacy finite or face decisions when certification cannot close. One-free-shape fits and every two-free-shape route use certificates; their budget and arithmetic stops carry finite diagnostic witnesses when available.

Failed negative-binomial solvers still stop, preserving their typed witnesses.
Successful all-free comparisons expose `certified` through the existing scalar
extra channel (0/1 in JS, logical in the R adapter). The R estimate retains a
logical `certified` attribute through the standard constructor. Limit conditions
state `Certified global comparison` or `Uncertified legacy comparison`; R
conditions also carry the logical flag. Both face solvers use the caller's iteration allowance.
