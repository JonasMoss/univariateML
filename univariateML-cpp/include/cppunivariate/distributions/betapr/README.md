---
id: betapr
inference_numeric:
  - regime: primary interior
    available: true
    condition: "finite shape1 > 0, shape2 > 0 and scale > 0; any regular subset free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Beta prime
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
- name: shape1
  symbol: 'p'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: shape2
  symbol: 'q'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: scale
  symbol: 'b'
  role: scale
  domain: (0, inf)
  simulate: 1.0
  kind: regular
r:
  density: dbetapr
  sampler: rbetapr
  control:
    shape1: Held positive finite coefficient; NULL estimates it.
    shape2: Held positive finite coefficient; NULL estimates it.
    gradtol: "All-free requested relative coefficient accuracy, capped at 1e-6, and proposal score tolerance. Fixed-scale and non-unit held-shape certificates retain their documented accuracy bounds."
    reltol: Relative objective tolerance for proposals.
    abstol: Proposal step tolerance in log coordinates after geometric-mean normalization.
    shape10: Optional positive starting value for shape1 (zero selects automatic starts).
    shape20: Optional positive starting value for shape2 (zero selects automatic starts).
    scale0: Optional positive starting value for scale (zero selects automatic starts).
    iterlim: "Default 2000. Work limit per solve: 1 * min(iterlim, 100000) solver iterations; per certificate: 128 * min(iterlim, 100000) certificate evaluations."
log_density: (shape1-1)*log(x/scale) - log(scale) - (shape1+shape2)*log(1+x/scale) - loggamma(shape1) - loggamma(shape2) + loggamma(shape1+shape2)
limits:
  - {firmness: proved, model: exp, when: 'With shape1 held at one, shape2 and scale diverge at fixed ratio.', status: ExpBoundary, fixed: [shape1], numerical: true}
  - {firmness: proved, model: invexp, when: 'With shape2 held at one, shape1 diverges and scale vanishes at fixed product.', status: InvExpBoundary, fixed: [shape2], numerical: true}
  - firmness: proved
    name: positive log-variance
    when: Both free shapes $p$ and $q$ tend to infinity with their ratio and scale chosen to concentrate at the common observation.
    detect: All observations are equal and both shapes are free.
    status: InvalidStart
  - firmness: proved
    model: gamma
    fixed: [shape1]
    when: With $p$ held, $q$ and $b$ tend to infinity while $b/q$ tends to a positive gamma scale.
    detect: Constant data select this limit analytically; nonconstant data require the signed gamma-tail and interior profile certificate. Unresolved comparisons stop.
    status: GammaBoundary
    numerical: true
  - firmness: proved
    model: invgamma
    fixed: [shape2]
    when: With $q$ held, $p$ tends to infinity and $b$ tends to zero while $bp$ tends to a positive inverse-gamma scale.
    detect: Constant data select this limit analytically; nonconstant data use the reciprocal signed gamma-tail and interior certificate. Unresolved comparisons stop.
    status: InvGammaBoundary
    numerical: true
  - model: gamma
    firmness: proved
    when: The shape $q$ tends to infinity while $p$ tends to a positive finite shape and $b/q$ tends to a positive gamma scale.
    detect: The signed joint gamma tail and compact interior cover exclude finite competitors; likelihood bounds separate this face from the inverse-gamma face. Unresolved comparisons stop.
    status: GammaBoundary
    numerical: true
  - model: invgamma
    firmness: proved
    when: The shape $p$ tends to infinity while $q$ tends to a positive finite shape and $bp$ tends to a positive inverse-gamma scale.
    detect: Reciprocal signed tails and the compact interior cover exclude finite competitors; likelihood bounds separate this face from the gamma face. Unresolved comparisons stop.
    status: InvGammaBoundary
    numerical: true
references:
- Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995). Continuous Univariate Distributions, Volume 2, Chapter 25.
  Wiley, New York.
- 'Wolodzko, T. and Klinke, S. (2026). extraDistr: Additional Univariate and Multivariate Distributions. R package
  reference manual, version 1.10.0.5. https://cran.r-project.org/web/packages/extraDistr/extraDistr.pdf.'
examples: |
  set.seed(1)
  x <- rml(200, mlbetapr())
  mlbetapr(x)

group: beta and generalised beta
construction:
- base
relations:
  special_case_of:
  - family: gb2
    at:
      shape1: shape2
      shape2: 1
      shape3: shape1
      scale: scale
fit:
  method: profile-search
  certified: "yes"
  note: 'betapr/README.md, Details; betapr/mle.hpp, centered.hpp, certificate.hpp and joint_certificate.hpp; betapr/test.cpp: fixed-scale beta concavity, unit-shape delegates and non-unit held-shape certificates cover conditional subsets. The all-free fitter combines uniform harmonic-shape exteriors, signed gamma/inverse-gamma tails and a compact cover of every possible conditional maximizer. Shape-gradient bounds, concave supporting planes and interval-Newton inclusions exclude cells or enclose their unique maxima. Finite fits require relative enclosures in both shapes and scale of at most min(tol, 1e-6); faces require complete interior exclusion and separated likelihood bounds. Unresolved arithmetic, ties and exhausted shared budgets stop with witnesses.'
areas:
- actuarial
- economics and income
classification: specialist
classification_reason: Used to model positive ratios with separate lower-tail and upper-tail shapes.
summary: Beta prime provides a model for positive ratios with separate lower-tail and upper-tail shapes.
aliases:
- "beta of the second kind"
- "inverted beta"
- "scaled beta prime"
- "scaled F"
- "Pearson type VI"
key_references:
- 2
existence:
  uniqueness: unknown
  statement: >-
    With all coefficients free, constant positive data have unbounded likelihood.
    For nonconstant data the gamma and inverse-gamma limits can compete with
    finite fits. The joint exterior bounds prove existence of a finite maximum
    when a finite witness exceeds both computed face upper bounds.
    A closed-form joint sample criterion and joint uniqueness are
    not established. A certified compact cover determines finite fits and winning faces when computed comparisons resolve; otherwise it stops with witnesses. With scale held, a unique finite MLE exists exactly when
    the sample is nonconstant if both shapes are free. With scale and either
    shape held, the remaining shape has a unique finite maximum even for
    constant data. With both shapes held, scale has a unique finite maximum
    for every positive sample. With just $p$ held, the gamma limit remains;
    with just $q$ held, the inverse-gamma limit remains. Constant samples
    select these respective limits. A closed-form sample criterion is not established;
    conditional global uniqueness can fail, as the two-peak construction below shows.
    Holding all coefficients leaves likelihood evaluation.
  reason: >-
    With scale $b$ held, $y_i=x_i/(b+x_i)$ transforms the likelihood to a
    beta likelihood with a data-only Jacobian. Strict concavity in the beta
    natural parameters gives uniqueness, and nonconstant interior data give
    existence. A single beta shape has a strictly decreasing digamma score
    that changes sign. With both shapes held, the log-scale score strictly
    decreases from $nq$ to $-np$, so it has one root. Gamma-ratio asymptotics
    establish the two limiting densities, but do not decide whether either
    dominates all finite fits.
---

The beta prime distribution models positive ratios. The scale $b$ sets the
measurement scale. The shape $p$ controls the lower-tail power and $q$
controls the polynomial upper tail.

## Details

Fitting requires a nonempty positive finite sample. The
[family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-halfthreshold.md)
describes the limiting paths and boundary comparisons.
With $p$ fixed, $q\to\infty$ and $b/q\to\theta>0$ give a gamma density
with shape $p$ and scale $\theta$. With $q$ fixed, $p\to\infty$ and
$pb\to\theta>0$ give an inverse-gamma density with shape $q$ and scale
$\theta$. These follow from $\Gamma(p+q)/\Gamma(q)\sim q^p$ in the first
case and the corresponding ratio with $p$ and $q$ interchanged in the second.
The pointwise density limits for $x>0$ imply total-variation convergence by
Scheffé's lemma. They do not certify which limit wins a likelihood comparison.

Holding scale excludes these nondegenerate escapes. The transformation
$y=x/(b+x)$ gives a beta likelihood, with a unique finite maximum for
nonconstant data. Constant data instead have unbounded likelihood as both
shapes grow with their ratio chosen to concentrate at the observation.
At held scale, sending only $q$ to infinity concentrates mass at zero;
sending only $p$ to infinity sends mass to infinity. Sending both to infinity
with finite positive ratio gives a point mass, rather than gamma or inverse gamma.

At held scale, holding one shape leaves a strictly decreasing digamma score.
Holding both shapes gives a strictly decreasing log-scale score with a unique
root, even for constant data. Holding just one shape leaves the corresponding
gamma or inverse-gamma escape. The limit comparison retains that shape and
optimizes the limit scale. For constant data, profiling scale gives a likelihood
strictly increasing in the remaining shape, so the supremum is the limit.
For example, with $p$ held its derivative per observation is
$\psi(p+q)-\psi(q)-\log((p+q)/q)>0$.
The inequality follows by integrating $\psi'(t)>1/t$ from $q$ to $q+p$.
Reciprocation proves the other case.

## Non-unit held-shape certificate

For held $a=p$, write $b=q$ and normalize observations to arithmetic mean
one. At fixed $b$, the log-scale score
$-a+(a+b)\overline{x/(s+x)}$ strictly decreases, and its unique zero
$s(b)$ lies in $[b x_{\min}/a,b x_{\max}/a]$. Implicit differentiation
shows $s(b)$ strictly increases. The profile derivative is
$D(b)=\psi(a+b)-\psi(b)-\overline{\log(1+x/s(b))}$.
On $[L,U]$ it is bounded below by
$\psi(a+U)-\psi(U)-\overline{\log(1+x/s(L))}$ and above by
$\psi(a+L)-\psi(L)-\overline{\log(1+x/s(U))}$.
Integrating these bounds encloses the profile from either endpoint.
Signed intervals contain no stationary maximum. Subdivision retains every
other interval unless its value upper bound is below a finite witness.
No unimodality assumption is used. For flat profiles a sharper enclosure
uses the Schur complement
$D'(b)=\psi'(a+b)-\psi'(b)+a^2/\{(a+b)^3Q(b)\}$, where
$Q(b)=\overline{q_i(1-q_i)}$ and $q_i=x_i/(s(b)+x_i)$.
Each $q_i$ decreases on a shape interval. The products $q_i(1-q_i)$ at its two
endpoint values give the minimum, and the larger product or $1/4$ (if the
endpoint values straddle $1/2$) the maximum.
The trigamma difference increases with $b$. These facts enclose $D'$;
integrating from either endpoint tightens the derivative bounds. This
avoids treating the two nearly cancelling terms in $D$ independently
throughout a flat root neighborhood.

For the lower exterior, $\psi'(t)>1/t^2$ gives
$bD(b)\ge a/(a+L)-L\overline{\log(1+a x/(Lx_{\min}))}$ for
$0<b\le L$. A positive right side excludes this entire exterior.
For $b\ge B>2a x_{\max}$, the scale equation also implies
$s(b)\ge b/a-x_{\max}$, hence $r=b/s(b)\le2a$.
Using $\psi(t)<\log t$ and
$\log(1+z)\ge z-z^2/2$ bounds the profile by
$\ell_\Gamma+(a^2+2a^2\overline{x^2})/B$ per observation.
Thus a finite witness strictly above this bound excludes the whole upper
exterior. The second-order bound below also gives the uniform remainder
$\max(0,A/B+C/B^2)$; the search uses the smaller upper bound. To select a gamma face, use the sharper bound
$\ell(b)\le\ell_\Gamma+A/b+C/b^2$, where
$A=a\{a(\overline{x^2}-1)-1\}/2$ and
$C=a^2/4+2a^3x_{\max}(1+2\overline{x^2})+2a^3\overline{x^2}$.
Indeed, the digamma integral gives $\psi(t)<\log t-1/(2t)$, hence
$\log\Gamma(b+a)-\log\Gamma(b)-a\log b
\le a(a-1)/(2b)+a^2/(4b^2)$.
The scale equation gives $a\le r\le2a$ and
$r-a\le2a^2x_{\max}/b$. Expanding the log-density with the same
logarithm inequality bounds its $1/b$ coefficient by
$A+2a^3x_{\max}(1+2\overline{x^2})/b$; the remaining term is at most
$2a^3\overline{x^2}/b^2$. If $A<0$ and $B>2C/(-A)$,
the entire tail lies strictly below the gamma face. The face is selected only
when every interior stationary interval is also excluded. The sign of $A$
alone is not a sample criterion for which model wins. A near-zero $A$, slow
escape, or unresolved interior comparison can still produce a numerical or
budget stop.

The certificate is valid in exact arithmetic conditional on the computed
comparisons. Its practical floating-point allowance per datum is
$512\epsilon(1+|\ell|+|\log B(a,b)|+a+b)$; arithmetic with allowance
above $10^{-7}$ stops. Subnormal means or face scales with relative spacing
above $10^{-7}$ also stop; finite scale reconstruction must retain relative
accuracy $10^{-6}$. Derivative enclosures use separate increment errors plus
$128\epsilon(|g(L)|+|g(U)|+m(L)+m(U))+
2a\{\delta_L/(a+L)+\delta_U/(a+U)\}$,
where $g(b)=\psi(a+b)-\psi(b)$,
$m(b)=\overline{\log(1+x/s(b))}$, and $\delta$ bounds log-scale
root error. Compensated sums accumulate these means and the scale score.
For $b<32$, $g$ uses the special-function difference with allowance
$64\epsilon(1+|\psi(a+b)|+|\psi(b)|)$.
For $b\ge32$, differences of the digamma expansion through $b^{-8}$
are evaluated with `log1p` and `expm1`; the remainder is bounded by
$\{b^{-10}+(b+a)^{-10}\}/132$, plus $64\epsilon|g|$ rounding allowance.
The alternating remainder follows by expanding the denominator in the
positive Binet integral for digamma. This family helper avoids subtracting
two rounded logarithmic digamma values; it does not change the shared seam.

Scale roots use log brackets narrower than
$8\epsilon(1+|\log s|)$; $\delta$ includes that rounding allowance
and the final bracket width. Stationary intervals are refined to relative
shape and scale width $10^{-10}$; surviving intervals must together fit
within relative width $10^{-6}$, otherwise the result is a comparison tie.
The Schur-complement upper bound must also be negative across that cluster.
This certifies a unique maximum for a successful finite fit, to golden
coefficient accuracy; it does not assert uniqueness for arbitrary samples. The leading tail sign requires a margin $10^{-10}(1+a^2\overline{x^2})$.
Solver and certificate caps follow `iterlim`; exhaustion never
returns an unchecked estimate. Witnesses and likelihood bounds are retained.
Reciprocation exchanges the shapes, reciprocates scale, converts the gamma
face to inverse gamma, and adds $-2\sum\log x$ to all likelihood fields,
including the last unresolved segment. A compensated Jacobian sum adds
allowance $64\epsilon(1+2\sum|\log x|)$ to the transported bounds.

### Nonuniqueness of conditional maxima

Held $p=0.22690393135372058$ with the nine observations in the
[counterexample data](../../../../../dev/research/evaluations/2026-11-07-betapr-prof-data/counterexample-refined.json)
gives local maxima near $q=0.09175915$ and $q=0.22150182$.
Their total log likelihoods are $-29.14985912$ and $-28.55611432$;
thus the conditional profile is not unimodal.

Multiply only the last observation by $e^t$. The certificate selects a
maximum above $q=0.12$ at $t=5.6463$, and below $q=0.12$ at $t=5.6466$.
Throughout this interval the profile value at $q=0.12$ is at least $0.17569$
below a finite witness, and the gamma face is at least $17.33799$ below it.
To obtain these uniform bounds, changing the log of one observation changes
the total likelihood at fixed $q,s$ at rate
$p-1-(p+q)x/(s+x)$, of magnitude at most $1+q$ here.
The profiled gamma value has rate bounded by $1+np$.
Apply these Lipschitz bounds to the central witness, valley and face values
in the linked data. Both shape exteriors are uniformly excluded on this
compact data interval. The maxima on either side of $q=0.12$ therefore
exist and depend continuously in value on $t$. Their ordering reverses,
while the valley cannot maximize; at some intervening $t$ there are at
least two separated global maxima. This is an exact-arithmetic existence
argument conditional on the endpoint certificates and computed gap bounds,
under the arithmetic contract above. The numerical crossing
$t\approx5.64643839503$ has peaks near $q=0.08352763$ and $q=0.16883269$;
that rounded data set produces a comparison-tie stop, not a uniqueness claim.

The all-free fitter covers the joint exterior and every remaining finite
stationary maximum. Its proposals do not determine acceptance.

### Joint exterior bounds

Put $y_i=\log x_i$, $h=pq/(p+q)$ and $\bar y=n^{-1}\sum y_i$.
The density of $Y=\log X$ has mode $m=\log b+\log(p/q)$,
log derivative between $-q$ and $p$, and mode height at most $h$.
Indeed its density is at least its mode height times $e^{-p(m-y)}$
to the left and $e^{-q(y-m)}$ to the right; integrating proves the
height bound. Consequently, uniformly over scale,
\[
 \ell(p,q,b)\le n\log h-\sum_i y_i.
\]
A verified likelihood witness $L$ excludes
$h\le\exp((L+\sum y_i)/n)$ from strictly better fits.

Stirling's remainder $0<w(z)<1/(12z)$ gives the sharper mode-height
bound $\log g(m)\le\tfrac12\log(h/(2\pi))+1/(48h)$.
At distance $d$ from the mode, the negative log-density curvature is
at least $h e^{-d}$. Integration gives a loss at least
$h\rho(d)$, where $\rho(d)=d-1+e^{-d}\ge d^2/(2(1+d))$.
The last inequality follows by integrating
$1-e^{-d}\ge d/(1+d)$, and differentiating
$d-\log(1+d)-d^2/(2(1+d))$.
For two observations separated by $D$ in log coordinates, convexity and
evenness of $\rho(|\cdot|)$ give a combined loss at least
$2h\rho(D/2)\ge hD^2/(4+2D)$, whatever the mode.
Pair the ordered smallest and largest logs, then the next smallest and
largest, and so on. With $J$ the sum of these pair contributions,
\[
 \ell(p,q,b)\le
 U(h):=\frac n2\log\frac{h}{2\pi}+\frac{n}{48h}
       -hJ-\sum_i y_i.
\]
For nonconstant data $J>0$, and $U$ decreases on
$h\ge\max(1,n/(2J))$ and tends to minus infinity. Doubling $H$ until
$U(H)<L$ excludes every $h\ge H$, uniformly in scale and shape ratio.
This closes the simultaneous two-large-shape corner, including paths
whose scale tends to zero or infinity.

The remaining large-shape tails admit a uniform, unsigned remainder.
For the gamma side normalize $z_i=x_i/\bar x$, write
$M=\max z_i$ and $V=n^{-1}\sum z_i^2$.
At fixed $p,q$, the unique maximizing scale $s$ in these units solves
$p=(p+q)\overline{z/(s+z)}$.
Since $\overline{z/(s+z)}\ge1/(s+M)$,
$q>2pM$ implies $r=q/s\le2p$.
Using $\log\Gamma(q+p)-\log\Gamma(q)\le p\log(q+p)$ and
$\log(1+t)\ge t-t^2/2$, dropping the additional negative term
$-p\sum\log(1+z_i/s)$, gives
\[
 \sup_b\ell(p,q,b)\le
 G(p)+\frac{np^2(1+2V)}q,
\]
where $G(p)$ is the gamma likelihood with shape $p$ and its maximizing
scale. If $h<H$ and $q\ge2H$, then $p<2H$. The small-$h$ bound also
gives $p>h_{\rm low}$. Thus throughout this tail the remainder is at most
$4nH^2(1+2V)/q$, uniformly over the other shape and scale.
$G$ is strictly concave because
$G''(p)=n(1/p-\psi_1(p))<0$; a supporting tangent bounds its maximum on
$[h_{\rm low},2H]$. Bisection tightens the tangent without needing to
assume convergence. If this upper bound is below $L$, another doubling
loop excludes all $q\ge Q$. Reciprocal log data give the corresponding
$p\ge P$ inverse-gamma bound with the exact likelihood Jacobian
$-2\sum\log x_i$. No floating-point reciprocals are required.

The public C++ diagnostic `BetaPrimeResult::joint_exterior` records the
exterior and cover work, coefficient enclosures and `global_certified`.
`FiniteExterior` by itself proves finite-MLE existence: competitive shapes
lie in a compact rectangle bounded away from zero, and their maximizing
scales lie in $[qx_{\min}/p,qx_{\max}/p]$. Continuity gives attainment.
The subsequent cover identifies its maxima. Selected faces and finite
successes set `global_certified` only after the comparisons below resolve.
Conditional certificates, including the interface used by GB2, are unchanged.

The bounds follow the package's exact-arithmetic, computed-comparison
contract. Log sums and pair sums are compensated. Pair gaps shrink by
$64\epsilon(1+|y_i|+|y_j|)$ and their sum by
$128\epsilon(1+J)$. Likelihood, tangent and tail comparisons carry the
`4096*epsilon` scale-dependent allowances in `joint_bounds.hpp`; tail
thresholds and remainders additionally expand by $10^{-10}$.
Unrepresentable quantities leave the affected bound unavailable.
Each doubling or scalar bisection uses at most `min(iterlim,100000)`
iterations; the total is below the certificate-evaluation budget.

There is no nondegenerate lognormal corner in the three-parameter
beta-prime family: $\operatorname{Var}(\log X)=\psi_1(p)+\psi_1(q)$
tends to zero when both shapes diverge. Changing $b$ translates $\log X$
and cannot prevent this concentration. A variable power in GB2 can change
this conclusion, but beta-prime has no such parameter.
The signed bound from the conditional certificate also extends uniformly
in the other shape. Write its per-observation leading coefficient as
$A(p)=p\{p(V-1)-1\}/2$ and its second-order remainder as $C(p)$ above.
Around the gamma optimum choose $[u,v]$ on which
$A(p)\le-a_0<0$ and $C(p)\le C(v)$.
For $q>2C(v)/a_0$, the entire neighborhood lies strictly below its
own gamma profile $G(p)$ and therefore below the gamma face supremum.
Outside $[u,v]$, concave supporting tangents give an upper bound $G_o$
on the two remaining shape intervals. A gamma witness $G_l>G_o$ gives
a strict gap $\delta=G_l-G_o$; the unsigned bound excludes those intervals
below the face for $q>8nH^2(1+2V)/\delta$.
Taking the larger cutoff and $4HM$ proves a signed joint tail bound
throughout the retained harmonic region. Reciprocal transport proves the
inverse-gamma result. The diagnostics `gamma_signed_shape2` and
`invgamma_signed_shape1` record these thresholds. The sign test inflates
$V-1$ by $10^{-10}(1+V)$, the gap uses the tangent's practical arithmetic
allowances, and thresholds expand by $10^{-10}$.

### All-free compact cover

Write $F(p,q)=\sup_t\ell(p,q,e^t)/n$ in geometric-mean units. The inner
scale score is strictly decreasing. With
$u_i=(1+\exp(t-y_i))^{-1}$ and $Q=\overline{u_i(1-u_i)}$,
\[
 \ell_t/n=-p+(p+q)\bar u,\qquad
 \ell_{tt}/n=-(p+q)Q<0.
\]
The root decreases in $p$ and increases in $q$. Thus a shape cell
$C=[p_l,p_u]\times[q_l,q_u]$ encloses every conditional maximizing scale
between its roots at $(p_u,q_l)$ and $(p_l,q_u)$.

The unchanged one-held-shape certificate is evaluated at a proposed $q_0$.
Its global upper bound includes every conditional branch, including
nonunique maxima and the inverse-gamma face. On cells containing $q_0$,
the enclosing $q$-score gives an integrated upper bound anchored at that
conditional value. The full shape cover retains the possible conditional
maximizers as multiple $p$ intervals; it does not assume that $p(q)$ is
single-valued or that the outer profile is unimodal.

The shape scores are
\[
 F_p=\psi(p+q)-\psi(p)-\overline{\log(1+e^{t-y_i})},\qquad
 F_q=\psi(p+q)-\psi(q)-\overline{\log(1+e^{y_i-t})}.
\]
Digamma-increment monotonicity and the scale enclosure bound both scores
throughout $C$. A cell whose gradient enclosure excludes zero cannot contain
a finite global maximum: all competitive finite maxima are interior to
the certified exterior cutoffs.

At fixed $t$, $-\log B(p,q)$ is strictly concave. Its supporting plane at
the cell center $m$ gives the independent value bound
\[
 \sup_C F\le \max_{v\in\operatorname{vertices}(C)}
 \{F(v)+\log B(v)-\log B(m)-\nabla\log B(m)\mathbin{\cdot}(v-m)\}.
\]
Scale profiling at each vertex supplies its value enclosure. Cells below
an incumbent lower bound are discarded. In large-shape cells the uniform
gamma remainder gives a cheaper bound; a negative leading coefficient
throughout the cell and its uniform remainder can exclude that cell
analytically below the gamma supremum. Reciprocal moments supply the other
tail. A face is selected only after this interior cover closes and its
likelihood lower bound exceeds all competing upper bounds.

A sharper signed bound uses the gamma optimum $a_*$, enclosed by its
strictly decreasing score. On a retained interval $p\le h$, let
$u=\max(h,a_{*,u})$, $\kappa=1/(2u^2)$, and
$B=1/2+u(V-1)$, where $V$ is the normalized second moment.
The trigamma inequality $\psi_1(p)>1/p+1/(2p^2)$ gives
$G(p)\le G(a_*)-\kappa(p-a_*)^2/2$ for the gamma profile $G$.
The tail coefficient $A(p)=p\{p(V-1)-1\}/2$ obeys
$A(p)\le A_0+B|p-a_*|$, with $A_0$ bounded over the enclosed optimum.
Completing the square therefore gives the uniform bound
\[
 F(p,q)\le G(a_*)+A_0/q+\{C(h)+B^2u^2\}/q^2.
\]
If $A_0<0$, taking $q$ strictly above both $2hM$ and
$\{C(h)+B^2u^2\}/(-A_0)$ excludes this whole strip below the gamma
supremum. This works even when $A(p)$ changes sign inside the strip.
On narrower cells, concavity bounds $G$ and the convex quadratic in $1/q$
gives the alternative remainder $\max(0,A_u/q_l+C/q_l^2)$.
The reciprocal argument handles inverse gamma.

The profiled Hessian is the Schur complement
\[
 H_{pp}=\psi_1(p+q)-\psi_1(p)+\frac{q^2}{(p+q)^3Q},\quad
 H_{qq}=\psi_1(p+q)-\psi_1(q)+\frac{p^2}{(p+q)^3Q},\quad
 H_{pq}=\psi_1(p+q)-\frac{pq}{(p+q)^3Q}.
\]
Endpoint scale bounds enclose every $u_i$, hence $Q$; monotonicity encloses
the trigamma differences. These yield an interval Hessian on each cell.
An interval-Newton image can exclude the stationary set or contract its
cell. A fixed direction with a positive lower curvature bound also excludes
a local maximum.

Around a proposed maximum, use relative shape coordinates and a symmetric
negative-definite approximate inverse Hessian $C_0$. If
$\|I-C_0H(C)\|_\infty<1/2$, and the interval fixed-point image lies inside
the cell, contraction proves existence and uniqueness of its stationary
point. Negative definiteness follows because $C_0$ is negative definite
and every eigenvalue of $C_0H$ is positive. The same contraction gives
componentwise coefficient bounds: with $R=|I-C_0H(C)|$ and
$d=|C_0g(m)|$, an explicitly checked vector $e>d+Re$ encloses the root.
Monotone scale roots at the shape-box corners enclose its scale.
A finite success requires relative error at most
$\min(\texttt{tol},10^{-6})$ in $p,q,b$, including scale reconstruction.
The full cell can then be assigned to this unique maximum; neighboring
cells remain covered independently.

Every surviving maximum is retained. Separated likelihood intervals choose
the winner; unresolved overlaps stop as comparison ties with the compared
finite or face witnesses. An early unresolved face comparison has an infinite
global upper bound and does not assert that either face wins. Failure to
close an exterior, a root enclosure, or a floating-point comparison gives
a numerical stop. Exhaustion of the shared work meter gives a budget stop.
No failed certificate is rescued by a proposal's optimizer score.

The all-free certificate shares `128 * min(iterlim,100000)` evaluations
across proposal objective/gradient calls, conditional-certificate work,
exterior evaluations, new scale-profile points and cell/Hessian bounds.
Each inner solve and refinement uses at most `min(iterlim,100000)`
iterations. Cached points are reused. The default is 2000: a seeded gamma-face sample needs about 141000 evaluations, above the former 128000 allowance. The fixed proposal grid supplies
witnesses only; its old large-shape evaluation guard does not restrict the
certified search domain.

The compact cover uses compensated sums. Normalizers use Stirling differences
above shape 32, with an analytic truncation allowance $10^{-19}$ per datum.
Point likelihood margins are $256\epsilon$ times the displayed likelihood
terms, plus input-log error and enclosed scale-root error. Shape scores use
the held certificate's digamma-increment bounds. Hessian margins are
$256\epsilon\{1+\psi_1(p_l)+\psi_1(q_l)+1/((p_l+q_l)Q_l)\}$.
Interval additions and multiplications expand by $16\epsilon$ times their
operand/product magnitudes plus one; root-image comparisons expand by
$128\epsilon$. Scale and reciprocal input transformations carry their
separate log-data/Jacobian allowances. These are practical floating-point
margins under the package's computed-comparison contract. Unresolved
inclusions stop rather than becoming accuracy claims.

With scale held, the likelihood is evaluated in beta mean/precision coordinates and transported by the log Jacobian, rather than reconstructed from separate log-gamma terms. For concentrated raw data, differences in the beta coordinate are formed directly as $s(x_i-x_0)/[(s+x_i)(s+x_0)]$. Profiling the centered mean displacement retains dispersion below one ulp of the absolute transformed mean. The scalar signs inherit beta strict concavity; they are computed comparisons, with numerical series evaluation rather than outward-rounded bounds. At concentration $t$, a mean perturbation $Delta\mu$ has leading likelihood cost $n t(\Delta\mu)^2/[2\mu(1-\mu)]$. Thus independently rounding the two shapes can matter even when each coefficient is accurate. Joint ratio assembly is checked against the centered optimum in log-X coordinates; a separated deficit in that unit-invariant likelihood gives an arithmetic failure. The [raw beta-prime checks](../../../../../dev/research/evaluations/2026-11-09-audit-beta-data/reference.json) include relative dispersions of order $10^{-12}$.

## R notes

Supply `scale` to hold it at a positive finite value. Either shape may also
be held with `shape1` or `shape2`. A certified conditional face stops with
its fitted limit coefficients and likelihood. Conditional numerical and budget
stops retain diagnostic witnesses; they do not select a limit model.
The all-free path returns finite estimates only after global cover and coefficient inclusion; its face, tie, budget and numerical stops retain structured witnesses. Constant positive data stop with unbounded-likelihood diagnostics.

## Decision history

univariateML 1.x estimated only the shapes, with scale fixed at 1.
On 2026-10-30 (TASK-125), identical observations with exactly one held shape
were assigned directly to the appropriate gamma or inverse-gamma limit using
the digamma inequality above. This avoids cancellation in the finite grid comparison.

The non-unit one-held-shape fit profiles over the free shape, using the unique
inner scale root at each shape. This gives explicit shape-tail bounds and
retains separated stationary intervals; the two-peak example rules out a
unimodality shortcut. The linked counterexample data, reproducers and
benchmark driver retain the numerical evidence.
