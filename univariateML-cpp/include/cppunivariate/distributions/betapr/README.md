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
    gradtol: Tolerance on the total gradient norm in log coordinates after dividing data by their geometric mean; with fixed scale, relative Newton step tolerance on log(shape2). Non-unit held-shape certificates use the fixed accuracy bounds described below.
    reltol: Relative objective tolerance.
    abstol: Step tolerance in log coordinates after dividing data by their geometric mean.
    shape10: Optional positive starting value for shape1 (zero selects automatic starts).
    shape20: Optional positive starting value for shape2 (zero selects automatic starts).
    scale0: Optional positive starting value for scale (zero selects automatic starts).
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations; per certificate: 128 * min(iterlim, 100000) certificate evaluations."
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
    detect: A numerical comparison selects the gamma limit within $10^{-8}n$ log-likelihood units. This comparison is not a global certificate.
    status: GammaBoundary
    numerical: true
  - model: invgamma
    firmness: proved
    when: The shape $p$ tends to infinity while $q$ tends to a positive finite shape and $bp$ tends to a positive inverse-gamma scale.
    detect: A numerical comparison selects the inverse-gamma limit within $10^{-8}n$ log-likelihood units. This comparison is not a global certificate.
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
  method: multistart
  certified: partial
  note: 'betapr/README.md, Details; betapr/mle.hpp (public fitter and held overloads); betapr/test.cpp: Held shape1=1 reduces exactly to certified Lomax and held shape2=1 to inverse Pareto, including all supported further holds and transformed stop bounds. Fixed-scale beta fitting is concave. Non-unit one-held-shape finite fits use a global profile certificate with explicit exterior bounds; constant samples select their gamma face analytically. Nonconstant faces use a signed second-order tail bound; unresolved comparisons stop. The joint three-parameter fit remains numerical.'
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
    finite fits. A complete joint sample criterion and joint uniqueness are
    not established. With scale held, a unique finite MLE exists exactly when
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
Each $q_i$ decreases on a shape interval, so its two endpoint values enclose
$q_i(1-q_i)$, with maximum $1/4$ if they straddle $1/2$.
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

The three-free-parameter fit retains its multistart search and
numerical comparisons with gamma and inverse-gamma limits. Its simultaneous
escape paths and global optimality remain uncertified.

## R notes

Supply `scale` to hold it at a positive finite value. Either shape may also
be held with `shape1` or `shape2`. A certified conditional face stops with
its fitted limit coefficients and likelihood. Conditional numerical and budget
stops retain diagnostic witnesses; they do not select a limit model.
The all-free path retains its $10^{-8}n$ numerical face preference.

## Decision history

univariateML 1.x estimated only the shapes, with scale fixed at 1.
On 2026-10-30 (TASK-125), identical observations with exactly one held shape
were assigned directly to the appropriate gamma or inverse-gamma limit using
the digamma inequality above. This avoids cancellation in the finite grid comparison.

The all-free face comparisons retain the family tolerance but stop with the existing numerical-failure status when `core/face_compare.hpp` cannot resolve the decision after its floating-point rounding allowance. This allowance concerns computed likelihood comparisons, not optimizer convergence or global optimality; see the [comparison analysis](../../../../../dev/research/evaluations/2026-11-02-face-compare-report.md).

The non-unit one-held-shape fit profiles over the free shape, using the unique
inner scale root at each shape. This gives explicit shape-tail bounds and
retains separated stationary intervals; the two-peak example rules out a
unimodality shortcut. The linked counterexample data, reproducers and
benchmark driver retain the numerical evidence.
