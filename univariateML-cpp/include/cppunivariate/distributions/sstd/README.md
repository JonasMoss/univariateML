---
id: sstd
inference_numeric:
  - regime: "regular finite interior"
    available: true
    condition: "finite mean, sd > 0, finite nu > 2 and finite xi > 0; any free subset, including xi = 1 with xi free; analytic piecewise natural-coordinate derivatives; successful interior fit and nonsingular information required"
    classification: "dev/research/evaluations/2026-11-04-dqm-b-report.md#two-piece-models-snorm-sstd-and-sged"
name: Skew Student-t
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
- name: mean
  symbol: '\mu'
  role: location
  domain: (-inf, inf)
  simulate: 0.0
  kind: regular
- name: sd
  symbol: '\sigma'
  role: scale
  domain: (0, inf)
  simulate: 1.0
  kind: regular
- name: nu
  symbol: '\nu'
  role: df
  domain: (2, inf)
  simulate: 5
  kind: regular
- name: xi
  symbol: '\xi'
  role: skew
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dsstd
  sampler: rsstd
  control:
    mean: "Optional held mean; NULL estimates it where the hold subset is supported."
    sd: "Optional held sd; NULL estimates it where the hold subset is supported."
    nu: "Optional held nu; NULL estimates it where the hold subset is supported."
    xi: "Optional held xi; NULL estimates it where the hold subset is supported."
    gradtol: "Requested relative coefficient accuracy for all-free fits (default 1e-10); checked by the final observed-information Newton correction."
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations."
log_density: "log(sqrt((1 - (gamma((nu - 1)/2)/gamma(nu/2)*sqrt((nu - 2)/pi))**2)*(xi**2 + xi**(-2)) + 2*(gamma((nu - 1)/2)/gamma(nu/2)*sqrt((nu - 2)/pi))**2 - 1)) - log(sd) + log(2/(xi + 1/xi)) + loggamma((nu + 1)/2) - loggamma(nu/2) - log((nu - 2)*pi)/2 - (nu + 1)*log(1 + (Piecewise(((sqrt((1 - (gamma((nu - 1)/2)/gamma(nu/2)*sqrt((nu - 2)/pi))**2)*(xi**2 + xi**(-2)) + 2*(gamma((nu - 1)/2)/gamma(nu/2)*sqrt((nu - 2)/pi))**2 - 1)*(x - mean)/sd + (gamma((nu - 1)/2)/gamma(nu/2)*sqrt((nu - 2)/pi))*(xi - 1/xi))/xi, (sqrt((1 - (gamma((nu - 1)/2)/gamma(nu/2)*sqrt((nu - 2)/pi))**2)*(xi**2 + xi**(-2)) + 2*(gamma((nu - 1)/2)/gamma(nu/2)*sqrt((nu - 2)/pi))**2 - 1)*(x - mean)/sd + (gamma((nu - 1)/2)/gamma(nu/2)*sqrt((nu - 2)/pi))*(xi - 1/xi)) > 0), ((sqrt((1 - (gamma((nu - 1)/2)/gamma(nu/2)*sqrt((nu - 2)/pi))**2)*(xi**2 + xi**(-2)) + 2*(gamma((nu - 1)/2)/gamma(nu/2)*sqrt((nu - 2)/pi))**2 - 1)*(x - mean)/sd + (gamma((nu - 1)/2)/gamma(nu/2)*sqrt((nu - 2)/pi))*(xi - 1/xi))*xi, True)))**2/(nu - 2))/2"
limits:
  - {firmness: proved, model: hnorm, name: right translated half-normal, fixed: [mean], when: "The shape $\\nu$ tends to infinity and $\\xi$ tends to infinity, retaining any held mean and standard deviation.", detect: "The numerically compared right half-normal corner wins among retained candidates.", status: RightHalfNormalBoundary, numerical: true}
  - {firmness: proved, model: hnorm, name: left translated half-normal, fixed: [mean], when: "The shape $\\nu$ tends to infinity and $\\xi$ tends to zero, retaining any held mean and standard deviation.", detect: "The numerically compared left half-normal corner wins among retained candidates.", status: LeftHalfNormalBoundary, numerical: true}
  - {firmness: proved, model: hnorm, name: right translated half-normal, fixed: [sd], when: "The shape $\\nu$ tends to infinity and $\\xi$ tends to infinity, retaining any held mean and standard deviation.", detect: "The numerically compared right half-normal corner wins among retained candidates.", status: RightHalfNormalBoundary, numerical: true}
  - {firmness: proved, model: hnorm, name: left translated half-normal, fixed: [sd], when: "The shape $\\nu$ tends to infinity and $\\xi$ tends to zero, retaining any held mean and standard deviation.", detect: "The numerically compared left half-normal corner wins among retained candidates.", status: LeftHalfNormalBoundary, numerical: true}
  - {firmness: proved, model: hnorm, name: right translated half-normal, fixed: [mean, sd], when: "The shape $\\nu$ tends to infinity and $\\xi$ tends to infinity, retaining any held mean and standard deviation.", detect: "The numerically compared right half-normal corner wins among retained candidates.", status: RightHalfNormalBoundary, numerical: true}
  - {firmness: proved, model: hnorm, name: left translated half-normal, fixed: [mean, sd], when: "The shape $\\nu$ tends to infinity and $\\xi$ tends to zero, retaining any held mean and standard deviation.", detect: "The numerically compared left half-normal corner wins among retained candidates.", status: LeftHalfNormalBoundary, numerical: true}
  - {firmness: proved, name: tie collapse, fixed: [mean], when: "The effective ordinary t scale tends to zero at an attainable concentration point.", detect: "A reachable concentration point has multiplicity $k>\\nu(n-k)$ for held shape; for free shape the diagnosed threshold is $k>2(n-k)$.", status: Unbounded}
  - {firmness: numerical, name: uncertified tie equality, fixed: [mean], when: "The effective ordinary t scale tends to zero at an attainable concentration point.", detect: "A reachable concentration point has multiplicity $k$, with $k=\\nu(n-k)$ for held shape; use the limiting value two for free shape. Equality is unresolved.", status: ScaleBoundary}
  - {firmness: proved, model: snorm, fixed: [mean], when: "The shape $\\nu$ tends to infinity while any held mean and standard deviation are retained.", detect: "The numerically compared limiting face wins among retained candidates while satisfying held constraints.", status: SnormBoundary, numerical: true}
  - {firmness: proved, name: two-piece t with 2 degrees of freedom, fixed: [mean], when: "The shape $\\nu$ decreases to two with finite positive ordinary t scale and any held mean retained; sd diverges.", detect: "The numerically compared two-piece t with two degrees of freedom wins among retained candidates.", status: DfTwoBoundary, numerical: true}
  - {firmness: proved, model: ht, name: right translated half-family, fixed: [mean], when: "The skew $\\xi$ tends to infinity, with endpoint and one-sided scale chosen to retain any held mean and standard deviation.", detect: "The numerically compared half-family face wins among retained candidates while satisfying held constraints.", status: RightHalfTBoundary, numerical: true}
  - {firmness: proved, model: ht, name: left translated half-family, fixed: [mean], when: "The skew $\\xi$ tends to zero, with endpoint and one-sided scale chosen to retain any held mean and standard deviation.", detect: "The numerically compared half-family face wins among retained candidates while satisfying held constraints.", status: LeftHalfTBoundary, numerical: true}
  - {firmness: proved, name: tie collapse, fixed: [sd], when: "The effective ordinary t scale tends to zero at an attainable concentration point.", detect: "A reachable concentration point has multiplicity $k>\\nu(n-k)$ for held shape; for free shape the diagnosed threshold is $k>2(n-k)$.", status: Unbounded}
  - {firmness: numerical, name: uncertified tie equality, fixed: [sd], when: "The effective ordinary t scale tends to zero at an attainable concentration point.", detect: "A reachable concentration point has multiplicity $k$, with $k=\\nu(n-k)$ for held shape; use the limiting value two for free shape. Equality is unresolved.", status: ScaleBoundary}
  - {firmness: proved, model: snorm, fixed: [sd], when: "The shape $\\nu$ tends to infinity while any held mean and standard deviation are retained.", detect: "The numerically compared limiting face wins among retained candidates while satisfying held constraints.", status: SnormBoundary, numerical: true}
  - {firmness: proved, model: ht, name: right translated half-family, fixed: [sd], when: "The skew $\\xi$ tends to infinity, with endpoint and one-sided scale chosen to retain any held mean and standard deviation.", detect: "The numerically compared half-family face wins among retained candidates while satisfying held constraints.", status: RightHalfTBoundary, numerical: true}
  - {firmness: proved, model: ht, name: left translated half-family, fixed: [sd], when: "The skew $\\xi$ tends to zero, with endpoint and one-sided scale chosen to retain any held mean and standard deviation.", detect: "The numerically compared half-family face wins among retained candidates while satisfying held constraints.", status: LeftHalfTBoundary, numerical: true}
  - {firmness: proved, name: tie collapse, fixed: [nu], when: "The effective ordinary t scale tends to zero at an attainable concentration point.", detect: "A reachable concentration point has multiplicity $k>\\nu(n-k)$ for held shape; for free shape the diagnosed threshold is $k>2(n-k)$.", status: Unbounded}
  - {firmness: numerical, name: uncertified tie equality, fixed: [nu], when: "The effective ordinary t scale tends to zero at an attainable concentration point.", detect: "A reachable concentration point has multiplicity $k$, with $k=\\nu(n-k)$ for held shape; use the limiting value two for free shape. Equality is unresolved.", status: ScaleBoundary}
  - {firmness: proved, model: ht, name: right translated half-family, fixed: [nu], when: "The skew $\\xi$ tends to infinity, with endpoint and one-sided scale chosen to retain any held mean and standard deviation.", detect: "The numerically compared half-family face wins among retained candidates while satisfying held constraints.", status: RightHalfTBoundary, numerical: true}
  - {firmness: proved, model: ht, name: left translated half-family, fixed: [nu], when: "The skew $\\xi$ tends to zero, with endpoint and one-sided scale chosen to retain any held mean and standard deviation.", detect: "The numerically compared half-family face wins among retained candidates while satisfying held constraints.", status: LeftHalfTBoundary, numerical: true}
  - {firmness: proved, name: tie collapse, fixed: [xi], when: "The effective ordinary t scale tends to zero at an attainable concentration point.", detect: "A reachable concentration point has multiplicity $k>\\nu(n-k)$ for held shape; for free shape the diagnosed threshold is $k>2(n-k)$.", status: Unbounded}
  - {firmness: numerical, name: uncertified tie equality, fixed: [xi], when: "The effective ordinary t scale tends to zero at an attainable concentration point.", detect: "A reachable concentration point has multiplicity $k$, with $k=\\nu(n-k)$ for held shape; use the limiting value two for free shape. Equality is unresolved.", status: ScaleBoundary}
  - {firmness: proved, model: snorm, fixed: [xi], when: "The shape $\\nu$ tends to infinity while any held mean and standard deviation are retained.", detect: "The numerically compared limiting face wins among retained candidates while satisfying held constraints.", status: SnormBoundary, numerical: true}
  - {firmness: proved, name: two-piece t with 2 degrees of freedom, fixed: [xi], when: "The shape $\\nu$ decreases to two with finite positive ordinary t scale and any held mean retained; sd diverges.", detect: "The numerically compared two-piece t with two degrees of freedom wins among retained candidates.", status: DfTwoBoundary, numerical: true}
  - {firmness: proved, name: tie collapse, fixed: [mean, sd], when: "The effective ordinary t scale tends to zero at an attainable concentration point.", detect: "A reachable concentration point has multiplicity $k>\\nu(n-k)$ for held shape; for free shape the diagnosed threshold is $k>2(n-k)$.", status: Unbounded}
  - {firmness: numerical, name: uncertified tie equality, fixed: [mean, sd], when: "The effective ordinary t scale tends to zero at an attainable concentration point.", detect: "A reachable concentration point has multiplicity $k$, with $k=\\nu(n-k)$ for held shape; use the limiting value two for free shape. Equality is unresolved.", status: ScaleBoundary}
  - {firmness: proved, model: snorm, fixed: [mean, sd], when: "The shape $\\nu$ tends to infinity while any held mean and standard deviation are retained.", detect: "The numerically compared limiting face wins among retained candidates while satisfying held constraints.", status: SnormBoundary, numerical: true}
  - {firmness: proved, model: ht, name: right translated half-family, fixed: [mean, sd], when: "The skew $\\xi$ tends to infinity, with endpoint and one-sided scale chosen to retain any held mean and standard deviation.", detect: "The numerically compared half-family face wins among retained candidates while satisfying held constraints.", status: RightHalfTBoundary, numerical: true}
  - {firmness: proved, model: ht, name: left translated half-family, fixed: [mean, sd], when: "The skew $\\xi$ tends to zero, with endpoint and one-sided scale chosen to retain any held mean and standard deviation.", detect: "The numerically compared half-family face wins among retained candidates while satisfying held constraints.", status: LeftHalfTBoundary, numerical: true}
  - {firmness: proved, name: tie collapse, fixed: [mean, nu], when: "The effective ordinary t scale tends to zero at an attainable concentration point.", detect: "A reachable concentration point has multiplicity $k>\\nu(n-k)$ for held shape; for free shape the diagnosed threshold is $k>2(n-k)$.", status: Unbounded}
  - {firmness: numerical, name: uncertified tie equality, fixed: [mean, nu], when: "The effective ordinary t scale tends to zero at an attainable concentration point.", detect: "A reachable concentration point has multiplicity $k$, with $k=\\nu(n-k)$ for held shape; use the limiting value two for free shape. Equality is unresolved.", status: ScaleBoundary}
  - {firmness: proved, model: ht, name: right translated half-family, fixed: [mean, nu], when: "The skew $\\xi$ tends to infinity, with endpoint and one-sided scale chosen to retain any held mean and standard deviation.", detect: "The numerically compared half-family face wins among retained candidates while satisfying held constraints.", status: RightHalfTBoundary, numerical: true}
  - {firmness: proved, model: ht, name: left translated half-family, fixed: [mean, nu], when: "The skew $\\xi$ tends to zero, with endpoint and one-sided scale chosen to retain any held mean and standard deviation.", detect: "The numerically compared half-family face wins among retained candidates while satisfying held constraints.", status: LeftHalfTBoundary, numerical: true}
  - {firmness: proved, name: tie collapse, fixed: [mean, xi], when: "The effective ordinary t scale tends to zero at an attainable concentration point.", detect: "A reachable concentration point has multiplicity $k>\\nu(n-k)$ for held shape; for free shape the diagnosed threshold is $k>2(n-k)$.", status: Unbounded}
  - {firmness: numerical, name: uncertified tie equality, fixed: [mean, xi], when: "The effective ordinary t scale tends to zero at an attainable concentration point.", detect: "A reachable concentration point has multiplicity $k$, with $k=\\nu(n-k)$ for held shape; use the limiting value two for free shape. Equality is unresolved.", status: ScaleBoundary}
  - {firmness: proved, model: snorm, fixed: [mean, xi], when: "The shape $\\nu$ tends to infinity while any held mean and standard deviation are retained.", detect: "The numerically compared limiting face wins among retained candidates while satisfying held constraints.", status: SnormBoundary, numerical: true}
  - {firmness: proved, name: two-piece t with 2 degrees of freedom, fixed: [mean, xi], when: "The shape $\\nu$ decreases to two with finite positive ordinary t scale and any held mean retained; sd diverges.", detect: "The numerically compared two-piece t with two degrees of freedom wins among retained candidates.", status: DfTwoBoundary, numerical: true}
  - {firmness: proved, model: ht, name: right translated half-family, fixed: [sd, nu], when: "The skew $\\xi$ tends to infinity, with endpoint and one-sided scale chosen to retain any held mean and standard deviation.", detect: "The numerically compared half-family face wins among retained candidates while satisfying held constraints.", status: RightHalfTBoundary, numerical: true}
  - {firmness: proved, model: ht, name: left translated half-family, fixed: [sd, nu], when: "The skew $\\xi$ tends to zero, with endpoint and one-sided scale chosen to retain any held mean and standard deviation.", detect: "The numerically compared half-family face wins among retained candidates while satisfying held constraints.", status: LeftHalfTBoundary, numerical: true}
  - {firmness: proved, name: tie collapse, fixed: [sd, xi], when: "The effective ordinary t scale tends to zero at an attainable concentration point.", detect: "A reachable concentration point has multiplicity $k>\\nu(n-k)$ for held shape; for free shape the diagnosed threshold is $k>2(n-k)$.", status: Unbounded}
  - {firmness: numerical, name: uncertified tie equality, fixed: [sd, xi], when: "The effective ordinary t scale tends to zero at an attainable concentration point.", detect: "A reachable concentration point has multiplicity $k$, with $k=\\nu(n-k)$ for held shape; use the limiting value two for free shape. Equality is unresolved.", status: ScaleBoundary}
  - {firmness: proved, model: snorm, fixed: [sd, xi], when: "The shape $\\nu$ tends to infinity while any held mean and standard deviation are retained.", detect: "The numerically compared limiting face wins among retained candidates while satisfying held constraints.", status: SnormBoundary, numerical: true}
  - {firmness: proved, name: tie collapse, fixed: [nu, xi], when: "The effective ordinary t scale tends to zero at an attainable concentration point.", detect: "A reachable concentration point has multiplicity $k>\\nu(n-k)$ for held shape; for free shape the diagnosed threshold is $k>2(n-k)$.", status: Unbounded}
  - {firmness: numerical, name: uncertified tie equality, fixed: [nu, xi], when: "The effective ordinary t scale tends to zero at an attainable concentration point.", detect: "A reachable concentration point has multiplicity $k$, with $k=\\nu(n-k)$ for held shape; use the limiting value two for free shape. Equality is unresolved.", status: ScaleBoundary}
  - {firmness: proved, model: ht, name: right translated half-family, fixed: [mean, sd, nu], when: "The skew $\\xi$ tends to infinity, with endpoint and one-sided scale chosen to retain any held mean and standard deviation.", detect: "The numerically compared half-family face wins among retained candidates while satisfying held constraints.", status: RightHalfTBoundary, numerical: true}
  - {firmness: proved, model: ht, name: left translated half-family, fixed: [mean, sd, nu], when: "The skew $\\xi$ tends to zero, with endpoint and one-sided scale chosen to retain any held mean and standard deviation.", detect: "The numerically compared half-family face wins among retained candidates while satisfying held constraints.", status: LeftHalfTBoundary, numerical: true}
  - {firmness: proved, name: tie collapse, fixed: [mean, sd, xi], when: "The effective ordinary t scale tends to zero at an attainable concentration point.", detect: "A reachable concentration point has multiplicity $k>\\nu(n-k)$ for held shape; for free shape the diagnosed threshold is $k>2(n-k)$.", status: Unbounded}
  - {firmness: numerical, name: uncertified tie equality, fixed: [mean, sd, xi], when: "The effective ordinary t scale tends to zero at an attainable concentration point.", detect: "A reachable concentration point has multiplicity $k$, with $k=\\nu(n-k)$ for held shape; use the limiting value two for free shape. Equality is unresolved.", status: ScaleBoundary}
  - {firmness: proved, model: snorm, fixed: [mean, sd, xi], when: "The shape $\\nu$ tends to infinity while any held mean and standard deviation are retained.", detect: "The numerically compared limiting face wins among retained candidates while satisfying held constraints.", status: SnormBoundary, numerical: true}
  - {firmness: proved, name: scale collapse, fixed: [mean, nu, xi], when: "The standard deviation $\\sigma$ tends to zero at the held mean.", detect: "The concentration point has multiplicity $k>\\nu(n-k)$ with scale free.", status: Unbounded, message: "Likelihood is unbounded at a tie"}
  - {firmness: proved, name: symmetric scale-collapse equality, fixed: [mean, nu, xi], when: "Held xi is one and the standard deviation tends to zero at the held mean.", detect: "The tie multiplicity satisfies $k=\\nu(n-k)$; the inverse-scale likelihood is strictly increasing to a finite unattained supremum.", status: ScaleBoundary, message: "finite and unattained"}
  - {firmness: numerical, name: nonunit-skew scale-collapse equality, fixed: [mean, nu, xi], when: "Held xi differs from one and the standard deviation tends to zero at the held mean.", detect: "The tie multiplicity satisfies $k=\\nu(n-k)$; the finite/face likelihood comparison is unresolved.", status: CertificateFailure, message: "Nonunit-skew scale-collapse equality"}
  - model: snorm
    firmness: proved
    when: 'The shape $\nu$ tends to infinity, retaining finite location and scale.'
    detect: The numerically compared limiting face is selected over the retained finite candidates.
    status: SnormBoundary
    numerical: true
  - model: hnorm
    firmness: proved
    when: 'The limiting path is $\nu \to \infty$ and $\xi \to \infty$.'
    detect: The numerically compared limiting face is selected over the retained finite candidates.
    status: RightHalfNormalBoundary
    numerical: true
  - name: reflected translated half-normal
    firmness: proved
    when: 'The limiting path is $\nu \to \infty$ and $\xi \to 0$.'
    detect: The numerically compared limiting face is selected over the retained finite candidates.
    status: LeftHalfNormalBoundary
    numerical: true
  - name: translated half-t
    firmness: proved
    when: 'The skew $\xi$ tends to infinity with finite one-sided scale and endpoint at the sample minimum.'
    detect: The numerically compared limiting face is selected over the retained finite candidates.
    status: RightHalfTBoundary
    numerical: true
  - name: reflected translated half-t
    firmness: proved
    when: 'The skew $\xi$ tends to zero with finite one-sided scale and endpoint at the sample maximum.'
    detect: The numerically compared limiting face is selected over the retained finite candidates.
    status: LeftHalfTBoundary
    numerical: true
  - name: two-piece t with 2 degrees of freedom
    firmness: proved
    when: 'The shape $\nu$ decreases to two with ordinary t scale and skew fixed; sd diverges.'
    detect: The numerically compared limiting face is selected over the retained finite candidates.
    status: DfTwoBoundary
    numerical: true
references:
  - "Fernandez, C. and Steel, M. F. J. (1998). On Bayesian modeling of fat tails and skewness. Journal of the American Statistical Association, 93(441), 359-371. doi:10.1080/01621459.1998.10474117."
examples: |
  x <- diff(log(datasets::EuStockMarkets[, "DAX"]))
  mlsstd(x)

summary: The moment-standardized Fernandez–Steel skew Student distribution allows asymmetry and heavy tails.
classification: specialist
classification_reason: Used to model financial returns requiring both asymmetry and polynomial tails.
aliases:
- "Fernández–Steel skew Student"
key_references:
- 1

group: skewed and robust location-scale
construction:
- skewing
- location-scale
relations: {}
fit:
  method: multistart
  certified: 'partial'
  note: 'certificate.hpp and README.md, Held-shape scalar certificates. Fixed nu and xi: mean-only and sd-only searches use exhaustive score enclosures with budget,
    tie and numerical stops. Symmetric scale equality has an exact unattained supremum. Held xi=1 with mean, sd and nu free delegates to the certified std parent. Other subsets,
    including free shape at asymmetric held skew, free skew with free moments, and all-free fits, remain uncertified. Fixed nu with both moments held uses skew_certificate.hpp to enclose the complete skew range and compare its constrained half-t faces.'
areas:
- finance
- measurement and errors
existence:
  uniqueness: unknown
  statement: >-
    With all coefficients free, scale collapse or a skew-normal, half-t, half-normal or two-piece $t_2$ limit can prevent a finite maximum. Global uniqueness is unknown, and the implementation does not certify the full search. The uniform closure theorem exhausts joint escapes in the strict tie regime $3k_{max}<2n$, but its quantitative face profilers are not implemented. Holding coefficients restricts the reachable faces by their moment constraints. Holding $\nu$ removes shape escapes, holding $\xi$ removes skew escapes, and holding $\sigma$ excludes a finite-scale $t_2$ face but does not exclude collapse when $\nu$ is free. With $\mu$ and both shapes held, a positive scale maximum exists when $\nu(n-k)>k$, where $k$ counts observations at that location; the reverse strict inequality gives unbounded likelihood, and equality is an unattained finite supremum at $\xi=1$ and remains numerically unresolved otherwise. With $\sigma$ and both shapes held, a location maximum exists even with ties. Holding all coefficients leaves likelihood evaluation.
---

The skew Student-t distribution combines heavy tails with asymmetry using the Fernandez-Steel construction. Its location and scale parameters are the mean and standard deviation.

## Details

The all-free accuracy check uses second-order chain derivatives of the
variance-standardized two-piece t density, including the shape-dependent moment
correction. Half-step gamma ratios and the normal-limit kernel are combined
before differentiation; compensated derivative sums preserve replicated-data
precision. The shape check applies to public nu, including its offset of two.
The location floor is the fitted standard deviation. See the [accuracy
validation](../../../../../dev/research/evaluations/2026-11-10-acc-allfree-report.md).

The coefficients $\mu$ and $\sigma$ are the mean and standard deviation, rather than the join point and kernel scale. The shape $\nu>2$ controls tail weight. The skew coefficient $\xi=1$ gives symmetry; values above one lengthen the right-hand side, and reciprocals reflect the standardized distribution. Holding moments therefore constrains the join point and side scales as shapes vary.


### Density limits

Write $g_\nu$ for the ordinary unit-scale Student density, $r=\xi$,
$c$ for the mode and $a>0$ for the ordinary t scale. The density is
$2g_\nu((x-c)/(ar))/(a(r+r^{-1}))$ for $x\ge c$, and uses
$(x-c)r/a$ on the other side. If $m_\nu=E|T_\nu|$, its mean is
$c+a m_\nu(r-r^{-1})$ and its variance is
$a^2\{\nu(r^2+r^{-2}-1)/(\nu-2)-m_\nu^2(r-r^{-1})^2\}$.
These identities follow by integrating the two half densities. Thus held
mean and/or sd are enforced by solving these two displayed equations along
the path, rather than holding the mode or ordinary scale fixed.

For fixed finite $r$, the gamma-ratio form of $g_\nu$ gives
$g_\nu\to\phi$ and $m_\nu\to\sqrt{2/\pi}$ as $\nu\to\infty$;
the variance formula converges too. This proves the moment-standardized
skew-normal faces, including the listed held subsets. For $r\to\infty$
with $ar\to b>0$ and $c\to c_0$, the negative-side probability is
$1/(1+r^2)$ and the positive-side density tends to
$2g_\nu((x-c_0)/b)/b$. The reflected statement follows by $r\mapsto
1/r$ and reflection. The limiting mean and standard deviation are $c_0\pm bm_\nu$
and $b\sqrt{\nu/(\nu-2)-m_\nu^2}$; they determine the endpoint and
scale when moments are held. If also $\nu\to\infty$, these become the
corresponding half-normal moments. These are density limits away from
the endpoint (and distributional limits); endpoint density values can
depend on the approach and matter in likelihood comparisons.

Finally, with finite $r$ and $a\to a_0>0$, $g_\nu\to g_2$ as
$\nu\downarrow2$ and $m_\nu\to\sqrt2$. Thus the limit is the stated
two-piece $t_2$, with mean $c_0+a_0\sqrt2(r-r^{-1})$ and infinite
variance. In particular this path requires the standard deviation to diverge and is not
reachable with standard deviation held. These proofs establish the declared faces, not
that a numerical fit or comparison is globally optimal.

The [conditional skew analysis](../../../../../dev/research/evaluations/2026-10-29-p2b-skew-report.md) derives the collapse exponents and face constraints; its equality cases and global comparisons remain uncertified.

A tie of multiplicity $k>2(n-k)$ makes the free-shape likelihood unbounded even at fixed skewness (the two side scales change the coefficient, not the collapse exponent); equality is reported as an uncertified scale-collapse boundary, and a half-t face has a positive scale root only when $\nu(n-k)>k$.

This is the fGarch parameterisation: $\mu$ and $\sigma$ are the mean and standard deviation, $\nu$ is greater than 2, and $\xi=1$ gives the Student-t distribution.

A finite MLE can fail to exist, especially in small samples, when a larger likelihood is approached only along a limiting parameter path. Fitting then stops with a message naming the numerically selected limit. The fitter compares the best of several interior fits with the skew-normal limit ($\nu \to \infty$, fitted by its own boundary-aware routine), the translated half-t limits ($\xi \to \infty$ or $0$, with the endpoint at the sample minimum or maximum and the degrees of freedom searched over $[2, \infty]$), and the two-piece t with 2 degrees of freedom ($\nu \to 2$), which has infinite variance and no finite $\sigma$. A limit is reported when it is at least as good as the best interior fit, up to $10^{-8}$ per observation.

This is a multistart comparison, not a certificate of the global maximum. The recorded simulation study selected limiting faces in about three quarters of size-20 samples from skew-t models with 5 or 10 degrees of freedom. This numerical evidence does not prove non-existence for those samples.


Every coefficient may be held, singly or jointly; held values are returned
exactly. Except for the held-shape and fixed-moment scalar certificates below, conditional fits compare multistart interior searches with every
reachable half-family and shape boundary, retaining the held mean and standard deviation
constraints on those faces. The fixed-shape, fixed-skew location-scale fit
profiles scale and scans location, including observation kinks. Remaining scalar
searches scan before refining competing brackets. Numerical multistart and
finite scans provide evidence rather than a global optimality certificate.
A search reaching its numerical guard stops instead of returning a fit.

For scale estimation with fixed mean and shapes, let k observations equal
the held mean. The likelihood is unbounded at scale zero when
$k>\nu(n-k)$. At $\xi=1$, equality is a finite unattained supremum; at other fixed skewness it stops numerically pending a finite/face comparison. With strict inequality reversed the likelihood vanishes at both
scale endpoints. With scale and both shapes held, location maxima exist even with ties.

Holding the standard deviation does not prevent collapse when shape is free: as shape tends to two,
the effective ordinary-t scale vanishes even at fixed standard deviation. The same tie
threshold applies, at the held mean when mean is held and at any sample
tie otherwise. Holding both standard deviation and shape prevents that collapse.

## Decision history

The 2026-10-31 fit-speed revision stops the fixed-shape scale bisection when its
midpoint rounds to an endpoint: the remaining iterations cannot change the
represented final midpoint or scale. The shared held-fit helper retains the same
grid and all refinements. In held location/scale fits with fixed shape and skew,
density normalizers are computed once while per-observation arithmetic grouping
is preserved. See the [paired evidence and rejected
trials](../../../../../dev/research/evaluations/2026-10-31-fit-speed-report.md).

2026-10-30 (adapter independence).

The unconditional BFGS failure rescue now lives in `sstd_mle`: accept only when the Euclidean norm of the finite-difference total score in (mu, log(sigma), log(nu-2), log(xi)) coordinates is below 1e-3. Conditional failures remain failures. This preserves the former adapter gate; it is a numerical stationarity check, not a global maximum proof.

## Held-shape scalar certificates

With $\nu$ and $\xi$ held, the mean-only and sd-only routes use
`certificate.hpp`, without a finite scan window or a unimodality assumption.
Write $s_u, m_u$ for the standardized core sd and mean, and
$z=(x-M)s_u/S+m_u$. On either side let $c=\sqrt{\nu-2}\xi^{\operatorname{sign}(z)}$.
The mean score sums $(\nu+1)s_u z/[S(c^2+z^2)]$.
Every stationary mean lies between the extreme observations translated by
$Sm_u/s_u$. The search encloses each rational term using its endpoints and
its possible extrema $z=\pm c$, splitting terms at $z=0$.

For scale, use log inverse sd $t$ in sample units. Its score is
$n-(\nu+1)\sum z(z-m_u)/(c^2+z^2)$.
The rational term's only interior extrema solve
$m_u z^2+2c^2z-m_uc^2=0$; both roots are included. Their coordinates use ratios scaled by
$\operatorname{hypot}(c,m_u)$: unscaled products can overflow while the
extrema themselves remain finite. Nonfinite radii or stationary coordinates,
and subnormal denominator widths, stop numerically.
The initial log-scale bracket expands by one until score bounds on the
**entire** two exterior half-lines have inward signs. Thus no finite cap
is mistaken for a tail certificate. A failed bracket stops numerically.
The nonunit-skew six-point example has two distinct local maxima, so a
single scale root is insufficient.

Subdivision excludes only intervals whose padded score range omits zero.
Every retained stationary interval has width at most
$2\times10^{-11}(1+|t|)$ in its search coordinate; adjacent intervals are
grouped only within $10^{-9}(1+|t|)$. The likelihood upper bound adds the
score bound times the half-width to its midpoint value. Subdivision continues
until this integrated score remainder is no larger than the point-likelihood
rounding allowance. Near $\nu=2$ a narrow coefficient interval can still
span a sharply varying likelihood, so coefficient width alone is insufficient. All surviving
components are compared, including minima; competing maxima within their
computed bounds produce a tie stop with witnesses. For example, observations
$-2,2$ with $S=1,\nu=3,\xi=1$ have two global mean maxima
$\pm\sqrt3$, disproving general conditional uniqueness.

These certificates are exact-arithmetic arguments conditional on the
computed enclosures and comparisons. The practical floating-point allowance
is 128 binary64 epsilons times one plus the absolute term sum, applied to
scores and likelihoods; it is not outward-rounded interval arithmetic.
Nonfinite bounds or lossy public-coordinate transport stop numerically.
Certificate work is $128\min(\mathrm{iterlim},100000)$ cells; the separate
bracket-expansion cap is $\min(\mathrm{iterlim},100000)$ solver steps.
Budget stops retain a finite incumbent and an infinite global upper bound.

For held mean, the scale-collapse exponent is $\nu(n-k)-k$, where $k$
counts observations at that mean. A negative exponent is unbounded. At
$\xi=1$ and equality, the inverse-scale score is strictly positive, so the
explicit finite collapse supremum is unattained. At nonunit skew, equality
stops numerically: the collapse value still needs comparison with finite
branches. With both shapes held, all-held evaluation is exact up to density
arithmetic. Free-shape searches and larger free subsets
retain the previous numerical methods and have no new certificate, except
that exactly held $\xi=1$ with mean, sd and nu free delegates to
`student_t_mle_certified`. This is the identical std likelihood and parameter
space. Its normal, two-df and collapse faces, budget and tie witnesses are
transported without local fallback; finite candidates retain their declared
parent model coordinates. Merely estimating xi close to one does not invoke
the reduction. Other held std subsets are not claimed by this delegation.

The [implementation record](../../../../../dev/research/evaluations/2026-11-07-skew-impl-report.md)
retains the scan replacement rationale, checks and remaining obligations;
the [closure and endpoint analysis](../../../../../dev/research/evaluations/2026-11-07-skew-theory-data/proofs.md)
supplies the held-moment half-face factors needed by future profilers.


## Fixed-moment skew certificate

With mean, sd and nu held, `skew_certificate.hpp` uses
$q=\xi^2/(1+\xi^2)\in(0,1)$. Let $m=E|Z_\nu|$ for the
unit-variance t, $d=(x-\mu)/\sigma$,
$H=1-m^2+(4m^2-3)q(1-q)$ and $N=d\sqrt H+m(2q-1)$.
The density is $2\sqrt H/\sigma$ times the unit-variance t density at
$N/q$ when $N>0$, and at $N/(1-q)$ otherwise. This identity preserves
both held moments throughout the search. Rational and square-root interval
bounds enclose its likelihood and score, including seam crossings.
Subdivision excludes a cell only by a likelihood upper bound or a definite
score sign; both skew endpoints are included as constrained half-t faces.

Finite root components are resolved in q and merged only when their full
span is at most $10^{-10}\min(q,1-q)$, implying a log-skew span of at
most approximately $10^{-10}$. Distinct unresolved winners return a tie.
For data $(-1,1)$ with mean zero, sd one and nu five, the two reflected
half-t faces tie; reflection proves their exact equality, while the computed
bounds certify their dominance over the finite stationary regions.

The endpoint with zero shrinking-side residual requires a removable
$0/0$ calculation: this route stops numerically on an endpoint contact
within its floating-point margin. The skew moment and t normalizer share stable gamma-ratio
evaluation, including the normal limit; see the
[accuracy checks](../../../../../dev/research/evaluations/2026-11-08-sstd-moment-data/check.py). It never substitutes the ordinary half-t
endpoint height. Numerical and budget stops retain a finite witness and an
infinite global upper bound. Work is bounded by
$128\min(\mathrm{iterlim},100000)$ cell evaluations. Practical margins
are 128 binary64 epsilons times one plus the sum of absolute interval-term
bounds. As with the held-shape scalar certificates, the proof is conditional
on computed comparisons; arithmetic is not outward rounded.

Free shape at asymmetric held skew still needs uniform shape-tail bounds
and constrained-face profile comparisons. Free skew with either moment free
needs exhaustive joint stationary-region enclosures. The all-free fit
additionally needs uniform bounds at joint shape/skew corners. Those routes
retain their numerical methods and are not certified. The
[derivation and remaining obligations](../../../../../dev/research/evaluations/2026-11-08-sstd-free-data/derivation.md)
records the precise remaining obligations and independent checks.

### Extreme d/p/q arithmetic

A finite centered residual can overflow when divided by the thin-side skew scale. The density then retains its logarithmic radius, and the probability uses the Student tail kernel in log-radius coordinates. This avoids squaring or materializing the unrepresentable standardized residual; unresolved extreme-skew centering errors remain separately recorded in the truth exclusions.

### Exact base-moment submodel

At nu=5 the standardized first absolute moment is 4*sqrt(3)/(3*pi). A two-word moment and compensated skew displacement retain the thin-side coordinate. The broad-side central probability uses the incomplete beta at v^2/(3+v^2); its linear branch has relative remainder bounded by v^2. The upper-tail hazard is bounded by 5/v for v>=1 and by 16 below one. The d/p bridge returns NaN if its practical coordinate margin fails or its raw displacement or standardized radius cannot be represented; these are named precision regions. Sampling and the inverse transformation retain their original arithmetic.

For other degrees of freedom, the first absolute moment uses a two-word half-shift gamma ratio. Its alternating Laplace expansion has remainder bounded by the first omitted term; recurrence reaches an argument of at least 32. Gamma log-convexity also bounds the difference from the normal moment by 2/nu, so the limiting constant is used only when that bound is below the two-word rounding margin. The symmetric member delegates directly to std.
