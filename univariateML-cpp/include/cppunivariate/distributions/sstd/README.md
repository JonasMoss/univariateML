---
id: sstd
inference_numeric:
  - regime: "symmetric member with skew held"
    available: true
    condition: "finite mean, sd > 0 and nu > 2; xi = 1 held; any other subset free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
  - regime: "unreviewed or unavailable"
    available: false
    condition: "finite mean, sd > 0 and nu > 2, xi > 0; xi free or xi != 1"
    reason: "DQM, finite full-rank information and a valid a.e. Hessian are established; numerical derivative and covariance validation for the expanded subsets is not yet implemented"
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
    gradtol: "Tolerance on the Euclidean norm of the total negative log-likelihood gradient in transformed coordinates after centering and scaling the sample to unit standard deviation; fixed step and objective-decrease criteria can also stop the fit."
    iterlim: "Maximum number of optimisation iterations."
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
  - {firmness: numerical, name: scale-collapse equality, fixed: [mean, nu, xi], when: "The standard deviation $\\sigma$ tends to zero at the held mean.", detect: "The tie multiplicity satisfies $k=\\nu(n-k)$; the scale-collapse boundary is uncertified.", status: ScaleBoundary, message: "Tie equality scale-collapse boundary"}
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
  certified: 'no'
  note: 'sstd/README.md, Details; sstd/mle.hpp (public fitter and held overloads); sstd/test.cpp: Default and nontrivial
    conditional fits use multistart, finite grids or local profile refinement without a global certificate; all-held
    density evaluation is not counted as certification.'
areas:
- finance
- measurement and errors
existence:
  uniqueness: unknown
  statement: >-
    With all coefficients free, scale collapse or a skew-normal, half-t, half-normal or two-piece $t_2$ limit can prevent a finite maximum. Global uniqueness is unknown, and the listed faces have not been proved to exhaust joint escapes. Holding coefficients restricts the reachable faces by their moment constraints. Holding $\nu$ removes shape escapes, holding $\xi$ removes skew escapes, and holding $\sigma$ excludes a finite-scale $t_2$ face but does not exclude collapse when $\nu$ is free. With $\mu$ and both shapes held, a positive scale maximum exists when $\nu(n-k)>k$, where $k$ counts observations at that location; the reverse strict inequality gives unbounded likelihood, and equality remains uncertified. With $\sigma$ and both shapes held, a location maximum exists even with ties. Holding all coefficients leaves likelihood evaluation.
---

The skew Student-t distribution combines heavy tails with asymmetry using the Fernandez-Steel construction. Its location and scale parameters are the mean and standard deviation.

## Details

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
exactly. Conditional fits compare multistart interior searches with every
reachable half-family and shape boundary, retaining the held mean and standard deviation
constraints on those faces. The fixed-shape, fixed-skew location-scale fit
profiles scale and scans location, including observation kinks. Scalar
searches scan before refining competing brackets. Numerical multistart and
finite scans provide evidence rather than a global optimality certificate.
A search reaching its numerical guard stops instead of returning a fit.

For scale estimation with fixed mean and shapes, let k observations equal
the held mean. The likelihood is unbounded at scale zero when
$k>\nu(n-k)$. Equality is reported as an uncertified scale-collapse
boundary; with strict inequality reversed the likelihood vanishes at both
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
