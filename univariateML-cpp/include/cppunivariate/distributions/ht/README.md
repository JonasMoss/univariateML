---
id: ht
inference_numeric:
  - regime: "known threshold"
    available: true
    condition: "finite mu held, nu > 0 and sigma > 0; nu and/or sigma free"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#known-threshold-and-fixed-shape-submodels"
  - regime: unreviewed or unavailable
    available: false
    condition: "threshold free"
    reason: "a free threshold changes support with positive endpoint density and is non-DQM"
    classification: "dev/roadmap/2026-10-30-inference-and-dqm.md#moving-support"
name: Half-t
support: {type: real, lower: "mu", upper: inf, closed: [true, false], endpoint_log_density: {lower: "log(2) + lgamma((nu+1)/2) - lgamma(nu/2) - log(nu*pi)/2 - log(sigma)"}}
params:
- name: mu
  symbol: '\mu'
  role: location
  domain: (-inf, inf)
  simulate: 0
  kind: threshold
  advice: Holding mu = 0 gives the ordinary half-t. Supply mu = NULL to estimate the translated endpoint, with estimated nu restricted to nu >= 1.
- name: nu
  symbol: '\nu'
  role: shape
  domain: (0, inf)
  simulate: 5
  fit_domain:
    lower: 1.0
    restriction:
      mechanism: collapse
      exempt_when_held: [[mu]]
      reason: Estimating nu with a free endpoint is restricted to nu >= 1 to exclude
        small-shape collapse for samples whose minimum occurs less than half the time.
        Holding mu removes this fitting bound; endpoint ties still require collapse diagnostics.
  kind: regular
- name: sigma
  symbol: '\sigma'
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dht
  sampler: rht
  control:
    mu: Required endpoint; a value holds it and explicit `NULL` estimates the translation.
log_density: log(2) + loggamma((nu+1)/2) - loggamma(nu/2) - log(nu*pi)/2 - log(sigma) - (nu+1)*log(1+(x-mu)**2/(nu*sigma**2))/2
limits:
  - model: hnorm
    firmness: proved
    name: half-normal with the same endpoint; use mlhnorm(x, mu = 0) after a held
      translation, or mlhnorm(x, mu = NULL) with a free one
    when: 'The shape $\nu$ tends to infinity, retaining finite location and scale.'
    detect: The numerically compared limiting face is selected over the retained finite candidates.
    status: NormalLimit
  - name: unbounded scale collapse
    firmness: proved
    when: 'The free scale $\sigma$ tends to zero at the free or held endpoint.'
    detect: 'With held shape, $k>\nu(n-k)$ endpoint observations cause collapse. With free shape and a held endpoint, any endpoint tie suffices. With both endpoint and shape free, $2k>n$ suffices under the fitting restriction.'
    status: Unbounded
  - name: finite unattained scale-zero supremum
    firmness: proved
    fixed: [nu]
    when: 'The free scale $\sigma$ tends to zero at fixed positive shape.'
    detect: 'The endpoint multiplicity satisfies $k=\nu(n-k)$ in a sample of size $n$. The scale supremum is finite and unattained.'
    status: ScaleBoundary
references:
- extraDistr package, HalfT documentation (density and parameterization).
- 'Wolodzko, T. and Klinke, S. (2026). extraDistr: Additional Univariate and Multivariate Distributions. R package
  reference manual, version 1.10.0.5. https://cran.r-project.org/web/packages/extraDistr/extraDistr.pdf.'
holds: []
fit_args:
  mu: 0
examples: |
  set.seed(1)
  x <- rml(200, mlht())
  mlht(x, mu = 0L)

group: normal and Student
construction:
- folding
relations:
  transform_of:
  - family: lst
    at:
      mu: 0
      sigma: sigma
      df: nu
    transform: abs
    shift: mu
  special_case_of:
  - family: fpareto
    at:
      shape1: nu/2
      shape2: 2
      shape3: 0.5
      scale: sigma*sqrt(nu)
      min: mu
fit:
  method: profile-search
  certified: partial
  note: 'ht/README.md, Details; ht/mle.hpp (public fitter and held overloads); ht/test.cpp: Endpoint and held-df
    scale solve are exact/monotone; free-df profile is demonstrably multimodal and a finite grid cannot exclude
    narrower peaks.'
areas:
- measurement and errors
classification: specialist
classification_reason: Used to model one-sided heavy-tailed deviations measured from a lower endpoint.
summary: Half-t provides a model for one-sided heavy-tailed deviations measured from a lower endpoint.
aliases: []
key_references:
- 2
existence:
  uniqueness: unknown
  statement: >-
    With all coefficients free, the endpoint is the sample minimum and estimated $\nu$ is restricted to $\nu\ge1$. A finite MLE is not guaranteed: excessive endpoint ties cause collapse and a half-normal limit can prevent attainment. Global shape uniqueness is unknown. Holding the endpoint permits any estimated $\nu>0$; with scale free, any observation at that held endpoint then makes the free-shape likelihood unbounded. With $\nu$ held and scale free, a unique scale maximum exists exactly when $\nu(n-k)>k$, where $k$ counts endpoint observations; equality gives a finite unattained supremum and the reverse inequality gives unbounded likelihood. Holding scale removes collapse, but free shape still requires boundary comparisons. With both shape and scale held, a free endpoint is uniquely the sample minimum. Holding all coefficients leaves likelihood evaluation.
  reason: >-
    The density decreases strictly with distance from the endpoint, so a free endpoint is the sample minimum. At fixed shape the log-scale score decreases strictly from $\nu(n-k)-k$ to $-n$ when a positive residual exists. The displayed two-mode example in Details rules out general shape-profile unimodality.
---

The half-t models nonnegative Student t magnitudes with an endpoint, translated from zero.

## Details

The endpoint $\mu$ is the smallest possible observation. Scale $\sigma$ is the ordinary Student scale, and $\nu$ controls tail weight. Holding the endpoint fits magnitudes measured from a known threshold; estimating it fits a translation as well.

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-halfthreshold.md) derives these boundary conditions, including the relevant held parameters.

The finite family is identifiable: its tail exponent determines $\nu$, and its density at the endpoint then determines $\sigma$. Any coefficient may be held fixed.

With free scale and held $\nu$, the log-scale score $\sum_i(\nu+1)w_i^2/(\nu+w_i^2)-n$, $w_i=(x_i-\mu)/\sigma$, is strictly decreasing. A unique root exists iff $\nu(n-k)>k$, where $k$ counts observations at the free or held endpoint. Equality gives a finite unattained scale-zero supremum; a strict reverse inequality gives unbounded likelihood.

With both $\nu$ and $\sigma$ free and $\mu$ held, any tie at that held value permits a small positive $\nu$ and scale collapse with unbounded likelihood; this is a measure-zero event for continuous data away from an exact coincidence. Holding scale removes collapse regardless of $\mu$, so ties are allowed in that subset; with a free $\mu$, free $\nu$ still uses the declared $\nu\ge1$ fitting domain. All held coefficients must be finite, $\mu$ of any sign, $\nu$ and $\sigma$ positive. With shape and scale held, a held endpoint leaves likelihood evaluation, including on data entirely at that endpoint; a free endpoint is estimated by the sample minimum.

For positive $x-\mu$, $f(x;\nu,\sigma,\mu)\leq 2t_\nu(0)(1+1/\nu)^{-(\nu+1)/2}/(x-\mu)$, by maximizing each individual density over scale at $\sigma=x-\mu$. This bound tends to zero as $\nu\to0$ uniformly in scale, ruling out a small-shape escape away from the endpoint. With scale held, densities at the endpoint also tend to zero as $\nu \to 0$ (the half-t mode there is $O(\sqrt\nu)$ at fixed $\sigma$). The other escaping face is the half-normal, compared at its exact root mean square scale (or at the held scale). Ties within $10^{-8}n$ log likelihood units select the half-normal face and stop.

The shape profile need not be unimodal: $(0.12773,12.5979,3.91782,2.58658,0.138212)$ has local maxima near $\nu=0.41508$ and $\nu=1.17074$ (independently checked with the reference density), the former higher. A grid in $\log\eta=-\log\nu$ refines every sampled local maximum, after exact monotone scale profiling. The small-shape bound determines the lower search extent; the large-shape extent is $10^8$, with an unresolved edge reported as a search failure. This numerical global search is not a certificate against peaks narrower than its grid. Scale is the ordinary Student t scale, not a standard deviation. This paragraph's unimodality example holds $\mu=0$; the same grid and golden-section refinement are reused for a free endpoint, restricted to $\nu\ge1$.

With $\mu$ held, the translated endpoint reduces to this fixed-zero-endpoint model on $x-\mu$, exactly as above (the measure-zero exemption noted there).

With $\mu$ free, the likelihood increases in $\mu$ up to the sample minimum (the density is strictly decreasing away from the endpoint for every finite $\nu$, $\sigma$), so the free endpoint is exactly $\mu=\min_i x_i$, including tied minima; a held $\mu$ above any observation is invalid. Writing $k$ for that tie multiplicity: holding $\nu$ reduces to the fixed-endpoint criteria above, with $k$ now counting ties at the minimum. Free $\nu$ is restricted to $\nu\ge1$ whenever $\mu$ is free, including when scale is held. With $\nu$ and $\sigma$ both free, any tie ($k\ge1$, which a free endpoint always produces) would make the unrestricted model collapse as above; restricting $\nu\ge1$ bounds the collapse exponent at the break-even point $\nu(n-k)=k$, i.e. $2k=n$.

For $2k>n$ the restriction does not help (the break-even $\nu_*=k/(n-k)$ itself exceeds 1, so part of the allowed range still collapses): unbounded. For $2k<n$, $\nu_*<1$, so every allowed $\nu\ge1$ is in the existence region, and the restricted profile search (compared against the half-normal escape, as always) is data-dependent. For $2k=n$ exactly, $\nu_*=1$: the restricted search explores $\nu>1$ and additionally compares the explicit finite $\nu=1$, $\sigma\to0$ limit
$$
n\log(2) + n\,\mathrm{logmode}(1) - 2\sum_{x_i>\min(x)}\log(x_i-\min(x)) = n\log(2/\pi) - 2\sum_{x_i>\min(x)}\log(x_i-\min(x)),
$$
an unattained supremum (a scale-boundary diagnostic) when it beats both the interior search and the normal escape, derived by taking the exact $\sigma\to0$ limit of each observation's log density at the break-even $\nu$, where the leading $\log\sigma$ terms cancel by construction. ($\mathrm{logmode}(\nu)$ is the ordinary Student t log density at zero, scale 1; at $\nu=1$ it is $-\log\pi$. The factor 2 for the half density is included separately.) See the [held-endpoint report](../../../../../dev/research/evaluations/2026-10-29-p4-report.md) for the full derivation.

Evaluation uses the internal d/p/q/r functions.

### Outer search evidence

| Question | Evidence |
|---|---|
| Endpoint and inner scale | Proved as above: the endpoint is the minimum when free, and the log-scale score is strictly decreasing with the stated tie criterion. The implemented bisection is numerical. |
| Shape on bounded intervals | The displayed two-mode sample disproves general unimodality. A grid and golden-section refinement do not bound missed maxima. |
| Shape faces | The pointwise small-shape bound above proves loss of likelihood in the stated cases. Convergence to the half-normal face does not by itself bound the entire finite-df tail beyond the implementation's cutoff. |
| Global certificate | Open: a computable uniform upper bound on the profiled likelihood over each interval and over the large-shape tail, including rounding and inner-solve errors, is needed. The present fit is numerical. |

For a finite bounded $\nu$ interval strictly inside the scale-existence
region, a data-dependent scale bracket is available. Let $p=n-k$ be the
number of strictly positive residuals, $d_{\min}$ and $d_{\max}$ their
smallest and largest values, and $q=n/[p(\nu+1)]<1$. At the scale root,
$\sum_{d_i>0}d_i^2/(\nu\sigma^2+d_i^2)=n/(\nu+1)$, so monotonicity
in $d_i$ gives
$$\frac{d_{\min}}{\sqrt{\nu q/(1-q)}}\leq\sigma_*(\nu)
\leq\frac{d_{\max}}{\sqrt{\nu q/(1-q)}}.$$
The bounds extend uniformly over a compact shape interval separated from
$\nu p=k$. Combined with the analytic shape score and envelope theorem,
they imply a finite profile derivative bound there. A practical evaluated
bound, its floating-point error, and treatment of the tie boundary are
still missing; this existence statement is not an implemented certificate.

More explicitly, with $t=\log\nu$, $w_i=d_i^2/(\nu\sigma^2)$ and
$A(\nu)=\log\Gamma((\nu+1)/2)-\log\Gamma(\nu/2)-\frac12\log(\pi\nu)$,
the fixed-scale score is
$$\partial_t\ell=n\nu A'(\nu)-\frac{\nu}{2}\sum_i\log(1+w_i)
 +\frac{\nu+1}{2}\sum_i\frac{w_i}{1+w_i}.$$
For $\nu\in[a,b]$ with $ap>k$, the bracket above implies
$\sigma_*\ge s_0=d_{\min}\sqrt{(ap-k)/(an)}$. Hence
$w_i\le W=d_{\max}^2/(a s_0^2)$ and the profiled score magnitude is at
most $n[\sup_{a\le\nu\le b}|\nu A'(\nu)|+b\log(1+W)/2+(b+1)/2]$.
The envelope theorem applies because the scale maximum is unique and
interior. The analogous held-scale bound uses the held scale for $s_0$.
A computable enclosure of the digamma term and the large-$\nu$ tail is
still needed before this can certify the complete search.

## R notes

Estimated coefficients on an applicable `fit_domain` bound carry `restricted`
and emit a warning of class `univariateML_restricted`. Held coefficients are
not flagged. README hold exemptions apply to free coefficients. Failed
optimizers return a failure status unless the score norm in fitting
coordinates is below `1e-3`; unresolved scalar searches stop.

Supply `mu` explicitly. Holding `mu = 0` gives the ordinary half-t, another numeric value holds a translation, and `mu = NULL` estimates the endpoint. A half-normal escape recommends `mlhnorm` with the same endpoint and any held scale.
