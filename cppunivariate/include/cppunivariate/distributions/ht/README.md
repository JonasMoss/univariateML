---
id: ht
name: Half-t
support: {type: real, lower: "mu", upper: inf, closed: [true, false], endpoint_log_density: {lower: "log(2) + lgamma((nu+1)/2) - lgamma(nu/2) - log(nu*pi)/2 - log(sigma)"}}
params:
- name: mu
  role: location
  domain: (-inf, inf)
  simulate: 0
  kind: threshold
  advice: mu = 0 is the ordinary half-t; NULL estimates the translated endpoint, restricting nu >= 1.
- name: nu
  role: shape
  domain: (0, inf)
  simulate: 5
  fit_domain:
    lower: 1.0
    restriction:
      mechanism: collapse
      exempt_when_held: [[mu]]
      reason: nu >= 1 prevents the free-endpoint scale collapse for ordinary samples;
        holding mu exempts the free shape, with exact endpoint ties still diagnosed.
  kind: regular
- name: sigma
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
    name: half-normal with the same endpoint; use mlhnorm(x, mu = 0) after a held
      translation, or mlhnorm(x, mu = NULL) with a free one
    when: '$\nu \to \infty$'
    status: NormalLimit
  - name: unbounded scale collapse
    when: '$\sigma \to 0$, $\nu(n-k)<k$, where $k$ counts observations at the
      free or held endpoint'
    status: Unbounded
  - name: finite unattained scale-zero supremum
    fixed: [nu]
    when: '$\sigma \to 0$, $\nu(n-k)=k$'
    status: ScaleBoundary
references:
  - extraDistr package, HalfT documentation (density and parameterization).
holds: []
fit_args:
  mu: 0
examples: |
  set.seed(1)
  x <- rml(200, mlht())
  mlht(x, mu = 0L)
---

The half-t models nonnegative Student t magnitudes with an endpoint, translated from zero.

## Details

The finite family is identifiable: its tail exponent determines `nu`, and its density at the endpoint then determines `sigma`. Any coefficient may be held fixed. With free scale and held `nu`, the log-scale score $\sum_i(\nu+1)w_i^2/(\nu+w_i^2)-n$, $w_i=(x_i-\mu)/\sigma$, is strictly decreasing. A unique root exists iff $\nu(n-k)>k$, where $k$ counts observations at the free or held endpoint. Equality gives a finite unattained scale-zero supremum; a strict reverse inequality gives unbounded likelihood. With both `nu` and `sigma` free and `mu` held, any tie at that held value permits a small positive `nu` and scale collapse with unbounded likelihood; this is a measure-zero event for continuous data away from an exact coincidence. Holding scale removes collapse regardless of `mu`, so ties are allowed in that subset; with a free `mu`, free `nu` still uses the declared `nu >= 1` fitting domain. All held coefficients must be finite, `mu` of any sign, `nu` and `sigma` positive. With `nu` and `sigma` both held, the likelihood is simply evaluated, including on data entirely at the endpoint.

For positive `x - mu`, $f(x;\nu,\sigma,\mu)\leq 2t_\nu(0)(1+1/\nu)^{-(\nu+1)/2}/(x-\mu)$, by maximizing each individual density over scale at $\sigma=x-\mu$. This bound tends to zero as $\nu\to0$ uniformly in scale, ruling out a small-df escape away from the endpoint. With scale held, densities at the endpoint also tend to zero as $\nu \to 0$ (the half-t mode there is $O(\sqrt\nu)$ at fixed $\sigma$). The other escaping face is the half-normal, compared at its exact root mean square scale (or at the held scale). Ties within $10^{-8}n$ log likelihood units select that face and stop with `use mlhnorm(x, mu = 0)` (or `mlhnorm(x, mu = NULL)` when `mu` was free here).

The df profile need not be unimodal: `c(.12773, 12.5979, 3.91782, 2.58658, .138212)` has local maxima near `nu = .41508` and `nu = 1.17074` (independently checked with the reference density), the former higher. A grid in $\log\eta=-\log\nu$ refines every sampled local maximum, after exact monotone scale profiling. The small-df bound determines the lower search extent; the large-df extent is $10^8$, with an unresolved edge reported as a search failure. This numerical global search is not a certificate against peaks narrower than its grid. Scale is the ordinary Student t scale, not a standard deviation. This paragraph's unimodality example holds `mu = 0`; the same grid and golden-section refinement are reused for a free endpoint, restricted to `nu >= 1`.

With `mu` held, the translated endpoint reduces to this fixed-zero-endpoint model on `x - mu`, exactly as above (the measure-zero exemption noted there). With `mu` free, the likelihood increases in `mu` up to the sample minimum (the density is strictly decreasing away from the endpoint for every finite `nu`, `sigma`), so the free endpoint is exactly `mu = min(x)`, including tied minima; a held `mu` above any observation is invalid. Writing `k` for that tie multiplicity: holding `nu` reduces to the fixed-endpoint criteria above, with `k` now counting ties at the minimum. Free `nu` is restricted to `nu >= 1` whenever `mu` is free, including when scale is held. With `nu` and `sigma` both free, any tie (`k >= 1`, which a free endpoint always produces) would make the unrestricted model collapse as above; restricting `nu >= 1` bounds the collapse exponent at the break-even point `nu*(n-k) = k`, i.e. `2k = n`. For `2k > n` the restriction does not help (the break-even `nu* = k/(n-k)` itself exceeds 1, so part of the allowed range still collapses): unbounded. For `2k < n`, `nu* < 1`, so every allowed `nu >= 1` is in the existence region, and the restricted profile search (compared against the half-normal escape, as always) is data-dependent. For `2k = n` exactly, `nu* = 1`: the restricted search explores `nu > 1` and additionally compares the explicit finite `nu = 1`, `sigma -> 0` limit
$$
n\log(2) + n\,\mathrm{logmode}(1) - 2\sum_{x_i>\min(x)}\log(x_i-\min(x)) = n\log(2/\pi) - 2\sum_{x_i>\min(x)}\log(x_i-\min(x)),
$$
an unattained supremum (`ScaleBoundary`) when it beats both the interior search and the normal escape, derived by taking the exact `sigma -> 0` limit of each observation's log density at the break-even `nu`, where the leading `log(sigma)` terms cancel by construction. (`logmode(nu)` is the ordinary Student t log density at zero, scale 1; at `nu = 1` it is `-log(pi)`. The factor 2 for the half density is included separately.) See `notes/evaluations/2026-10-29-p4-report.md` for the full derivation.

Evaluation uses the internal d/p/q/r functions.

Estimated coefficients on an applicable `fit_domain` bound carry `restricted`
and emit a warning of class `univariateML_restricted`. Held coefficients are
not flagged. README hold exemptions apply to free coefficients. Failed
optimizers return a failure status unless the score norm in fitting
coordinates is below `1e-3`; unresolved scalar searches stop.
