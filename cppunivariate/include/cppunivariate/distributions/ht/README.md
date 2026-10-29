---
id: ht
name: Half-t
support: {type: real, lower: 0, upper: inf, closed: [true, false]}
params:
- name: nu
  role: shape
  domain: (0, inf)
  simulate: 3
  fit_domain:
    lower: 1.0
    restriction:
      mechanism: collapse
      exempt_when_held:
      - - mu
      reason: nu >= 1 prevents free-threshold collapse below half minimum multiplicity;
        the current fixed-zero endpoint is exempt.
  kind: regular
- name: sigma
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: extraDistr::dht
  sampler: extraDistr::rht
  control: {}
log_density: log(2) + loggamma((nu+1)/2) - loggamma(nu/2) - log(nu*pi)/2 - log(sigma) - (nu+1)*log(1+x**2/(nu*sigma**2))/2
limits:
  - model: hnorm
    name: half-normal with endpoint zero; use mlhnorm(x, mu = 0)
    when: '$\nu \to \infty$'
    status: NormalLimit
  - name: unbounded scale collapse
    when: '$\sigma \to 0$, $\nu(n-k)<k$, where $k$ counts zeros'
    status: Unbounded
  - name: finite unattained scale-zero supremum
    fixed: [nu]
    when: '$\sigma \to 0$, $\nu(n-k)=k$'
    status: ScaleBoundary
references:
  - extraDistr package, HalfT documentation (density and parameterization).
holds: []
---

The half-t models nonnegative Student t magnitudes with an endpoint fixed at zero.

## Details

The finite family is identifiable: its tail exponent determines `nu`, and its density at zero then determines `sigma`. Both parameters may be held fixed. With free scale and held `nu`, the log-scale score $\sum_i(\nu+1)w_i^2/(\nu+w_i^2)-n$, $w_i=x_i/\sigma$, is strictly decreasing. A unique root exists iff $\nu(n-k)>k$, where $k$ counts zeros. Equality gives a finite unattained scale-zero supremum; a strict reverse inequality gives unbounded likelihood. With both parameters free, any zero permits a small positive `nu` and scale collapse with unbounded likelihood. Holding scale removes this collapse, so zeros are allowed in that subset. All held parameters must be positive and finite. With both held, the likelihood is simply evaluated, including on all-zero data.

For positive data, $f(x;\nu,\sigma)\leq 2t_\nu(0)(1+1/\nu)^{-(\nu+1)/2}/x$, by maximizing each individual density over scale at $\sigma=x$. This bound tends to zero as $\nu\to0$ uniformly in scale, ruling out a small-df escape. With scale held, densities at zero also tend to zero. The other escaping face is the half-normal, compared at its exact root mean square scale (or at the held scale). Ties within $10^{-8}n$ log likelihood units select that face and stop with `use mlhnorm(x, mu = 0)`.

The df profile need not be unimodal: `c(.12773, 12.5979, 3.91782, 2.58658, .138212)` has local maxima near `nu = .41508` and `nu = 1.17074` (independently checked with the reference density), the former higher. A grid in $\log\eta=-\log\nu$ refines every sampled local maximum, after exact monotone scale profiling. The small-df bound determines the lower search extent; the large-df extent is $10^8$, with an unresolved edge reported as a search failure. This numerical global search is not a certificate against peaks narrower than its grid. Scale is the ordinary Student t scale, not a standard deviation.

A free translated endpoint is deliberately excluded: moving it to the sample minimum and choosing sufficiently small positive df makes the likelihood unbounded through scale collapse. The translated half-t faces of the skew-t fitter therefore are not this fixed-zero-endpoint model.

The d/p/q/r functions use extraDistr, with runtime-checked tail repairs.
