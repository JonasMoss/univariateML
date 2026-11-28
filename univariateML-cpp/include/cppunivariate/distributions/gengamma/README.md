---
id: gengamma
inference_numeric:
  - regime: finite interior including Q=0
    available: false
    condition: 'finite mu and Q, finite sigma > 0; any nonempty coordinate subset free'
    reason: 'The interior is regular, including Q=0, but a numerical inference adapter has not been implemented.'
    classification: 'dev/research/evaluations/2026-11-10-gg-theory-report.md#inference-regimes'
name: Prentice generalized gamma
aliases: [generalized gamma]
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
- {name: mu, symbol: '\mu', role: location, domain: '(-inf, inf)', simulate: 0, kind: regular}
- {name: sigma, symbol: '\sigma', role: scale, domain: '(0, inf)', simulate: 1, kind: regular}
- {name: Q, symbol: 'Q', role: shape, domain: '(-inf, inf)', simulate: 0, kind: regular}
r:
  density: dgengamma
  sampler: rgengamma
  control:
    mu: Optional held log-location; NULL estimates it.
    sigma: Optional held positive log-scale; NULL estimates it.
    Q: Optional held signed shape; NULL estimates it.
    tol: Requested coefficient accuracy (default 1e-10).
    iterlim: 'Work limit per solve: 1 * min(iterlim, 100000) solver iterations; per certificate: 128 * min(iterlim, 100000) certificate evaluations.'
log_density: 'Piecewise((-log(sigma)-log(x)-log(2*pi)/2-((log(x)-mu)/sigma)**2/2, Eq(Q,0)), (log(Abs(Q))-log(sigma)-log(x)+(log(Q**(-2))+Q*(log(x)-mu)/sigma-exp(Q*(log(x)-mu)/sigma))/Q**2-loggamma(Q**(-2)), True))'
limits:
  - model: power
    firmness: proved
    when: '$Q\to +\infty$, $\sigma |Q|\to d>0$, and $\mu\to\mu_0$. For the full power trace require $\varepsilon(\log x_i-\mu_0)\le0$ for all observations, where $\varepsilon=\operatorname{sign}Q$. If an observation equals the endpoint on the log scale, require $k\exp(A\varepsilon(\mu_0-\mu)/k)\to0$, where $k=Q^{-2}$ and $A=1/(\sigma |Q|)$. Holding $\mu=\mu_0$ suffices.'
    detect: 'With all coefficients free, a strict signed-profile comparison certifies the power face over the Pareto face and both signed interiors. With only location held at zero, the same limit holds for a balanced pair whose upper endpoint is exactly one.'
    status: PowerBoundary
    message: power
    detect_firmness: proved
    detect_proof: ["dev/research/evaluations/2026-11-10-gg-theory-report.md#free-fit-signed-profile-and-certificate", "dev/research/evaluations/2026-11-26-gengamma-twopoint-report.md#held-coefficients"]
    proof: ["dev/research/evaluations/2026-11-10-gg-theory-report.md#free-fit-signed-profile-and-certificate"]
  - model: pareto
    firmness: proved
    when: '$Q\to -\infty$, $\sigma |Q|\to d>0$, and $\mu\to\mu_0$. For the full Pareto trace require $\varepsilon(\log x_i-\mu_0)\le0$ for all observations, where $\varepsilon=\operatorname{sign}Q$. If an observation equals the endpoint on the log scale, require $k\exp(A\varepsilon(\mu_0-\mu)/k)\to0$, where $k=Q^{-2}$ and $A=1/(\sigma |Q|)$. Holding $\mu=\mu_0$ suffices.'
    detect: 'With all coefficients free, a strict signed-profile comparison certifies the Pareto face over the power face and both signed interiors. With only location held at zero, the same limit holds for a balanced pair whose lower endpoint is exactly one.'
    status: ParetoBoundary
    message: Pareto
    detect_firmness: proved
    detect_proof: ["dev/research/evaluations/2026-11-10-gg-theory-report.md#free-fit-signed-profile-and-certificate", "dev/research/evaluations/2026-11-26-gengamma-twopoint-report.md#held-coefficients"]
    proof: ["dev/research/evaluations/2026-11-10-gg-theory-report.md#free-fit-signed-profile-and-certificate"]
  - kind: nonexistence
    name: unbounded likelihood
    firmness: proved
    when: 'The free location and scale allow concentration at a common positive observation'
    detect: 'All observations are equal and both $\mu$ and $\sigma$ are free'
    status: Unbounded
    message: unbounded
    detect_firmness: proved
    detect_proof: ["dev/research/evaluations/2026-11-10-gg-theory-report.md#free-fit-signed-profile-and-certificate"]
  - kind: nonexistence
    name: unbounded likelihood at held location
    firmness: proved
    fixed: [mu]
    when: 'The scale is free and all observations equal $e^\mu$'
    detect: 'Every observation equals the held-location mode and $\sigma$ is free'
    status: Unbounded
    message: unbounded
    detect_firmness: proved
    detect_proof: ["dev/research/evaluations/2026-11-10-gg-theory-report.md#mu-held-sigma-and-q-free"]
holds: []
references:
- 'Prentice, R. L. (1974). A log gamma model and its maximum likelihood estimation. Biometrika 61(3), 539–544. doi:10.1093/biomet/61.3.539.'
- 'Stacy, E. W. (1962). A generalization of the gamma distribution. The Annals of Mathematical Statistics 33(3), 1187–1192. doi:10.1214/aoms/1177704481.'
- 'flexsurv: Generalized gamma distribution, https://chjackson.github.io/flexsurv/reference/GenGamma.html.'
summary: The Prentice generalized gamma unifies transformed gamma, inverse transformed gamma and lognormal models.
classification: specialist
classification_reason: Flexible positive-support model for survival times and related data.
group: gamma and Weibull
construction: [generalisation]
areas: [reliability and survival, waiting times and queues]
key_references: [1, 2]
existence:
  uniqueness: unknown
  statement: >-
    Constant positive data have unbounded likelihood when location and scale
    are free. For nonconstant data the free model can escape to power or
    Pareto. With $Q$ held the nonconstant-data conditional maximum is unique;
    with $\sigma$ held a finite maximum exists. For a balanced pair with
    only location held at the exact log of a sample endpoint, the corresponding
    power or Pareto path reaches the all-free supremum. General uniqueness and the joint
    corners with $\mu$ held remain open.
  reason: >-
    Fixed nonzero $Q$ gives a strictly concave profile in signed log-power;
    $Q=0$ gives the normal likelihood for the log observations. With $\sigma$
    fixed, the mode-density bound tends to zero as $|Q|$ grows. The power and
    Pareto paths and held-coordinate arguments are established in the
    [likelihood analysis](../../../../../dev/research/evaluations/2026-11-10-gg-theory-report.md).
  equivariance:
    group: power-scale
    map: {mu: "log(A) + B*mu", sigma: "B*sigma"}
    evidence: >-
      $\log X$ is a location-scale variable with location $\mu$ and scale
      $\sigma$. dev/validation/equivariance.py shows the density identity
      symbolically (SymPy) and at 24 seeded points on the dossier density (40
      digits) and dpqr.hpp (binary64).
  two_point:
    outcome: nonexistence(unattained)
    evidence: >-
      With all coefficients free and equal counts at two distinct values,
      every finite fit lies strictly below the common power and Pareto
      supremum. Both limiting models have exponent
      $2/\log(x_{\max}/x_{\min})$.
      [Proof](../../../../../dev/research/evaluations/2026-11-25-gbshape-math-report.md).
relations: {}
fit:
  method: profile-search
  certified: partial
  note: 'Fixed-Q conditional fits, certain exactly symmetric Q=0 fits, separated all-free parent-side winners, and strictly winning power or Pareto faces have conditional certificates. Other successful local searches are marked uncertified.'
examples: |
  set.seed(1)
  x <- rml(200, mlgengamma())
  mlgengamma(x)
---

The Prentice generalized gamma family includes the lognormal at $Q=0$.

With free $\mu$ and a relative spread $(\max x-\min x)/\min x$ below $10^{-6}$ (and not zero), the fit works on the relative logs $\log(x_i/x_1)$ and shifts $\mu$ and the log likelihood back by $\log x_1$ and $-n\log x_1$. Binary64 logs are too coarse for the spread there: their rounding error $8\epsilon\lvert\log x\rvert$ can exceed it.

## Details

At fixed finite $Q$, holding scale permits a unique location estimate even for
constants. Holding location permits a unique scale unless every log
observation equals that location. [Proof](../../../../../dev/research/evaluations/2026-11-25-transfer-math-report.md#appendix-dossier-ready-existence-and-uniqueness-text).
