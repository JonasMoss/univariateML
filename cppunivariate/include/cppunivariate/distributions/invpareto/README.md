---
id: invpareto
name: Inverse Pareto
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
- name: shape
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: scale
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dinvpareto
  sampler: rinvpareto
  control:
    shape: Optional held shape; NULL estimates it.
    scale: Optional held scale; NULL estimates it.
    scale0: Optional positive finite starting value; NULL uses the automatic start.
    reltol: Tolerance for the underlying fit.
    iterlim: Maximum number of iterations.
log_density: log(shape) + log(scale) - 2*log(x) - (shape+1)*log(1+scale/x)
limits:
  - model: invexp
    when: '$\mathrm{shape} \to \infty$ with $\mathrm{shape} \cdot \mathrm{scale}$ fixed'
    detect: the reciprocal Lomax profile gap maximised over 0 < u < ubar is at most 1e-8*n
    status: ExponentialLimit
references:
  - "del Castillo, J. and Daoudi, J. (2009). Estimation of the generalized Pareto distribution. Statistics & Probability Letters 79(5), 684–688. doi:10.1016/j.spl.2008.10.021."
  - "Grimshaw, S. D. (1993). Computing maximum likelihood estimates for the generalized Pareto distribution. Technometrics 35(2), 185–191. doi:10.1080/00401706.1993.10485040."
  - "Klugman, S. A., Panjer, H. H. and Willmot, G. E. (2012), Loss Models, From Data to Decisions, Fourth Edition, Wiley."
  - "Dutang, C., Goulet, V., & Pigeon, M. (2008). actuar: An R package for actuarial science. Journal of Statistical Software, 25(7), 1-37."
examples: |
  set.seed(1)
  x <- rml(200, mlinvpareto())
  mlinvpareto(x)
---

The inverse Pareto distribution models positive heavy-tailed data.

## Details

The fit is the reciprocal of the Lomax fit on `1/x`.

The MLE exists exactly when the Lomax MLE of `1/x` exists (see `mllomax`): with $y_i = (1/x_i)/\overline{1/x}$, $\psi(u)=n^{-1}\sum_i\log(e^{-u}+uy_i)$ must be nonpositive for some $0<u<\bar u$, $\bar u = \overline{1/x}\,(\prod_i x_i)^{1/n}$; the implementation prefers the inverse exponential face within $10^{-8}n$ log-likelihood units. The moment condition `mean(1/x^2) < 2*mean(1/x)^2` decides only local behaviour at that face: `mlinvpareto(c(1, 0.01))` has an MLE. A pair has an MLE iff larger/smaller exceeds approximately 28.81.

Holding either shape or scale gives a unique finite fit for positive samples, including constant data. The inverse exponential escape concerns the fit with both free.
