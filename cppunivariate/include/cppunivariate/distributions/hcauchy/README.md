---
id: hcauchy
name: Half-Cauchy
support: {type: real, lower: 0, upper: inf, closed: [true, false]}
params:
- name: sigma
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dhcauchy
  sampler: rhcauchy
  control:
    sigma: Held positive finite scale; NULL estimates it.
log_density: log(2/pi) - log(sigma) - log(1+x**2/sigma**2)
limits:
  - name: unbounded scale collapse
    when: '$\sigma \to 0$ with more than half the observations zero'
    status: Unbounded
  - name: finite unattained scale-zero supremum
    when: '$\sigma \to 0$ with exactly half the observations zero'
    status: ScaleBoundary
references:
  - extraDistr package, HalfCauchy documentation (density and parameterization).

---

The half-Cauchy models nonnegative magnitudes with heavy tails and an endpoint fixed at zero.

## Details

If $k$ of $n$ observations are zero, a positive-scale MLE exists uniquely iff $2k<n$. Indeed, the log-scale score is $n-\sum_i 2\sigma^2/(\sigma^2+x_i^2)$, strictly decreasing from $n-2k$ to $-n$ whenever some observation is positive. For $2k=n$ it is strictly negative at every positive scale: the finite scale-zero supremum is unattained. For $2k>n$, including all-zero data, the likelihood is unbounded as scale tends to zero. Negative and nonfinite observations are rejected. Even one strictly positive observation identifies a unique scale estimate.

Evaluation uses the internal d/p/q/r functions. The endpoint density is positive.

A positive finite held `sigma` is evaluated directly, including for all-zero
data; the scale-collapse conditions apply only when scale is estimated.
