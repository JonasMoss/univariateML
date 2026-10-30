---
id: gompertz
name: Gompertz
support: {type: real, lower: 0, upper: inf, closed: [true, false]}
params:
- name: a
  role: rate
  domain: (0, inf)
  simulate: 1.0
  kind: regular
- name: b
  role: shape
  domain: (0, inf)
  simulate: 1.0
  kind: regular
r:
  density: dgompertz
  sampler: rgompertz
  control:
    a: Optional held a; NULL estimates it.
    b: Optional held b; NULL estimates it.
    reltol: Relative step tolerance in `b * mean(x)`, with an absolute floor of the same size in these dimensionless units.
    iterlim: Maximum number of iterations.
log_density: log(a) + b*x - (a/b)*(exp(b*x)-1)
limits:
- name: unbounded likelihood
  fixed: [b]
  when: All observations are zero and a is free.
  status: Unbounded
  message: unbounded
- name: exponential face
  model: exp
  fixed: [a]
  when: sum(x) <= a*sum(x^2)/2 and sum(x)>0.
  status: Overdispersed
  message: mlexp
- model: 'exp'
  when: '$b \to 0$ with $a$ fixed'
  detect: 'mean(x^2) >= 2*mean(x)^2'
  status: 'Overdispersed'
references:
  - "Lenart, A. (2012). The Gompertz distribution and Maximum Likelihood Estimation of its parameters - a revision. MPIDR Working Paper WP 2012-008."
examples: |
  set.seed(1)
  x <- rml(200, mlgompertz())
  mlgompertz(x)
---

The Gompertz distribution has an exponentially increasing hazard and is used to model lifetimes.

## Details

The hazard is $a\exp(bx)$, with both parameters positive.

With a held and b free, a positive finite maximizer exists exactly when sum(x) > a*sum(x^2)/2; otherwise the supremum is the exponential face (use mlexp). All-zero data with a held give a flat likelihood in b, and b=1 is returned. With b held, a is finite when sum(x)>0; all-zero data have unbounded likelihood in a.

### Adapter independence (2026-10-30)

The fully free constant-sample rejection now lives in `gompertz_mle` as `Unbounded`, preserving the former adapter decision. Conditional all-zero handling remains in the core.
