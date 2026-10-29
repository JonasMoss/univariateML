---
id: ztbinom
name: Zero-truncated binomial
support: {type: integer, lower: "1", upper: "size", closed: [true, true]}
params:
- name: size
  role: trials
  domain: '[1, inf)'
  integer: yes
  simulate: 8.0
  kind: design
- name: prob
  role: probability
  domain: '[0, 1]'
  simulate: 0.4
  kind: regular
r:
  density: actuar::dztbinom
  sampler: actuar::rztbinom
  control:
    size: "Optional fixed size; NULL estimates size."
    reltol: "Tolerance for scalar profile search."
    iterlim: "Maximum scalar search iterations."
    prob: "Optional fixed probability in [0, 1]; NULL estimates it."
log_density: loggamma(size+1)-loggamma(x+1)-loggamma(size-x+1)+x*log(prob)+(size-x)*log(1-prob)-log(1-(1-prob)**size)
limits:
- model: ztpois
  when: $size \to \infty$ with the positive conditional mean fixed
  detect: the maximised face likelihood wins within 1e-8 times the sample size
  status: PoissonLimit
fit_args:
  size: ~
---

Zero-truncated binomial distributions model positive counts.

## Details

Actuar defines the point mass at one at `prob = 0`. This boundary is returned as an attained member.
Holding size fixed removes the infinite-size escape. The point mass at one remains attained for every fixed size.

A held probability is preserved exactly. With probability strictly between zero and one, the fixed-probability integer search bounds the remaining tail before returning; exhausting its work budget is an error. Boundary probabilities must give positive likelihood to the observations.
