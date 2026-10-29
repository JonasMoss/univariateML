---
id: ztgeom
name: Zero-truncated geometric
support:
  type: integer
  lower: 1
  upper: inf
  closed:
  - true
  - false
params:
- name: prob
  role: probability
  domain: (0, 1]
  simulate: 0.4
  kind: regular
r:
  density: actuar::dztgeom
  sampler: actuar::rztgeom
  control: {}
log_density: log(prob)+(x-1)*log(1-prob)
holds:
- held:
  - prob
  status: not-implemented
  reason: The adapter does not accept `prob`; this hold is not implemented (P2).
---

Zero-truncated geometric distributions model positive counts.

## Details

Actuar defines the point mass at one at `prob = 1`. This boundary is returned as an attained member.
