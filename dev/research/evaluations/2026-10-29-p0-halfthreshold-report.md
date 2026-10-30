# P0 half-threshold report (TASK-79)

The six family tables, hold subsets, scripts, numerical results and proposed
README entries are in `dev/research/evaluations/2026-10-29-p0-halfthreshold.md` and
its companion directory. No package code, READMEs, tests, goldens or git
history were changed.

- Proved: half-normal endpoint MLE including tied minima; half-Cauchy
  collapse exponent 1 and exact tie criterion; half-t exponent nu and its
  fixed-nu criterion. The nu ≥ 1 restriction prevents collapse for minimum
  multiplicity below n/2 but leaves the half-normal escape. At exactly n/2,
  the finite scale-zero face also needs comparison.
- Proved: fatigue's unrestricted threshold spike and its fixed-beta spike
  when 3k > n. For 3k = n an explicit finite endpoint profile must be
  compared with interior candidates. With alpha fixed, nonconstant data have
  an attained fit. A compact positive interval for alpha restores existence
  for nonconstant data, but has no weakest endpoints.
- Fatigue also has a normal limit as mu→−∞, beta≈−mu and alpha→0. An upper
  alpha bound alone leaves that escape. This was missing from the first pass.
- `extraDistr::dht(nu=Inf)` evaluates the proper half-normal, unlike the
  README's finite nu domain. If R7 admits that extended-real parameter, the
  half-normal face becomes attained; this policy decision remains open.
- Master accepts but silently ignores beta-prime shape hold arguments.
  Other unsupported holds and the implicit zero thresholds are tabulated.
- On simbox, the independent reference-density multistart sweep covered
  323 sample/hold combinations, including default n=20, 100, 1000, small n,
  ties, endpoints and limit-law samples. All 108 returned master fits matched
  within 1e-6*n; maximum absolute difference 1.01e-11. Folded-normal's
  package density agreed with the independently coded README formula.
- Remaining incomplete: beta-prime free-scale closure and exclusion of every
  unbounded path under an alpha upper bound alone lack full proofs. Numerical
  global optimality is not certified; 29 best multistart runs reported
  nonzero convergence, often on boundary paths. Near-equality decisions and
  the fatigue endpoint example require certified search. These qualifications
  and the exact comparison conditions are in the analysis note.
