# New distributions from issues #52 and #7; reference-package imports

**Status:** evaluated 2026-10-28 (six research reports in
`notes/evaluations/2026-10-28-*.md`). Implemented: ZINB, GPD (constrained),
and the actuar reciprocals invexp, invpareto, invparalogis. VGAM is excluded
as a source (unreliable).

## Recommendations

| Distribution | Reference (non-VGAM) | MLE | Verdict |
|---|---|---|---|
| zero-inflated negative binomial | extraDistr `d/p/q/rzinb` | finite for some samples; NB boundary attained, ZIP/Poisson limits | **next**, M |
| generalized Pareto | extraDistr `dgpd` (evd as cross-check) | unbounded unless the shape is bounded; fixed threshold, shape >= -1/2 | **next, constrained**, M |
| folded normal | greybox `d/p/q/rfnorm` | always attained with `mu >= 0`; half-normal is a member | add if greybox's dependency weight is acceptable, M |
| beta-binomial | extraDistr `dbbinom` (no `q`) | binomial / two-point limits; free size harder | later, fixed size first |
| beta negative binomial | extraDistr `dbnbinom` (no `q`) | NB/Poisson limits; size-beta ambiguity | later, L |
| hypergeometric | stats `dhyper` | finite grid; escapes to binomial/Poisson when unrestricted | later, known population sizes |
| Conway-Maxwell-Poisson | COMPoissonReg | interior only for intermediate statistics | later (best discrete candidate), L |
| Conway-Maxwell-binomial | RTMBdist | fixed size first; free size escapes to CMP | later |
| zeta, Yule-Simon | poweRlaw / cpd (incomplete or shifted) | unique unless all ones | later: no clean d/p/q/r source |
| GEV | extraDistr `dgev`, evd | unbounded for every sample; needs both shape bounds | later, L |
| shifted Gompertz | extraDistr `dsgomp` (no `q`) | finite in closure (exponential member) | later |
| truncated normal (known bounds) | extraDistr `dtnorm` | truncated exponential / uniform boundaries | later; tail accuracy of the reference |
| generalized gamma | flexsurv, ggamma, gamlss.dist | lognormal / power / Pareto boundaries | later, L |
| exponentially modified Gaussian | emg, gamlss.dist `dexGAUS` | normal and translated-exponential limits | later; emg's quantiles defective |
| triangular | extraDistr `dtriang` | always attained for nonconstant data (corner-mode members included); mode at an observation; unique concave endpoint problem per mode candidate; ties possible | **next**, M known endpoints, L full (`2026-10-28-tier2.md`); `ptriang(0, 0, 1, 0)` is `NaN` |
| Huber density | extraDistr `dhuber` | fixed epsilon: attained, concave in $(1/\sigma, \mu/\sigma)$; free epsilon: normal / Laplace faces, n = 2 never has a finite MLE | fixed epsilon M, free L; `phuber` `NaN` in far tails |
| discrete Weibull I | extraDistr `ddweibull` | finite MLE iff range >= 2; range 1 escapes to an adjacent two-point law, constant to a point mass; geometric is a member | M; reference log pmf `-Inf` and unfloored CDF |
| Poisson-inverse Gaussian | actuar `dpoisinvgauss` | exact reduction $\hat\mu = \bar x$, 1-D in shape; Poisson face (`mlpois`); overdispersion guarantees a finite MLE | later, L: actuar pmf returns probability 2 near the Poisson limit |

## Reference packages (`2026-10-28-deps.md`)

Decided 2026-10-28: all reference packages stay in Imports (no workflow
complications); sads stays for zipf, with its `qzipf` defect worked around
at runtime and every defect listed in `KNOWN_ISSUES.md`. ZINB and the
constrained GPD are being added.

- sads is used only for zipf's d/p/q/r and imports VGAM (25 recursive
  dependencies); `pzipf`/`qzipf` also have off-by-one errors
  (`pzipf(1.5, 10, 1) = 0`, `qzipf(0.4, 10, 1) = 1`). No clean non-VGAM
  finite-Zipf provider was found (tolerance's `zipfman` has defects).
- Fitting never needs a reference package (the fitters are C++); only
  `dml`/`pml`/`qml`/`rml`, `simulate_default` and the tests do. Moving
  reference packages to Suggests needs `requireNamespace` guards at one
  resolver plus `skip_if_not_installed` in tests and conditional examples.
- Defects found in references: `fGarch::dstd(log = TRUE)` is `-Inf` at
  `x = 1e6, nu = 1e6`; `nakagami::pnaka(-1, 2, 3) = 0.144` (should be 0).

## Trigger to revisit

A decision on the sads/Suggests question; a user request for one of the
families; or the end of the fixed-parameter work (which several of these
need: fixed size, threshold, bounds).
