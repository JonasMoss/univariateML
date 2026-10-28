# New distributions from issues #52 and #7; reference-package imports

**Status:** evaluated 2026-10-28 (five research reports in
`notes/evaluations/2026-10-28-*.md`); nothing implemented. VGAM is excluded
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
