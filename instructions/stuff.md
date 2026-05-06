Needed special functions (current uses via `mlcore::special::*`):

| Function        | Used for                                 | Source in R Mathlib     | Difficulty (deps) |
|-----------------|------------------------------------------|-------------------------|-------------------|
| `digamma(x)`    | inverse_digamma; many MLE Newton steps   | `psigamma.c` (`psi`), or `polygamma.c` | High (needs psigamma core, reflection, helpers) |
| `trigamma(x)`   | Burr roots, Beta/Gamma Newton, etc.      | `psigamma.c`/`polygamma.c`             | High (same core as digamma) |
| `tetragamma(x)` | Burr root init, tail expansions          | `psigamma.c`/`polygamma.c`             | High (same core as digamma) |
| `psigamma(x,n)` | Exposed for tests; rarely used in core   | `psigamma.c`/`polygamma.c`             | High (shares core) |
| `lgamma(x)`     | binomial/nbinom/log-likelihoods          | `lgamma.c` + `lgammacor.c` + `stirlerr.c` | Medium (few small helpers) |
| `lbeta(a,b)`    | beta prime log-lik                       | `lbeta.c` + `lgamma`                   | Low (wraps lgamma) |
| `dnorm(x, m,s)` | fatigue log-density                      | `dnorm.c`                              | Low (standalone) |
| `dpois(x, λ)`   | Poisson/ZIP log-lik                      | `dpois.c` + `Rf_d1mach` for underflow? | Medium (needs small machine consts) |
| `dgeom(x, p)`   | geometric log-lik                        | `dgeom.c`                              | Low (simple) |
| `R_pow_di`      | pow with int exponent (used in psigamma) | `nmath.h` helper                        | Low |
| Constants/macros| `ML_NAN`, `ML_POSINF`, `M_PI`, eps, etc. | `nmath.h` standalone branch            | Low |
