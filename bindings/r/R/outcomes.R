#' Fit outcomes and conditions
#'
#' Every estimate carries logical `certified` and `unique` attributes. An
#' uncertified estimate lacks a global certificate; a non-unique estimate has
#' multiple maximizers. `maximizers` describes listed non-unique solutions;
#' `candidates` records comparisons the certificate could not separate.
#' Inference refuses non-unique estimates; model selection accepts them.
#'
#' A limit names a limiting model; `nomle` records proved nonexistence.
#' A budget stop exhausts the work cap. An unresolved stop has no candidate
#' to return after an undecided comparison. Numerical stops report failed
#' computation or accuracy; invalid and unsupported stops reject requests.
#'
#' Stops carry `kind` and `reason`. Kinds and condition classes are
#' `limit`, `nomle`, `budget`, `unresolved`, `numerical`, `invalid`, and
#' `unsupported`, each prefixed with `univariateML_`. Tolerance and conditioning
#' stops have `univariateML_tolerance` or `univariateML_conditioning` before
#' `univariateML_numerical`. All classes end with `univariateML_stop`, `error`,
#' `condition`. Limits also carry `certified`: FALSE means no improvement above
#' the likelihood margin was found.
#'
#' Reasons are `none`, `unbounded`, `unattained`, `invalid-input`,
#' `invalid-argument`, `unsupported`, `no-convergence`, `evaluation`,
#' `tolerance`, `conditioning`, and `unresolved`. Non-unique inference refusals
#' use reason `non-unique` and class `univariateML_unsupported`.
#' @name univariateML_outcomes
NULL
