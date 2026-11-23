#' Fit outcomes and conditions
#'
#' Every estimate carries logical `certified` and `unique` attributes. The
#' `unique` attribute can be `TRUE`, `FALSE`, or `NA` when the comparison
#' leaves uniqueness undetermined. A versioned `maximizer_set` describes
#' the complete set for fitters that publish typed sets. The first row of
#' `maximizers` repeats the returned estimate. Additional rows illustrate the
#' set. `candidates` records comparison evidence. Inference requires known
#' uniqueness. Model selection accepts estimates with any uniqueness flag.
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
