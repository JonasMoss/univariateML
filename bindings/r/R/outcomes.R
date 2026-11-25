#' Fit outcomes and conditions
#'
#' Every estimate carries logical `certified` and `unique` attributes. The
#' `unique` attribute is `TRUE` for a unique estimate, `FALSE` for established
#' multiplicity with a versioned `maximizer_set`, or `NA` when uniqueness
#' remains undetermined. The first row of `maximizers` repeats the returned
#' estimate. Additional rows illustrate the set. `candidates` records
#' comparison evidence. Inference requires known
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
#'
#' [model_select()] signals `univariateML_select_failed` when no candidate can
#' be fitted. It inherits from `univariateML_stop` and carries kind
#' `"select_failed"`. Its reason is `"all-fits-failed"` when fitting was
#' attempted and `"no-eligible-models"` otherwise. The named `failures` list
#' carries the original fit conditions and diagnostic strings for skipped
#' candidates.
#' @name univariateML_outcomes
NULL
