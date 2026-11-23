fit_payload <- function(fit) {
  if (!inherits(fit,"univariateML")) return(fit)
  out <- attributes(fit)
  if (is.null(out$maximizer_set) && is.null(out$candidates)) out$candidates <- out$maximizers
  out$model <- attr(fit,"comparison_model")
  out$estimates <- coef(fit)
  out$kind <- "estimate"
  out
}
fit_diagnostic <- function(fit) {
  if (inherits(fit,"error")) conditionMessage(fit) else attr(fit,"outcome_message")
}
