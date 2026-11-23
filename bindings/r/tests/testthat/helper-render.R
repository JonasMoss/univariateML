fit_payload <- function(fit) {
  if (!inherits(fit,"univariateML")) return(fit)
  out <- attributes(fit)
  out$model <- attr(fit,"comparison_model")
  out$estimates <- coef(fit)
  out
}
fit_diagnostic <- function(fit) {
  if (inherits(fit,"error")) conditionMessage(fit) else attr(fit,"outcome_message")
}
