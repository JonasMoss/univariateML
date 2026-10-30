# Shared sample-size convention for package-defined random generators.
univariateML_rng_length <- function(n) {
  if (length(n) != 1L) return(length(n))
  if (!(is.numeric(n) || is.logical(n)) || is.na(n) || !is.finite(n) || n < 0 || n > .Machine$integer.max)
    stop("invalid arguments", call. = FALSE)
  as.integer(n)
}
