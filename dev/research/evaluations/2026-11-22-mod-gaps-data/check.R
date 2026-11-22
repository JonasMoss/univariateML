# Run from the worktree root after the R test recipe has built the package DLL.
pkgload::load_all("univariateML-r", compile = FALSE, quiet = TRUE)
refs <- jsonlite::fromJSON("dev/research/evaluations/2026-11-22-mod-gaps-data/reference.json")

for (bounds in names(refs)) {
  window <- as.numeric(strsplit(bounds, ":", fixed = TRUE)[[1L]])
  counts <- seq.int(window[1L], window[2L])
  cuts <- counts[-length(counts)]
  ref <- refs[[bounds]]
  obj <- mlnbinom(size = 1e8, prob = .5, truncate = window)
  relative_error <- function(actual, expected) max(abs(actual / expected - 1))
  errors <- c(
    log_pmf = max(abs(dml(counts, obj, log = TRUE) - ref$log_pmf)),
    dominant_log_pmf_relative = relative_error(
      dml(tail(counts, 1), obj, log = TRUE), tail(ref$log_pmf, 1)),
    log_cdf = max(abs(pml(counts, obj, log.p = TRUE) - ref$log_cdf)),
    log_survival_relative = relative_error(
      pml(cuts, obj, lower.tail = FALSE, log.p = TRUE), ref$log_survival)
  )
  stopifnot(all(errors < 1e-12))
  matches <- 0L
  for (lower in c(TRUE, FALSE)) for (logged in c(TRUE, FALSE)) {
    targets <- if (logged) log(ref$probabilities) else ref$probabilities
    expected <- if (lower) ref$lower_quantiles else ref$upper_quantiles
    stopifnot(all(qml(targets, obj, lower.tail = lower, log.p = logged) == expected))
    matches <- matches + length(targets)
  }
  cat(sprintf("window [%s], %d/%d quantiles match\n", bounds, matches, matches))
  print(errors)
}

for (id in c("lgser", "nbinom")) {
  at <- if (id == "lgser") 1 else 0
  obj <- if (id == "lgser") mllgser(theta = 0, zero = "none") else
    mlnbinom(size = 0, prob = .5, zero = "none")
  x <- at + c(-1, 0, 1)
  stopifnot(all(dml(x, obj) == c(0, 1, 0)), all(pml(x, obj) == c(0, 1, 1)),
            all(qml(c(0, .5, 1), obj) == at), all(rml(512, obj) == at))
  cat(sprintf("%s point mass at %d, 512/512 draws match\n", id, at))
}
