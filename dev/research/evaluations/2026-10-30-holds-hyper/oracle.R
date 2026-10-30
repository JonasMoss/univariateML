# Independent finite-grid checks of adjacent ratios against reference densities.
stopifnot(requireNamespace("extraDistr", quietly = TRUE))
checks <- 0L
for (N in c(8L, 15L, 40L, 100L)) for (m in seq_len(min(N, 12L))) {
  for (x in list(c(0L, min(m, 2L)), rep(min(m, 1L), 30), c(min(m, 1L), m))) {
    for (k in seq.int(max(x), N - m + min(x))) {
      a <- sum(dhyper(x, m, N-m, k, log=TRUE))
      b <- sum(dhyper(x, k, N-k, m, log=TRUE))
      stopifnot(isTRUE(all.equal(a, b, tolerance=1e-10)))
      checks <- checks + 1L
    }
  }
  for (x in list(c(1L, 2L), rep(3L, 30), c(2L, min(N-m+1L, 5L)))) {
    lo <- max(1L, max(x) - (N-m)); hi <- min(m, min(x))
    if (lo >= hi) next
    r <- lo:hi
    ll <- vapply(r, function(z) sum(extraDistr::dnhyper(x, N-m, m, z, log=TRUE)), 0.0)
    if (N == m) next # extraDistr's zero-black boundary is defective
    d <- diff(ll)
    stopifnot(all(diff(d) < 1e-9))
    checks <- checks + length(d)
  }
}
for (m in c(3L, 10L, 40L)) for (r in seq_len(min(m, 5L))) {
  x <- c(r, r+1L, r+7L)
  low <- m + max(x) - r
  for (N in seq.int(low+1L, max(low+1L, 3L*m*max(x)))) {
    a <- sum(extraDistr::dnhyper(x, N-m, m, r, log=TRUE))
    b <- sum(extraDistr::dnhyper(x, N-m-1L, m, r, log=TRUE))
    ratio <- sum(log1p((m*x-N*r)/(N*(N-m-x+r))))
    stopifnot(abs(a-b-ratio) < 1e-8)
    if (N > m*max(x)/r) stopifnot(ratio < 0)
    checks <- checks + 1L
  }
}
cat("reference ratio and symmetry checks:", checks, "\n")
# Large-population limits, independently evaluated on the reference densities.
for (N in c(10000, 100000, 1000000)) {
  m <- N / 2
  h <- max(abs(dhyper(0:8, m, N-m, 8) - dbinom(0:8, 8, .5)))
  nh <- max(abs(extraDistr::dnhyper(3:30, N-m, m, 3) - dnbinom(0:27, 3, .5)))
  stopifnot(h < 4/N, nh < 4/N)
  cat("limit N =", N, "hyper error =", signif(h, 5), "nhyper error =", signif(nh, 5), "\n")
}
