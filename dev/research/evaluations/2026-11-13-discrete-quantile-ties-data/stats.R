# The same round trip with R's stats quantiles (TASK-215). Rscript stats.R
f <- function(p, q, ks, ...) {
  F <- p(ks, ...); ok <- F > 0 & F < 0.999 & c(TRUE, diff(F) != 0); r <- q(F[ok], ...)
  c(cases = sum(ok), above = sum(r == ks[ok] + 1), below = sum(r < ks[ok]))
}
tot <- 0; for (l in c(.1, .5, 1, 2.5, 7, 20, 55.5, 130)) tot <- tot + f(ppois, qpois, 0:300, lambda = l)
cat("pois  ", tot, "\n")
tot <- 0; for (s in c(1, 2, 5, 10, 31, 100)) for (pr in c(.01, .1, .25, .3, .5, .75, .9)) tot <- tot + f(pbinom, qbinom, 0:(s - 1), size = s, prob = pr)
cat("binom ", tot, "\n")
tot <- 0; for (pr in c(.01, .1, .25, .3, .5, .75, .9)) tot <- tot + f(pgeom, qgeom, 0:300, prob = pr)
cat("geom  ", tot, "\n")
tot <- 0; for (s in c(.5, 1, 2, 5.5, 20)) for (pr in c(.05, .25, .3, .5, .75, .9)) tot <- tot + f(pnbinom, qnbinom, 0:300, size = s, prob = pr)
cat("nbinom", tot, "\n")
# The price of the fuzz: p a few ulp above a jump returns the lower value.
l <- 2.5; k <- 0:20; F <- ppois(k, l)
cat("qpois(F(k) * (1 + 4 eps)) == k in", sum(qpois(F * (1 + 4 * .Machine$double.eps), l) == k), "of", length(k), "\n")
cat(R.version.string, "\n")
