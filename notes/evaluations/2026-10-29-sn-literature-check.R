set.seed(7)
ll <- function(par, x, a) { xi <- par[1]; om <- exp(par[2]); z <- (x - xi)/om
  sum(log(2) + dnorm(z, log = TRUE) + pnorm(a * z, log.p = TRUE) - log(om)) }
prof <- function(x, a) {  # max over (xi, log omega) at fixed alpha, several starts
  d <- a / sqrt(1 + a^2); om0 <- sd(x) / sqrt(1 - 2 * d^2 / pi); xi0 <- mean(x) - om0 * d * sqrt(2/pi)
  starts <- list(c(xi0, log(om0)), c(if (a > 0) min(x) else max(x), log(sd(x))), c(mean(x), log(sd(x))))
  max(sapply(starts, function(s) -optim(s, function(p) -ll(p, x, a), control = list(reltol = 1e-12, maxit = 4000))$value)) }
face <- function(x) { # best translated half-normal, both orientations
  f <- function(y) { m <- min(y); s2 <- mean((y - m)^2); length(y) * (log(2) - 0.5 * log(2 * pi * s2) - 0.5) }
  max(f(x), f(-x)) }
rsn5 <- function(n, a) { d <- a/sqrt(1+a^2); d * abs(rnorm(n)) + sqrt(1 - d^2) * rnorm(n) }
grid <- c(-10^(5:1), -3, -1, -0.3, 0, 0.3, 1, 3, 10^(1:5))
res <- data.frame()
for (a in c(5, 10)) for (r in 1:100) {
  x <- rsn5(50, a); pv <- sapply(grid, function(g) prof(x, g)); fs <- face(x)
  exact_inf <- max(pv) <= fs + 1e-8 * 50 && which.max(pv) %in% c(1, length(grid))
  # Emulate a numerical optimizer over (xi, log omega, alpha) from moment-type starts
  o <- optim(c(mean(x) - sd(x), log(sd(x) * 1.3), sign(mean((x - mean(x))^3)) * 2),
             function(p) -ll(p[1:2], x, p[3]), control = list(reltol = 1e-8, maxit = 2000))
  res <- rbind(res, data.frame(a = a, exact_inf = exact_inf, num_big = abs(o$par[3]) > 100,
                               num_alpha = o$par[3], gap_100 = fs - prof(x, sign(o$par[3]) * 100)))
}
for (a in c(5, 10)) { s <- res[res$a == a, ]
  cat(sprintf("alpha=%d n=50: exact face-comparison infinite %.2f | optimizer |alpha|>100: %.2f | median optimizer |alpha| among exact-infinite: %.1f | median loglik gap face - profile(100): %.2e\n",
      a, mean(s$exact_inf), mean(s$num_big), median(abs(s$num_alpha[s$exact_inf])), median(s$gap_100[s$exact_inf]))) }
cat("\n--- profile shapes for exact-infinite samples where the optimizer stopped at |alpha| < 100 (alpha = 5)\n")
set.seed(7); shown <- 0; fine <- c(0.5, 1, 2, 3, 5, 8, 12, 16, 20, 30, 50, 100, 300, 1000, 1e4, 1e5, 1e6)
for (r in 1:100) {
  x <- rsn5(50, 5); s <- res[res$a == 5, ][r, ]
  if (s$exact_inf && abs(s$num_alpha) < 100 && shown < 2) {
    sg <- sign(s$num_alpha); pv <- sapply(sg * fine, function(g) prof(x, g)) - face(x)
    cat(sprintf("sample %d, optimizer alpha %.1f; profile - face at alpha = %s:\n", r, s$num_alpha, paste(sg * fine, collapse = ",")))
    print(round(pv, 4)); shown <- shown + 1 }
}
