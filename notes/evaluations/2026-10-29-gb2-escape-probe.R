suppressMessages(pkgload::load_all("univariateML/package", compile = FALSE, quiet = TRUE))
set.seed(2026)
rgb2 <- function(n, a, b, p, q) b * (rgamma(n, p) / rgamma(n, q))^(1 / a)
gens <- list(
  "GB2(2,1,.4,.3)" = function(n) rgb2(n, 2, 1, .4, .3),
  "GB2(3,1,2,1.5)" = function(n) rgb2(n, 3, 1, 2, 1.5),
  "Burr(q=2,a=3)"  = function(n) rgb2(n, 3, 1, 1, 2),
  "lognormal"      = function(n) rlnorm(n, 0, .5),
  "gamma(2)"       = function(n) rgamma(n, 2),
  "Weibull(1.5)"   = function(n) rweibull(n, 1.5),
  "Pareto-ish (lomax)" = function(n) extraDistr::rlomax(n, 1, 3))
for (n in c(50, 500)) for (g in names(gens)) {
  out <- character(); t0 <- proc.time()[3]
  for (r in 1:20) {
    x <- gens[[g]](n)
    m <- tryCatch({ mlgb2(x); "interior" }, error = function(e) {
      msg <- conditionMessage(e)
      hit <- regmatches(msg, regexpr("use `ml[a-z0-9]+`|double Pareto|unresolved|[Uu]nresolved", msg))
      if (length(hit)) hit else substr(msg, 1, 40) })
    out <- c(out, m)
  }
  tab <- sort(table(out), decreasing = TRUE)
  cat(sprintf("n=%-4d %-20s %s  (%.1fs)\n", n, g, paste(names(tab), tab, sep = ":", collapse = "  "), proc.time()[3] - t0))
}
