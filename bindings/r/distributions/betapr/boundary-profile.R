# Run from the worktree root after `just test` (one core).
suppressPackageStartupMessages(pkgload::load_all(
  "univariateML-r", compile=FALSE, quiet=TRUE))
# Profile each small shape over the other shape and scale with six BFGS starts.
# The 1e6 ceiling here bounds the numerical search; the bound printed alongside
# is uniform over ALL values of the other shape and scale.
set.seed(1); x <- rbetapr(n = 20, shape1 = 2, shape2 = 3, scale = 4)
for (fixed in 1:2) for (epsilon in c(.1,.01,.001,.0001, .00001)) {
 f <- function(p) {
  par <- numeric(3); par[fixed] <- epsilon; par[3-fixed] <- exp(p[1]); par[3] <- exp(p[2])
  if(any(!is.finite(par)) || any(par<=0) || par[3-fixed]>1e6) return(1e100)
  z <- -sum(dbetapr(x = x, shape1 = par[1], shape2 = par[2], scale = par[3], log = TRUE))
  if(is.finite(z)) z else 1e100
 }
 fits <- lapply(c(.01,.1,1,10,100,1e4),function(b) optim(c(log(b),mean(log(x))),f,method='BFGS',control=list(maxit=3000,reltol=1e-13)))
 best <- min(vapply(fits,function(z) z$value,0.))
 cat('fixed shape',fixed,'epsilon',epsilon,'best logLik',-best,'bound', length(x)*log(exp(epsilon*log(epsilon)-epsilon-lgamma(epsilon)))-sum(log(x)), '\n')
}
