# Run from repository root after just regen; all searches below are finite
# reference searches, not proofs of behavior beyond N=200.
options(pkg.build_extra_flags=FALSE)
pkgload::load_all('univariateML-r', quiet=TRUE)
nh <- getFromNamespace('dnhyper','univariateML')
set.seed(12103)
hx <- rhyper(150,500,500,5)
nx <- rnhyper(150,500,1000,2)
scan_h <- function(x) max(vapply(5:200, function(N)
  max(vapply(0:N, function(m) sum(dhyper(x,m,N-m,5,log=TRUE)), 0.0)), 0.0))
scan_n <- function(x, free) max(vapply(max(x):200, function(N)
  max(vapply(if(free) 1:min(x) else 2, function(r)
    max(vapply(r:(N-max(x)+r),function(m) sum(nh(x,m,N,r,log=TRUE)),0.0)),0.0)),0.0))
record <- function(label, fun, brute) {
  fit <- tryCatch(fun(), error=identity)
  if (inherits(fit,'error')) {
    msg <- conditionMessage(fit)
    stopifnot(grepl('not excluded|numerical failure',msg,ignore.case=TRUE))
    cat(label, 'unresolved:', msg, '; brute best through 200:', brute, '\n')
  } else {
    ll <- as.numeric(logLik(fit))
    stopifnot(ll + 1e-7 >= brute)
    cat(label, 'finite:', ll, '; brute best through 200:', brute, '\n')
  }
}
record('hyper held k',function() mlhyper(hx,k=5,N=NULL,m=NULL),scan_h(hx))
record('hyper held m',function() mlhyper(hx,m=5,N=NULL,k=NULL),scan_h(hx))
record('nhyper held r',function() mlnhyper(nx,r=2,N=NULL,m=NULL),scan_n(nx,FALSE))
record('nhyper free',function() mlnhyper(nx,r=NULL,N=NULL,m=NULL),scan_n(nx,TRUE))
