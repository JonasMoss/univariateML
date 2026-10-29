# Rscript notes/evaluations/2026-10-29-p0-locscale/huber-m4.R TYPE N
# Fixed seed per cell, 100 independent replicates; outputs to scratch.
library(univariateML, lib.loc='/tmp/umvl-p0lib')
a <- commandArgs(TRUE); type <- a[1]; n <- as.integer(a[2]); types <- c('normal','laplace','t3','cauchy')
stopifnot(type %in% types, n %in% c(20L,100L,1000L))
set.seed(20261029 + 10000*match(type,types) + n)
random <- switch(type, normal=function(n) rnorm(n), laplace=function(n) extraDistr::rlaplace(n), t3=function(n) rt(n,3), cauchy=function(n) rcauchy(n))
rows <- vector('list',100)
for (i in seq_len(100)) {
 x <- random(n); center <- median(x); unit <- median(abs(x-center)); y <- (x-center)/unit
 nm <- mean(y); ns <- sqrt(mean((y-nm)^2)); lm <- median(y); ls <- mean(abs(y-lm))
 normal <- sum(dnorm(y,nm,ns,log=TRUE)); laplace <- sum(extraDistr::dlaplace(y,lm,ls,log=TRUE))
 fn <- function(p) {z <- suppressWarnings(extraDistr::dhuber(y,mu=p[1],sigma=exp(p[2]+p[3]),epsilon=exp(p[3]),log=TRUE)); if(any(!is.finite(z))) return(1e100); -sum(z)}
 starts <- lapply(c(.08,.3,1,2,5),function(e) c(lm,log(if(e<1) ls else ns/e),log(e)))
 fits <- lapply(starts,function(p) tryCatch(optim(p,fn,method='BFGS',control=list(maxit=500,reltol=1e-11)),error=function(e) NULL))
 vals <- vapply(fits,function(z) if(is.null(z)) -Inf else -z$value,numeric(1)); best <- max(vals)
 face <- if(normal>=laplace) 'normal' else 'laplace'; faceval <- max(normal,laplace)
 # Numerical face decision: close values are indeterminate, not a proof.
 oracle <- if(best>faceval+1e-8*n) 'finite' else face
 fit <- tryCatch(mlhuber(x,epsilon=NULL),error=function(e)e)
 package <- if(inherits(fit,'error')) {msg <- conditionMessage(fit); if(grepl('normal face',msg)) 'normal' else if(grepl('Laplace face',msg)) 'laplace' else paste0('error:',msg)} else 'finite'
 pll <- if(inherits(fit,'error')) NA_real_ else as.numeric(logLik(fit))+n*log(unit)
 rows[[i]] <- data.frame(type,n,rep=i,package,oracle,normal,laplace,best,package_ll=pll,oracle_minus_package=best-pll)
}
out <- do.call(rbind,rows); write.csv(out,sprintf('notes/evaluations/2026-10-29-p0-locscale/huber-%s-%d.csv',type,n),row.names=FALSE)
print(table(package=out$package,oracle=out$oracle)); print(summary(out$oracle_minus_package))
