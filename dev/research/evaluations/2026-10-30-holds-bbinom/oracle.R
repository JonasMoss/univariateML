# Independent finite-grid exploration, not a certificate of an integer maximum.
# Run from repository root with Rscript. extraDistr is the reference density.
stopifnot(requireNamespace('extraDistr', quietly=TRUE))
set.seed(951)
samples <- list()
for (n in c(20L,100L)) {
 samples[[paste0('over_',n)]] <- extraDistr::rbbinom(n, 12, 1.2, 2)
 samples[[paste0('under_',n)]] <- rbinom(n, 12, .35)
 samples[[paste0('nb_',n)]] <- rnbinom(n, size=2, mu=4)
 samples[[paste0('pois_',n)]] <- rpois(n, 4)
 samples[[paste0('ties_',n)]] <- rep(c(0L,2L,7L), length.out=n)
}
samples$endpoints <- rep(c(0L,7L),10)
samples$constant <- rep(3L,20)
out <- list(); summaries <- list()
for (nm in names(samples)) {
 x <- samples[[nm]]; n <- length(x); mx <- max(x); meanx <- mean(x)
 grids <- sort(unique(c(seq.int(mx, max(mx, 60L)), round(exp(seq(log(max(mx+1,61)), log(2000), length.out=35))))))
 nb <- function(a) sum(dnbinom(x,size=a,mu=meanx,log=TRUE))
 nbopt <- optimize(function(loga) -nb(exp(loga)), c(-9, 12))
 faces <- c(poisson=sum(dpois(x,meanx,log=TRUE)), nb_fixed=nb(2), nb_free=-nbopt$objective)
 for (held in c('none','alpha','beta')) {
  for (N in grids) {
   bm <- sum(dbinom(x,N,if(N) meanx/N else 0,log=TRUE))
   fn <- function(t) {
    ab <- switch(held,none=exp(t),alpha=c(2,exp(t)),beta=c(exp(t),3))
    ll <- sum(extraDistr::dbbinom(x,N,ab[1],ab[2],log=TRUE))
    if (is.finite(ll)) -ll else 1e100
   }
   dim <- if (held=='none') 2L else 1L
   starts <- if(dim==2L) list(c(-2,0),c(0,0),c(2,3),c(5,5)) else lapply(c(-3,0,3,7),identity)
   fits <- lapply(starts, function(z) optim(z,fn,method='L-BFGS-B',lower=rep(-9,dim),upper=rep(9,dim),control=list(maxit=300)))
   vals <- vapply(fits, `[[`, 0., 'value'); best <- which.min(vals)
   out[[length(out)+1L]] <- data.frame(sample=nm,n=n,held=held,N=N,loglik=-vals[best],binomial=bm,convergence=fits[[best]]$convergence,at_shape_bound=any(abs(fits[[best]]$par)>8.99),poisson=faces['poisson'],nb_fixed=faces['nb_fixed'],nb_free=faces['nb_free'],row.names=NULL)
  }
 }
 cat(nm, 'done\n'); flush.console()
}
tab <- do.call(rbind,out)
write.csv(tab,'dev/research/evaluations/2026-10-30-holds-bbinom/oracle.csv',row.names=FALSE)
for (key in unique(paste(tab$sample,tab$held))) {
 z <- tab[paste(tab$sample,tab$held)==key,]; b <- z[which.max(z$loglik),]
 summaries[[length(summaries)+1]] <- data.frame(sample=b$sample,held=b$held,best_N=b$N,best_ll=b$loglik,last_ll=tail(z$loglik,1),poisson=b$poisson,nb_fixed=b$nb_fixed,nb_free=b$nb_free,bound_hits=sum(z$at_shape_bound),nonconverged=sum(z$convergence!=0))
}
write.csv(do.call(rbind,summaries),'dev/research/evaluations/2026-10-30-holds-bbinom/oracle-summary.csv',row.names=FALSE)
