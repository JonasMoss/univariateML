#!/usr/bin/env Rscript
# Examine integer profile, including the Poisson limit. Fixed seeds unnecessary.
profile <- function(x, top=3000L) {
  mu <- mean(x); s <- max(x); N <- seq.int(max(1L,s), top)
  ll <- vapply(N, function(k) sum(dbinom(x,k,mu/k,log=TRUE)), 0.0)
  list(size=N[which.max(ll)], ll=max(ll), limit=sum(dpois(x,mu,log=TRUE)),
       variance=mean((x-mu)^2), mean=mu)
}
for (x in list(c(0,0,3), c(0,1,2,5), c(0,0,1,4,4), c(1,1,2,2,3), rep(3,5))) {
  p <- profile(x)
  cat(paste(x,collapse=","), paste(names(p),unlist(p),sep="=",collapse=" "),"\n")
}
for (a in 0:6) for (b in a:6) for (c in b:6) for (d in c:6) {
  x <- c(a,b,c,d); if (!mean(x) || mean((x-mean(x))^2) < mean(x)) next
  p <- profile(x, 300L)
  if (p$size < 300 && p$ll > p$limit+1e-7) {
    cat("counterexample to variance rejection:",paste(x,collapse=","),"size",p$size,"finite minus Poisson",p$ll-p$limit,"\n")
    quit(status=0)
  }
}
cat("No counterexample in this finite search.\n")
