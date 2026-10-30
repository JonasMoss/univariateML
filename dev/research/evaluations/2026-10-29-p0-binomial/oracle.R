#!/usr/bin/env Rscript
# Fixed-design numerical cross-check. Run from the repository root.
# Results are diagnostic, not a certificate of existence or global optimality.
suppressPackageStartupMessages(library(univariateML, lib.loc = "/tmp/umvl-p0lib"))
set.seed(20261029)
ids <- c("binom", "bbinom", "cmb", "tbinom", "zib", "zmbinom", "ztbinom", "hyper", "nhyper")
size <- c(binom=10, bbinom=10, cmb=10, tbinom=12, zib=8, zmbinom=8, ztbinom=8)
dens <- function(id, x, N, v) switch(id,
  binom = dbinom(x, N, v[1], log=TRUE),
  bbinom = extraDistr::dbbinom(x, N, v[1], v[2], log=TRUE),
  cmb = { if (v[1] %in% c(0,1)) ifelse(x==if(v[1]==0)0 else N,0,-Inf) else { k <- 0:N; z <- v[2]*lchoose(N,k)+k*qlogis(v[1]); mx <- max(z); v[2]*lchoose(N,x)+x*qlogis(v[1])-mx-log(sum(exp(z-mx))) } },
  tbinom = extraDistr::dtbinom(x, N, v[1], 0, 10, log=TRUE),
  zib = extraDistr::dzib(x, N, v[1], v[2], log=TRUE),
  zmbinom = actuar::dzmbinom(x, N, v[1], v[2], log=TRUE),
  ztbinom = actuar::dztbinom(x, N, v[1], log=TRUE))
oracle <- function(id, x, N, held=list()) {
  nm <- switch(id, bbinom=c("alpha","beta"), cmb=c("prob","nu"),
               zib=c("prob","pi"), zmbinom=c("prob","p0"), "prob")
  free <- setdiff(nm, names(held))
  unpack <- function(t) {
    v <- numeric(length(nm)); names(v) <- nm; v[names(held)] <- unlist(held)
    for (j in seq_along(free)) v[free[j]] <- if (free[j] %in% c("alpha","beta")) exp(t[j]) else if (free[j] == "nu") t[j] else plogis(t[j])
    v
  }
  ll <- function(t) { z <- suppressWarnings(dens(id,x,N,unpack(t))); if (any(!is.finite(z)) || any(z > 1e-9)) return(-1e100); sum(z) }
  if (!length(free)) return(ll(numeric()))
  starts <- c(-7, -2, 0, 2)
  lower <- ifelse(free=="nu", -30, -14); upper <- ifelse(free=="nu", 30, ifelse(free %in% c("alpha","beta"),12,14))
  fits <- lapply(starts, function(z) optim(rep(z,length(free)), function(t) -ll(t), method="L-BFGS-B", lower=lower, upper=upper, control=list(maxit=500,factr=1e4)))
  best <- max(vapply(fits, function(z) -z$value, 0.0))
  if ("prob" %in% free && N > 0) {
    init <- rep(0,length(free)); init[match("prob",free)] <- qlogis(max(1e-5,min(.99999,mean(x)/N)))
    fit <- optim(init,function(t) -ll(t),method="L-BFGS-B",lower=lower,upper=upper,control=list(maxit=500,factr=1e4))
    best <- max(best,-fit$value)
  }
  if (length(free)==1) {
    g <- seq(lower,upper,length.out=25); scores <- vapply(g,ll,0.0)
    for (i in which(scores>=c(-Inf,head(scores,-1)) & scores>=c(tail(scores,-1),-Inf))) {
      interval <- range(g[max(1,i-1)],g[min(length(g),i+1)])
      if (diff(interval)>0) best<-max(best,-optimize(function(t)-ll(t),interval=interval,tol=1e-9)$objective)
    }
  }
  # Include attained closed faces explicitly; recurse only on remaining free coordinates.
  for (name in intersect(free, c("prob","pi","p0")))
    for (edge in 0:1) best <- max(best, oracle(id,x,N,c(held,setNames(list(edge),name))))
  best
}
args_for <- function(id) switch(id, tbinom=list(size=12,a=0,b=10),
  hyper=list(N=50,k=10), nhyper=list(N=50,r=3), list(size=unname(size[id])))
samples <- function(id) {
  a <- lapply(c(20,100,1000,2,3,5), function(n) simulate_default(paste0("ml",id),n))
  names(a) <- paste0("default_",c(20,100,1000,2,3,5))
  N <- if(id %in% names(size)) unname(size[id]) else if(id=="hyper") 10 else 10
  a$tied <- if(id=="nhyper") c(3,3,4,4,5,5) else if(id %in% c("tbinom","ztbinom")) c(1,1,2,2,3,3) else c(0,0,1,1,2,2)
  a$boundary <- if(id=="nhyper") rep(3,5) else if(id %in% c("tbinom","ztbinom")) rep(1,5) else rep(0,5)
  a$limit <- if(id=="hyper") rbinom(100,10,.4) else if(id=="nhyper") 3+rnbinom(100,size=3,prob=.4) else if(id=="bbinom") rbinom(100,N,.4) else if(id=="cmb") rep(c(2,3),50) else if(id=="tbinom") extraDistr::rtpois(100,3,0,10) else if(id=="zib") extraDistr::rzip(100,3,.4) else if(id=="zmbinom") actuar::rzmpois(100,3,.3) else if(id=="ztbinom") actuar::rztpois(100,3) else rpois(100,3)
  if (id == "bbinom") { a$limit_negbinom <- rnbinom(100,2,prob=.4); a$endpoint_mix <- rep(c(0,10),50) }
  if (id == "cmb") a$endpoint_mix <- rep(c(0,10),50)
  a
}
if (Sys.getenv("P0_FUNCTIONS_ONLY") != "1") {
out <- list(); j <- 0L
for (id in ids) for (sn in names(ss <- samples(id))) {
  cat("Running", id, sn, "\n"); flush.console()
  x <- ss[[sn]]; args <- args_for(id)
  val <- tryCatch({
    if (id == "hyper") max(vapply(0:50, function(m) sum(dhyper(x,m,50-m,10,log=TRUE)), 0.0))
    else if (id == "nhyper") {
      hi <- 50-max(x)+3
      if (hi < 3 || min(x)<3) -Inf else max(vapply(3:hi, function(m) {
        # extraDistr 1.10.0.5 segfaults at n=0; this is its degenerate limit.
        if (m == 50) return(if (all(x == 3)) 0 else -Inf)
        sum(extraDistr::dnhyper(x,50-m,m,3,log=TRUE))
      }, 0.0))
    }
    else oracle(id,x,unname(size[id]))
  }, error=function(e) NA_real_)
  fit <- tryCatch(suppressWarnings(do.call(get(paste0("ml",id)),c(list(x=x),args))),error=function(e)e)
  got <- if (inherits(fit,"error")) NA_real_ else as.numeric(logLik(fit))
  j <- j+1L; out[[j]] <- data.frame(id=id,sample=sn,n=length(x),oracle=val,master=got,delta=got-val,message=if(inherits(fit,"error")) conditionMessage(fit) else "")
}
res <- do.call(rbind,out)
write.table(res, "dev/research/evaluations/2026-10-29-p0-binomial/fixed-design.tsv", sep="\t", row.names=FALSE, quote=TRUE)
print(res, row.names=FALSE)
}
