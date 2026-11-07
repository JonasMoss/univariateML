pkgload::load_all("univariateML-r", recompile=FALSE, quiet=TRUE)
set.seed(8104)
samples <- list(rbetapr(n=20,shape1=.7,shape2=1.2,scale=1),
  rbetapr(n=100,shape1=.7,shape2=1.2,scale=1),c(.05,.05,.3,2,15),c(.2,4),1,rep(2,4),
  rgamma(20,2),rinvgamma(n=100,alpha=3,beta=1))
records <- list()
for(i in seq_along(samples))for(args in list(list(shape1=.7),list(shape2=1.2))) {
  x <- samples[[i]]
  r <- tryCatch(do.call(mlbetapr,c(list(x),args)),error=identity)
  records[[length(records)+1]] <- list(sample=i,held=args,x=x,
    result=if(inherits(r,"error")) list(kind=r$kind,candidate_count=nrow(r$candidates),candidates=if(nrow(r$candidates)>6)rbind(head(r$candidates,3),tail(r$candidates,3))else r$candidates,lower=r$loglik_lower,upper=r$loglik_upper) else as.numeric(r))
}
cat(jsonlite::toJSON(records,auto_unbox=TRUE,digits=17,pretty=TRUE))
