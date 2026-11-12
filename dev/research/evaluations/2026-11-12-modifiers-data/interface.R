# Run from the worktree after just regen/test; emits small reviewable examples.
pkgload::load_all("univariateML-r", compile=FALSE, quiet=TRUE)
x<-c(0,0,1,2,3,5)
fits<-list(modified=mlpois(x,zero="modified"),
           held_atom=mlpois(x,zero="modified",p0=.2),
           truncated=mlpois(x[x>0],zero="truncated"),
           window=mlpois(x[x>0],truncate=c(1,5)),
           censored=mlexp(cbind(c(1,2,3,4),c(1,Inf,3,Inf))))
examples<-lapply(fits,function(f)list(estimates=as.list(setNames(as.numeric(f),names(f))),
  logLik=as.numeric(logLik(f)),fixed=attr(f,"fixed"),zero=attr(f,"zero"),
  truncate=attr(f,"truncate"),censored=isTRUE(attr(f,"censored"))))
examples$evaluations<-list(density=dml(0:3,fits$modified),cdf=pml(0:3,fits$modified),
                          quantile=qml(c(.1,.5,.9),fits$modified))
examples$refusals<-lapply(list(function()mlcmp(x,zero="modified"),
  function()mlgamma(x+1,truncate=c(0,Inf)),function()mlpois(x,zero="modified",truncate=c(0,5)),
  function()mlgamma(cbind(c(1,2),c(1,Inf)))),function(f)tryCatch(f(),error=conditionMessage))
examples$selection_skipped<-as.list(attr(model_select(cbind(c(1,2,3,4),c(1,Inf,3,Inf)),models=c("exp","gamma")),"skipped"))
cat(jsonlite::toJSON(examples,auto_unbox=TRUE,pretty=TRUE,digits=16),"\n")
