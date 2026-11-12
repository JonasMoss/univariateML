# Run from repository root after just regen; no installation or fixture changes.
options(pkg.build_extra_flags=FALSE)
pkgload::load_all("univariateML-r",quiet=TRUE,compile=FALSE)
root <- "univariateML-cpp/include/cppunivariate/distributions"
out <- "dev/research/evaluations/2026-11-12-stats-route-data"
ids <- c("beta","gamma","hyper","lnorm","cauchy","logis","unif","exp","binom","pois","weibull","geom","nbinom","norm")
changes <- list(); summaries <- list(); conventions <- list(); seams <- list()
relative <- function(a,b) {
  z <- rep(Inf,length(a)); same <- !is.na(a)&!is.na(b)&a==b
  z[same] <- 0; z[is.na(a)&is.na(b)] <- 0
  ok <- is.finite(a)&is.finite(b);z[ok] <- abs(a[ok]-b[ok])/ifelse(b[ok]==0,1,abs(b[ok]));z
}
for(id in ids) {
  g <- jsonlite::fromJSON(file.path(root,id,"golden.json"))
  meta <- univariateML_metadata[[paste0("ml",id)]]
  points <- list(golden=as.list(g$estimates),simulate=setNames(as.list(meta$simulate),meta$names))
  # An ordinary independent grid varies each coefficient across its natural units.
  for(j in seq_along(meta$params)) {
    par <- meta$params[[j]]; name <- par$name
    vals <- if(name %in% c("prob")) c(.01,.2,.5,.8,.99) else
      if(name %in% c("mean","meanlog","location")) c(-2,0,2) else
      if(name %in% c("size","m","N","k")) numeric() else c(.1,.5,1,2,10,100)
    for(v in vals) {theta<-points$simulate;theta[[name]]<-v;points[[paste(name,v)]]<-theta}
  }
  for(point in names(points)) {
    theta <- lapply(points[[point]],as.numeric)
    if(id=="unif" && theta$min>=theta$max) next
    ref <- theta;if(id=="hyper")ref<-list(m=theta$m,n=theta$N-theta$m,k=theta$k)
    fit <- univariateML_construct(unlist(theta),paste0("ml",id),list(logLik=0,n=1,call=NULL))
    for(kind in c("d","p","q")) {
      fun<-getFromNamespace(paste0("core_",kind,id),"univariateML")
      reference<-get(paste0(kind,id),asNamespace("stats"))
      # Quantile-derived locations also cover each law's ordinary bulk.
      at<-if(kind=="q")c(0,seq(.001,.999,length.out=101),1) else
        unique(c(-Inf,-2,-1,0,.5,1,2,10,Inf,suppressWarnings(do.call(get(paste0("q",id),asNamespace("stats")),c(list(seq(.001,.999,length.out=101)),ref)))))
      worst<-0; misses<-0
      for(lower in c(TRUE,FALSE))for(logged in c(TRUE,FALSE)) {
        flags<-if(kind=="d")list(log=logged) else list(lower.tail=lower,log.p=logged)
        input<-if(kind=="q" && logged)log(at) else at
        a<-suppressWarnings(do.call(fun,c(list(input),theta,flags)))
        b<-suppressWarnings(do.call(reference,c(list(input),ref,flags)))
        err<-relative(a,b);worst<-max(worst,err); hit<-which(err>1e-12); misses<-misses+length(hit)
        if(length(hit))changes[[length(changes)+1]]<-data.frame(id,point,parameters=paste(names(theta),unlist(theta),sep="=",collapse=";"),kind,lower,logged,at=input[hit],input_hex=sprintf("%a",input[hit]),core=a[hit],core_hex=sprintf("%a",a[hit]),stats=b[hit],stats_hex=sprintf("%a",b[hit]),relative=err[hit])
      }
      summaries[[length(summaries)+1]]<-data.frame(id,point,kind,worst,misses)
    }
  }
  theta<-points$golden; ref<-theta;if(id=="hyper")ref<-list(m=theta$m,n=theta$N-theta$m,k=theta$k)
  fit<-do.call(get(paste0("ml",id)),c(list(g$data),lapply(g$fit_args,function(x)if(is.null(x))NULL else as.numeric(x))))
  ll<-sum(dml(g$data,fit,log=TRUE));seams[[id]]<-data.frame(id,core=ll,fit=as.numeric(logLik(fit)),relative=relative(ll,as.numeric(logLik(fit))))
  for(kind in c("d","p","q")) {
    fun<-getFromNamespace(paste0("core_",kind,id),"univariateML");reference<-get(paste0(kind,id),asNamespace("stats"))
    cases<-list(missing=c(NA_real_,NaN),edges=c(-Inf,Inf),near_integer=c(1-1e-8,1+1e-8),near_integer_tiny=c(1-1e-12,1+1e-12),fractional=1.5,outside_fractional=c(-1.5,1000000.5))
    for(case in names(cases)) {
      capture<-function(f,a) {warnings<-character();v<-withCallingHandlers(do.call(f,a),warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart("muffleWarning")});list(value=v,warnings=warnings)}
      a<-capture(fun,c(list(cases[[case]]),theta));b<-capture(reference,c(list(cases[[case]]),ref))
      conventions[[paste(id,kind,case)]]<-list(core=a,stats=b)
    }
    for(j in seq_along(theta)) for(v in c(NA_real_,NaN,Inf,-Inf,0,-1,
      if(id=="hyper" || id=="binom" && names(theta)[j]=="size") theta[[j]]+c(-1e-12,1e-12))) {
      ta<-theta;tb<-ref;ta[[j]]<-v
      if(id=="hyper")tb<-list(m=ta$m,n=ta$N-ta$m,k=ta$k) else tb<-ta
      a<-capture(fun,c(list(.5),ta));b<-capture(reference,c(list(.5),tb))
      conventions[[paste(id,kind,names(theta)[j],sprintf("%.17g",v))]]<-list(core=a,stats=b)
    }
  }
}
write.csv(do.call(rbind,summaries),file.path(out,"ordinary-summary.csv"),row.names=FALSE)
write.csv(if(length(changes))do.call(rbind,changes) else data.frame(),file.path(out,"ordinary-changes.csv"),row.names=FALSE)
write.csv(do.call(rbind,seams),file.path(out,"golden-seams.csv"),row.names=FALSE)
jsonlite::write_json(conventions,file.path(out,"conventions.json"),pretty=TRUE,na="string",digits=17)
print(aggregate(worst~id+kind,do.call(rbind,summaries),max))

# Inspect exact and adjacent median probabilities independently of the grid's
# decimal formatting. For Hyper(5,10,5), F(2)=126/252 exactly.
p <- c(.5-2^-54,.5,.5+2^-53)
boundary <- data.frame(p=sprintf("%.17g",p),
  hyper_core=getFromNamespace("core_qhyper","univariateML")(p,5,10,5),
  hyper_stats=stats::qhyper(p,5,5,5),
  hyper_exact=ifelse(p<=.5,2,3),
  geom_upper_core=getFromNamespace("core_qgeom","univariateML")(p,.5,lower.tail=FALSE),
  geom_upper_stats=stats::qgeom(p,.5,lower.tail=FALSE),
  geom_upper_exact=ifelse(p<.5,1,0))
write.csv(boundary,file.path(out,"median-boundaries.csv"),row.names=FALSE)
writeLines(c(paste("Baseline HEAD:",system2("git",c("rev-parse","HEAD"),stdout=TRUE)),
             capture.output(sessionInfo())),file.path(out,"session-info.txt"))
archive <- jsonlite::fromJSON("dev/research/evaluations/2026-11-11-seam-check-data/r-beta-density.json")
archived_fit <- univariateML_construct(setNames(archive$parameters,c("shape1","shape2")),
  "mlbeta",list(logLik=archive$independent_point,n=length(archive$data),call=NULL))
attr(archived_fit,"density") <- "stats::dbeta"
write.csv(data.frame(core=sum(dml(archive$data,archived_fit,log=TRUE)),
  stats=sum(stats::dbeta(archive$data,archive$parameters[1],archive$parameters[2],log=TRUE)),
  independent=archive$independent_point),file.path(out,"archived-beta-seam.csv"),row.names=FALSE)
