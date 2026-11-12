# Run from repository root, after just regen; no writes outside this worktree.
Sys.setenv(UMVL_TEST_TIER="full")
library(pkgload)
load_all(Sys.getenv("UMVL_AUDIT_PKG","univariateML-r"),quiet=TRUE)
library(testthat)
library(jsonlite)
root<-getwd();dest<-file.path(root,"dev/research/evaluations/2026-11-12-modifiers-2-data")
map<-read.csv("dev/validation/family-reductions.csv",stringsAsFactors=FALSE)
map<-map[map$retirement=="pending",]
meta<-univariateML:::univariateML_metadata
cases<-list();rows<-list()
add<-function(id,source,x,args=list()) {
  cases[[length(cases)+1L]]<<-list(id=id,source=source,x=as.numeric(x),args=args)
}
resolve<-function(v)if(is.character(v)&&length(v)==1&&v %in% c("Inf","-Inf"))as.numeric(v) else v
capture_file<-file.path(dest,"captured-cases.rds")
if(identical(Sys.getenv("UMVL_REPLAY_ONLY"),"1") && file.exists(capture_file)) {
  cases<-readRDS(capture_file)
} else {
capture_results<-list()
for(id in map$family) {
  m<-meta[[paste0("ml",id)]]
  g<-fromJSON(file.path("univariateML-cpp/include/cppunivariate/distributions",id,"golden.json"))
  args<-lapply(g$fit_args,resolve);if(is.null(args))args<-list()
  base<-suppressWarnings(do.call(get(paste0("ml",id)),c(list(x=g$data),args)))
  params<-m$names
  for(mask in 0:(2^length(params)-1L)) {
    a<-setNames(rep(list(NULL),length(params)),params)
    for(j in seq_along(params))if(bitwAnd(mask,bitwShiftL(1L,j-1L)))a[[params[j]]]<-unname(base[[params[j]]])
    add(id,paste0("golden-holds-",mask),g$data,a)
  }
}
archive_text<-paste(readLines(gzfile("dev/research/evaluations/2026-11-07-fit-audit-data/cases.json.gz"),warn=FALSE),collapse="\n")
for(token in c("-Infinity","Infinity","NaN"))archive_text<-gsub(token,paste0('"',sub("Infinity","Inf",token,fixed=TRUE),'"'),archive_text,fixed=TRUE)
archive<-fromJSON(archive_text,simplifyVector=FALSE)
for(c in archive)if(c$family %in% map$family) {
  args<-setNames(rep(list(NULL),length(meta[[paste0("ml",c$family)]]$names)),meta[[paste0("ml",c$family)]]$names)
  for(n in names(c$held))args[n]<-list(resolve(c$held[[n]]))
  add(c$family,paste0("archive-",c$index),unlist(c$data),args)
}
# Replay data/holds from instrumented native test calls. Native-only numerical
# controls are replayed when exposed by the public wrapper. Native-only
# controls remain recorded and covered by the original native assertions.
nativepath<-file.path(dest,"native-cases.jsonl.gz")
if(file.exists(nativepath)) {
  native<-lapply(readLines(gzfile(nativepath)),fromJSON,simplifyVector=FALSE)
  for(i in seq_along(native)) {
    c<-native[[i]];a<-c$args
    args<-setNames(rep(list(NULL),length(meta[[paste0("ml",c$family)]]$names)),meta[[paste0("ml",c$family)]]$names)
    for(n in names(a)) {
      param<-sub("^fixed_","",sub("^held_","",n))
      if(n=="held")param<-"prob"
      if(n=="fixed")param<-if(c$family=="ztpois")"lambda" else "prob"
      if(param %in% names(args) && !identical(a[[n]],"NaN"))args[param]<-list(resolve(a[[n]]))
    }
    controls<-names(formals(get(paste0("ml",c$family,"_"))))
    if("tol" %in% names(a) && "reltol" %in% controls)args$reltol<-resolve(a$tol)
    if("maxit" %in% names(a) && "iterlim" %in% controls)args$iterlim<-resolve(a$maxit)
    data<-unlist(lapply(c$data,function(pair)rep(if(is.null(pair[[1]]))NaN else pair[[1]],pair[[2]])))
    add(c$family,paste0("native-",i),data,args)
  }
}
# Execute each original R test file with wrappers which capture its actual fit
# calls (including calls within loops and expected errors). Existing assertions
# run against the retained old family; no tests are rewritten or suppressed.
for(id in map$family) {
  cat("Capturing R tests:",id,"\n")
  e<-new.env(parent=globalenv());name<-paste0("ml",id);fun<-get(name)
  # Insert an entry recorder while preserving formals and match.call().
  # An extra forwarding frame changes frozen-model call semantics.
  recorded<-fun;environment(recorded)<-new.env(parent=environment(fun))
  environment(recorded)$.retirement_record<-local({i<-id;function(x,na.rm,dots)add(i,"old-R-test",x,c(list(na.rm=na.rm),dots))})
  body(recorded)<-substitute({if(!missing(x)).retirement_record(x,na.rm,list(...));BODY},list(BODY=body(fun)))
  e[[name]]<-recorded
  path<-file.path(root,"univariateML-r/tests/testthat",paste0("test-dist-",id,".R"))
  # Fixture paths in the tests are relative to the generated testthat directory.
  previous<-getwd();setwd(file.path(root,"univariateML-r/tests/testthat"))
  checked<-testthat::test_file(path,env=e,package="univariateML",reporter="silent",load_helpers=TRUE,stop_on_failure=FALSE)
  capture_results[[id]]<-as.data.frame(checked)
  setwd(previous)
}
saveRDS(cases,capture_file,compress="gzip")
write.csv(do.call(rbind,lapply(names(capture_results),function(id) {
  d<-capture_results[[id]];data.frame(family=id,tests=nrow(d),failed=sum(d$failed),errors=sum(d$error),warnings=sum(d$warning))
})),file.path(dest,"capture-tests.csv"),row.names=FALSE)
}
if(identical(Sys.getenv("UMVL_CAPTURE_ONLY"),"1"))quit(status=0)
# Every golden hold subset, with the Cartesian grid of its held
# coordinates at interior values and domain edges. Data include interior, zero-only and one-only samples.
for(id in map$family) {
  m<-meta[[paste0("ml",id)]];params<-m$names
  g<-fromJSON(file.path("univariateML-cpp/include/cppunivariate/distributions",id,"golden.json"))
  fit_args<-lapply(g$fit_args,resolve);if(is.null(fit_args))fit_args<-list()
  baseline<-suppressWarnings(do.call(get(paste0("ml",id)),c(list(x=g$data),fit_args)))
  edges<-list(lambda=c(0,-1,Inf),size=c(0,1,-1,1.5,Inf),prob=c(0,1,-.1,1.1,Inf),p0=c(0,1,-.1,1.1),pi=c(0,1,-.1,1.1),
    N=c(0,1,2,2.5,Inf),s=c(-1,0,1,Inf),mean=c(-Inf,Inf),sd=c(0,-1,Inf),a=c(-Inf,Inf),b=c(-Inf,Inf))
  samples<-if(id=="tnorm")list(g$data,rep(0,4),c(-1,0,1)) else list(g$data,rep(0,4),rep(1,4),c(0,1,2,3),c(1,2,3))
  for(mask in 1:(2^length(params)-1L)) {
    a<-setNames(rep(list(NULL),length(params)),params)
    held<-params[vapply(seq_along(params),function(j)bitwAnd(mask,bitwShiftL(1L,j-1L))!=0,logical(1))]
    options<-stats::setNames(lapply(held,function(p)unique(c(unname(baseline[[p]]),edges[[p]]))),held)
    grid<-do.call(expand.grid,c(options,list(KEEP.OUT.ATTRS=FALSE,stringsAsFactors=FALSE)))
    for(corner in seq_len(nrow(grid)))for(sample in seq_along(samples)) {
      changed<-a
      for(p in held)changed[[p]]<-grid[[p]][corner]
      add(id,paste0("edge-",mask,"-corner",corner,"-data",sample),samples[[sample]],changed)
    }
  }
}
write_json(lapply(cases,function(c){c$n<-length(c$x);if(length(c$x)>10000)c$x<-NULL;c}),file.path(dest,"cases.json"),auto_unbox=TRUE,null="null",digits=17)
run<-function(f,args)tryCatch(suppressWarnings(do.call(f,args)),error=identity)
newargs<-function(c) {
  id<-c$id;i<-match(id,map$family);base<-map$base_family[i];a<-c$args
  # Positional controls in recorded tests use the same public x,na.rm,... API.
  coefficients<-fromJSON(map$base_coefficients[i])
  for(p in names(coefficients))if(p!=coefficients[[p]]) {a[p]<-a[coefficients[[p]]];a[coefficients[[p]]]<-NULL}
  if(startsWith(map$modifier[i],"zero="))a$zero<-sub("zero=","",map$modifier[i],fixed=TRUE)
  else if(id=="zipf") {
    N<-a$N;a$N<-NULL;a$truncate<-c(1,if(is.null(N))NA_real_ else N)
  } else {
    lower<-a$a;upper<-a$b;a$a<-NULL;a$b<-NULL
    a$truncate<-c(if(is.null(lower))NA_real_ else if(id=="tnorm")lower else max(0,floor(lower)+1),if(is.null(upper))NA_real_ else if(id=="tnorm")upper else floor(upper))
  }
  if(base=="binom" && c$source!="old-R-test" && !"size" %in% names(a))a["size"]<-list(NULL)
  list(f=get(paste0("ml",base)),args=c(list(x=c$x),a))
}
for(k in seq_along(cases)) {
  c<-cases[[k]];oldargs<-c$args
  if(c$source!="old-R-test")for(p in meta[[paste0("ml",c$id)]]$required)if(!p %in% names(oldargs))oldargs[p]<-list(NULL)
  old<-run(get(paste0("ml",c$id)),c(list(x=c$x),oldargs))
  n<-newargs(c);new<-run(n$f,n$args)
  oe<-inherits(old,"error");ne<-inherits(new,"error")
  differences<-character()
  if(oe!=ne)differences<-c(differences,"status")
  okind<-if(oe)old$kind else NULL;nkind<-if(ne)new$kind else NULL
  if(!identical(okind,nkind))differences<-c(differences,"stop-kind")
  oc<-if(oe)old$certified else attr(old,"certified");nc<-if(ne)new$certified else attr(new,"certified")
  if(!identical(oc,nc))differences<-c(differences,"certified")
  coefficient_error<-likelihood_error<-NA_real_
  if(!oe && !ne) {
    coefficients<-fromJSON(map$base_coefficients[match(c$id,map$family)])
    old_names<-names(new)
    for(p in intersect(old_names,names(coefficients)))old_names[old_names==p]<-coefficients[[p]]
    wanted<-as.numeric(old[old_names])
    coefficient_error<-max(abs(as.numeric(new)-wanted)/pmax(1,abs(wanted)))
    likelihood_error<-abs(as.numeric(logLik(new))-as.numeric(logLik(old)))
    if(coefficient_error>1e-10)differences<-c(differences,"coefficients")
    if(likelihood_error>1e-10*max(1,abs(as.numeric(logLik(old)))))differences<-c(differences,"logLik")
  }
  rows[[k]]<-list(case=k,family=c$id,source=c$source,differences=paste(differences,collapse=";"),coefficient_error=coefficient_error,logLik_error=likelihood_error,
    old_status=if(oe)"stop" else "Ok",new_status=if(ne)"stop" else "Ok",old_kind=okind,new_kind=nkind,
    old_certified=oc,new_certified=nc,
    old_estimates=if(!oe)stats::setNames(as.list(as.numeric(old)),names(old)) else NULL,
    new_estimates=if(!ne)stats::setNames(as.list(as.numeric(new)),names(new)) else NULL,
    logLik_change=if(!oe && !ne)as.numeric(logLik(new))-as.numeric(logLik(old)) else NA_real_,old_message=if(oe)conditionMessage(old) else "",new_message=if(ne)conditionMessage(new) else "")
  if(k%%100==0)cat(k,"/",length(cases),"\n")
}
write_json(rows,file.path(dest,"results.json"),auto_unbox=TRUE,null="null",digits=17,pretty=TRUE)
write.csv(do.call(rbind,lapply(rows,function(r)data.frame(case=r$case,family=r$family,source=r$source,differences=r$differences))),file.path(dest,"differences.csv"),row.names=FALSE)
cat("Cases",length(cases),"; differing",sum(vapply(rows,function(r)nzchar(r$differences),logical(1))),"\n")
