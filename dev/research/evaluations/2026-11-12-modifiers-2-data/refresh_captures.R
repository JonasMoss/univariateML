# Repair instrumentation only, preserving all existing captured inputs.
library(pkgload);load_all('univariateML-r',quiet=TRUE);library(testthat);library(jsonlite)
Sys.setenv(UMVL_TEST_TIER='full')
root<-getwd();dest<-file.path(root,'dev/research/evaluations/2026-11-12-modifiers-2-data')
cases<-readRDS(file.path(dest,'captured-cases.rds'))
if(Sys.getenv('UMVL_REFRESH_PART')=='r') {
 fresh<-list();fun<-mlzib;recorded<-fun;environment(recorded)<-new.env(parent=environment(fun))
 environment(recorded)$.retirement_record<-function(x,na.rm,dots)fresh[[length(fresh)+1L]]<<-list(id='zib',source='old-R-test',x=as.numeric(x),args=c(list(na.rm=na.rm),dots))
 body(recorded)<-substitute({if(!missing(x)).retirement_record(x,na.rm,list(...));BODY},list(BODY=body(fun)))
 e<-new.env(parent=globalenv());e$mlzib<-recorded
 checked<-testthat::test_file(file.path(root,'univariateML-r/tests/testthat/test-dist-zib.R'),env=e,package='univariateML',reporter='silent',load_helpers=TRUE,stop_on_failure=FALSE)
 d<-as.data.frame(checked);stopifnot(sum(d$failed)==0,sum(d$error)==0)
 selected<-which(vapply(cases,function(c)c$id=='zib'&&c$source=='old-R-test',logical(1)))
 where<-min(selected);keep<-cases[-selected];cases<-append(keep,fresh,after=where-1L)
 counts<-read.csv(file.path(dest,'capture-tests.csv'));i<-match('zib',counts$family);counts[i,2:5]<-list(nrow(d),sum(d$failed),sum(d$error),sum(d$warning));write.csv(counts,file.path(dest,'capture-tests.csv'),row.names=FALSE)
 cat('ZIB:',nrow(d),'tests;',length(fresh),'fit calls; all assertions passed\n')
} else {
 native<-lapply(readLines(gzfile(file.path(dest,'native-cases.jsonl.gz'))),fromJSON,simplifyVector=FALSE)
 ix<-which(vapply(cases,function(c)startsWith(c$source,'native-'),logical(1)));stopifnot(length(ix)==length(native))
 meta<-univariateML:::univariateML_metadata
 resolve<-function(v)if(is.character(v)&&length(v)==1&&v %in% c('Inf','-Inf'))as.numeric(v) else v
 for(i in seq_along(native)) {
  c<-native[[i]];a<-c$args;args<-setNames(rep(list(NULL),length(meta[[paste0('ml',c$family)]]$names)),meta[[paste0('ml',c$family)]]$names)
  for(n in names(a)) {
   param<-sub('^fixed_','',sub('^held_','',n));if(n=='held')param<-'prob';if(n=='fixed')param<-if(c$family=='ztpois')'lambda' else 'prob'
   if(param %in% names(args)&&!identical(a[[n]],'NaN'))args[param]<-list(resolve(a[[n]]))
  }
  controls<-names(formals(get(paste0('ml',c$family,'_'))))
  if('tol' %in% names(a)&&'reltol' %in% controls)args$reltol<-resolve(a$tol)
  if('maxit' %in% names(a)&&'iterlim' %in% controls)args$iterlim<-resolve(a$maxit)
  x<-as.numeric(unlist(lapply(c$data,function(pair)rep(if(is.null(pair[[1]]))NaN else pair[[1]],pair[[2]]))))
  stopifnot(cases[[ix[i]]]$id==c$family)
  cases[[ix[i]]]<-list(id=c$family,source=paste0('native-',i),x=x,args=args)
 }
 cat(length(native),'native captures refreshed without losing nonfinite observations\n')
}
saveRDS(cases,file.path(dest,'captured-cases.rds'),compress='gzip')
