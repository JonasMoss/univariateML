library(pkgload);load_all(Sys.getenv('UMVL_AUDIT_PKG','univariateML-r'),quiet=TRUE);library(jsonlite)
p<-'dev/research/evaluations/2026-11-12-modifiers-2-data'
frozen<-fromJSON(gzfile(file.path(p,'tbinom-default-inputs.json.gz')),simplifyVector=FALSE)
resolve<-function(v)if(is.character(v)&&length(v)==1&&v %in% c('Inf','-Inf','NA','NaN'))switch(v,'Inf'=Inf,'-Inf'=-Inf,'NA'=NA_real_,'NaN'=NaN) else v
rows<-lapply(frozen$rows,function(z) {
 c<-z$input
 x<-if(!is.null(c$histogram))unlist(lapply(c$histogram,function(h)rep(resolve(h[[1]]),h[[2]]))) else as.numeric(unlist(lapply(c$x,resolve)))
 a<-lapply(c$args,resolve)
 f<-tryCatch(suppressWarnings(do.call(mltbinom,c(list(x=x),a))),error=identity);error<-inherits(f,'error')
 list(case=z$case,source=z$source,old_status=if(error)'stop' else 'Ok',old_kind=if(error)f$kind else NULL,
 old_certified=if(error)f$certified else attr(f,'certified'),old_estimates=if(error)NULL else setNames(as.list(as.numeric(f)),names(f)))
})
write_json(rows,file.path(p,'tbinom-default-after.json'),auto_unbox=TRUE,null='null',digits=17)
