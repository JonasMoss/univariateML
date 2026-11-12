# Attach absolute likelihood values to the complete list of changed outcomes.
library(pkgload);load_all(Sys.getenv('UMVL_AUDIT_PKG','univariateML-r'),quiet=TRUE);library(jsonlite)
p<-'dev/research/evaluations/2026-11-12-modifiers-2-data'
readj<-function(name)fromJSON(if(file.exists(file.path(p,name)))file.path(p,name) else gzfile(file.path(p,paste0(name,'.gz'))),simplifyVector=FALSE)
cases<-readj('cases.json');rows<-readj('results.json')
map<-read.csv('dev/validation/family-reductions.csv',stringsAsFactors=FALSE);meta<-univariateML:::univariateML_metadata
resolve<-function(v)if(is.character(v)&&length(v)==1&&v %in% c('Inf','-Inf','NA','NaN'))switch(v,'Inf'=Inf,'-Inf'=-Inf,'NA'=NA_real_,'NaN'=NaN) else v
run<-function(f,a)tryCatch(suppressWarnings(do.call(f,a)),error=identity)
output<-list()
for(r in rows)if(nzchar(r$differences)) {
 c<-cases[[r$case]];stopifnot(!is.null(c$x));x<-as.numeric(unlist(lapply(c$x,resolve)));a<-lapply(c$args,resolve);id<-c$id
 olda<-a
 if(c$source!='old-R-test')for(k in meta[[paste0('ml',id)]]$required)if(!k %in% names(olda))olda[k]<-list(NULL)
 old<-run(get(paste0('ml',id)),c(list(x=x),olda));i<-match(id,map$family);base<-map$base_family[i]
 coefficients<-fromJSON(map$base_coefficients[i])
 for(k in names(coefficients))if(k!=coefficients[[k]]) {a[k]<-a[coefficients[[k]]];a[coefficients[[k]]]<-NULL}
 if(startsWith(map$modifier[i],'zero='))a$zero<-sub('zero=','',map$modifier[i],fixed=TRUE)
 else if(id=='zipf') {N<-a$N;a$N<-NULL;a$truncate<-c(1,if(is.null(N))NA_real_ else N)}
 else {lower<-a$a;upper<-a$b;a$a<-NULL;a$b<-NULL;a$truncate<-c(if(is.null(lower))NA_real_ else if(id=='tnorm')lower else max(0,floor(lower)+1),if(is.null(upper))NA_real_ else if(id=='tnorm')upper else floor(upper))}
 if(base=='binom' && c$source!='old-R-test' && !'size' %in% names(a))a['size']<-list(NULL)
 new<-run(get(paste0('ml',base)),c(list(x=x),a))
 stopifnot(identical(inherits(old,'error'),r$old_status=='stop'),identical(inherits(new,'error'),r$new_status=='stop'))
 output[[length(output)+1L]]<-data.frame(case=r$case,family=id,old_logLik=if(inherits(old,'error'))NA_real_ else as.numeric(logLik(old)),new_logLik=if(inherits(new,'error'))NA_real_ else as.numeric(logLik(new)))
}
write.csv(do.call(rbind,output),file.path(p,'changed-logLik.csv'),row.names=FALSE)
cat(length(output),'absolute changed-case likelihood pairs recorded\n')
