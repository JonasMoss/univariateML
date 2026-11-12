# Data-free constructors: original R test calls, explicit-default subsets, and
# Cartesian held-domain edges. Map the constructed coefficients to the new law.
library(pkgload);load_all(Sys.getenv('UMVL_AUDIT_PKG','univariateML-r'),quiet=TRUE)
library(testthat);library(jsonlite);Sys.setenv(UMVL_TEST_TIER='full')
root<-getwd();pkg<-normalizePath(Sys.getenv('UMVL_AUDIT_PKG','univariateML-r'))
dest<-file.path(root,'dev/research/evaluations/2026-11-12-modifiers-2-data')
map<-read.csv('dev/validation/family-reductions.csv',stringsAsFactors=FALSE);map<-map[map$retirement=='pending',]
meta<-univariateML:::univariateML_metadata;frozen_cases<-list();captured_tests<-list()
add<-function(id,source,args)frozen_cases[[length(frozen_cases)+1L]]<<-list(id=id,source=source,args=args)
for(id in map$family) {
 cat('Frozen capture:',id,'\n');name<-paste0('ml',id);f<-get(name);recorded<-f
 environment(recorded)<-new.env(parent=environment(f))
 environment(recorded)$.record_frozen<-local({i<-id;function(na.rm,dots)add(i,'old-R-test',c(list(na.rm=na.rm),dots))})
 body(recorded)<-substitute({if(missing(x)).record_frozen(na.rm,list(...));BODY},list(BODY=body(f)))
 e<-new.env(parent=globalenv());e[[name]]<-recorded
 checked<-testthat::test_file(file.path(pkg,'tests/testthat',paste0('test-dist-',id,'.R')),env=e,package='univariateML',reporter='silent',load_helpers=TRUE,stop_on_failure=FALSE)
 d<-as.data.frame(checked);captured_tests[[id]]<-data.frame(family=id,tests=nrow(d),failed=sum(d$failed),errors=sum(d$error),warnings=sum(d$warning))
 stopifnot(sum(d$failed)==0,sum(d$error)==0)
}
edges<-list(lambda=c(0,-1,Inf),size=c(0,1,-1,1.5,Inf),prob=c(0,1,-.1,1.1,Inf),p0=c(0,1,-.1,1.1),pi=c(0,1,-.1,1.1),N=c(0,1,2,2.5,Inf),s=c(-1,0,1,Inf),mean=c(-Inf,Inf),sd=c(0,-1,Inf),a=c(-Inf,Inf),b=c(-Inf,Inf))
for(id in map$family) {
 m<-meta[[paste0('ml',id)]];defaults<-setNames(as.list(m$simulate),m$names)
 for(mask in 0:(2^length(m$names)-1L)) {
  selected<-m$names[vapply(seq_along(m$names),function(j)bitwAnd(mask,bitwShiftL(1L,j-1L))!=0,logical(1))]
  add(id,paste0('explicit-defaults-',mask),defaults[selected])
 }
 options<-setNames(lapply(m$names,function(p)unique(c(defaults[[p]],edges[[p]]))),m$names)
 grid<-do.call(expand.grid,c(options,list(KEEP.OUT.ATTRS=FALSE,stringsAsFactors=FALSE)))
 for(j in seq_len(nrow(grid)))add(id,paste0('edge-',j),as.list(grid[j,,drop=FALSE]))
}
run<-function(f,a)tryCatch(suppressWarnings(do.call(f,a)),error=identity)
match_values<-function(a,b) {
 if(inherits(a,'error')||inherits(b,'error'))return(inherits(a,'error')&&inherits(b,'error'))
 if(length(a)!=length(b)||!identical(is.na(a),is.na(b)))return(FALSE)
 finite<-is.finite(a)&is.finite(b)
 if(!identical(is.finite(a),is.finite(b)))return(FALSE)
 if(any(!is.na(a)&!finite&a!=b))return(FALSE)
 all(abs(a[finite]-b[finite])<=1e-10*pmax(1,abs(a[finite]),abs(b[finite])))
}
rows<-list();unknown<-list()
for(k in seq_along(frozen_cases)) {
 c<-frozen_cases[[k]];id<-c$id;i<-match(id,map$family);m<-meta[[paste0('ml',id)]]
 old<-run(get(paste0('ml',id)),c$args);oe<-inherits(old,'error')
 vals<-setNames(as.list(m$simulate),m$names)
 for(p in intersect(names(c$args),m$names))vals[p]<-c$args[p]
 if(!oe)for(p in names(old))vals[p]<-list(unname(old[[p]]))
 a<-c$args[setdiff(names(c$args),m$names)];mapping<-fromJSON(map$base_coefficients[i])
 for(p in names(mapping))a[p]<-vals[mapping[[p]]]
 if(startsWith(map$modifier[i],'zero=')) {
  a$zero<-sub('zero=','',map$modifier[i],fixed=TRUE)
  for(p in intersect(c('p0','pi'),m$names))a[p]<-vals[p]
 } else if(id=='zipf')a$truncate<-c(1,if(is.null(vals$N))NA_real_ else vals$N)
 else a$truncate<-c(if(is.null(vals$a))NA_real_ else if(id=='tnorm')vals$a else max(0,floor(vals$a)+1),if(is.null(vals$b))NA_real_ else if(id=='tnorm')vals$b else floor(vals$b))
 new<-run(get(paste0('ml',map$base_family[i])),a);ne<-inherits(new,'error')
 differences<-character();reason<-''
 if(oe!=ne)differences<-c(differences,'status')
 old_kind<-if(oe)old$kind else NULL;new_kind<-if(ne)new$kind else NULL
 if(!identical(old_kind,new_kind))differences<-c(differences,'stop-kind')
 oc<-if(oe)old$certified else attr(old,'certified');nc<-if(ne)new$certified else attr(new,'certified')
 if(!identical(oc,nc))differences<-c(differences,'certified')
 if(!oe&&!ne) {
  wanted<-setNames(vapply(names(new),function(p)unname(old[[if(p %in% names(mapping))mapping[[p]] else p]]),0.),names(new))
  if(!match_values(as.numeric(new),as.numeric(wanted)))differences<-c(differences,'coefficients')
  points<-unique(c(-Inf,0,1,2,as.numeric(suppressWarnings(qml(c(.1,.5,.9),old))),Inf))
  probes<-list(d=function(o)dml(points,o,log=TRUE),p=function(o)pml(points,o,lower.tail=FALSE,log.p=TRUE),q=function(o)qml(c(.01,.5,.99),o))
  for(type in names(probes)) {
   ov<-tryCatch(suppressWarnings(probes[[type]](old)),error=identity);nv<-tryCatch(suppressWarnings(probes[[type]](new)),error=identity)
   if(!match_values(ov,nv))differences<-c(differences,paste0('evaluation-',type))
  }
 }
 if(length(differences)) {
  if(oe&&!ne&&id=='zipf'&&is.numeric(vals$s)&&vals$s<0)reason<-'finite-window exponent domain includes negative s'
  else if(oe&&!ne&&id=='zipf'&&is.numeric(vals$N)&&is.finite(vals$N)&&vals$N!=floor(vals$N))reason<-'closed integer window floors its upper endpoint'
  else if(oe&&!ne&&id %in% c('tpois','tbinom','tnorm')&&any(vapply(vals[c('a','b')],function(v)is.numeric(v)&&length(v)==1&&is.infinite(v),logical(1))))reason<-'proper unbounded window; old fixed-model metadata required finite design bounds'
  else if(oe&&!ne&&id %in% c('tpois','tbinom')&&is.numeric(vals$a)&&vals$a==0&&identical(vals$b,Inf))reason<-'positive full window uses the attained zero-truncation extension'
  if(!nzchar(reason))unknown[[length(unknown)+1L]]<-k
 }
 rows[[k]]<-list(case=k,family=id,source=c$source,differences=paste(differences,collapse=';'),reason=reason,old_status=if(oe)'stop' else 'Ok',new_status=if(ne)'stop' else 'Ok',old_kind=old_kind,new_kind=new_kind,old_certified=oc,new_certified=nc,old_message=if(oe)conditionMessage(old) else '',new_message=if(ne)conditionMessage(new) else '')
}
write_json(list(cases=frozen_cases,results=rows),file.path(dest,'frozen-audit.json'),auto_unbox=TRUE,null='null',digits=17)
write.csv(do.call(rbind,captured_tests),file.path(dest,'frozen-capture-tests.csv'),row.names=FALSE)
summary<-list(cases=length(rows),differences=sum(vapply(rows,function(r)nzchar(r$differences),logical(1))),unclassified=length(unknown),unclassified_cases=unlist(unknown),old_R_calls=sum(vapply(frozen_cases,function(c)c$source=='old-R-test',logical(1))))
write_json(summary,file.path(dest,'frozen-audit-summary.json'),auto_unbox=TRUE,null='null',pretty=TRUE)
print(summary);stopifnot(length(unknown)==0)
