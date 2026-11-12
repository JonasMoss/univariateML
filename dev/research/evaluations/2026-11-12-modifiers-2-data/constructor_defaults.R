# Audit omitted-coefficient simulation defaults independently of mapped laws.
pkgload::load_all('univariateML-r',quiet=TRUE)
library(jsonlite)
map<-read.csv('dev/validation/family-reductions.csv',stringsAsFactors=FALSE)
map<-map[map$retirement=='pending',];rows<-list()
for(i in seq_len(nrow(map))) {
 id<-map$family[i];defaults<-suppressWarnings(do.call(get(paste0('ml',id)),list()))
 for(mask in 0:(2^length(defaults)-1L)) {
 selected<-names(defaults)[vapply(seq_along(defaults),function(j)bitwAnd(mask,bitwShiftL(1L,j-1L))!=0,logical(1))]
 old_args<-as.list(defaults[selected]);old<-suppressWarnings(do.call(get(paste0('ml',id)),old_args))
 mapping<-fromJSON(map$base_coefficients[i]);a<-list()
 for(p in names(mapping))if(mapping[[p]] %in% selected)a[p]<-old_args[mapping[[p]]]
 for(p in intersect(selected,c('p0','pi')))a[p]<-old_args[p]
 if(startsWith(map$modifier[i],'zero='))a$zero<-sub('zero=','',map$modifier[i],fixed=TRUE)
 else if(id=='zipf')a$truncate<-c(1,unname(old[['N']]))
 else a$truncate<-c(if(id=='tnorm')unname(old[['a']]) else max(0,floor(old[['a']])+1),if(id=='tnorm')unname(old[['b']]) else floor(old[['b']]))
 new<-suppressWarnings(do.call(get(paste0('ml',map$base_family[i])),a))
 expected<-setNames(vapply(names(new),function(p)unname(old[[if(p %in% names(mapping))mapping[[p]] else p]]),0.),names(new))
 difference<-!identical(as.numeric(new),as.numeric(expected))
 reason<-if(difference)'The base zeta simulation default remains s=2; the old finite Zipf default was s=1. Supply s=1 explicitly to reproduce that data-free default law.' else ''
 if(difference)stopifnot(id=='zipf',identical(unname(new[['s']]),2),identical(unname(old[['s']]),1))
 rows[[length(rows)+1L]]<-list(family=id,subset=mask,explicit_old_arguments=old_args,replacement=map$base_family[i],modifier_arguments=a,old_coefficients=setNames(as.list(as.numeric(old)),names(old)),mapped_old_coefficients=as.list(expected),new_default_coefficients=setNames(as.list(as.numeric(new)),names(new)),difference=difference,reason=reason)
 }
}
write_json(list(cases=length(rows),differences=sum(vapply(rows,function(r)r$difference,logical(1))),results=rows),'dev/research/evaluations/2026-11-12-modifiers-2-data/constructor-defaults.json',auto_unbox=TRUE,null='null',pretty=TRUE,digits=17)
cat(length(rows),'constructor default calls;',sum(vapply(rows,function(r)r$difference,logical(1))),'explained default difference\n')
