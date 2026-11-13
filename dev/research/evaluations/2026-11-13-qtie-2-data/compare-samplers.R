a<-readRDS('dev/research/evaluations/2026-11-13-qtie-2-data/r-samplers-before.rds');b<-readRDS('dev/research/evaluations/2026-11-13-qtie-2-data/r-samplers-after.rds')
cat('models',length(a),length(b),'\n');cat('draw entries',sum(vapply(a,length,integer(1))),'\n');cat('identical',identical(a,b),'\n')
if(!identical(a,b))for(i in seq_along(a))if(!identical(a[[i]],b[[i]]))cat('model',i,'differences',sum(!is.na(a[[i]]!=b[[i]]) & a[[i]]!=b[[i]]),'\n')
