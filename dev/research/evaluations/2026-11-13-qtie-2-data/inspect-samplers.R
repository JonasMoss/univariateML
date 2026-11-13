a<-readRDS('dev/research/evaluations/2026-11-13-qtie-2-data/r-samplers-before.rds')
cases<-jsonlite::fromJSON('dev/research/evaluations/2026-11-13-qtie-2-data/cases.json',simplifyVector=FALSE)
rows<-read.csv('dev/research/evaluations/2026-11-13-qtie-2-data/r-before.csv');excluded<-rows[is.na(rows$lower),]
keys<-paste(excluded$id,excluded$point)
kept<-Filter(function(c)!paste(c$id,c$point) %in% keys,cases)
stopifnot(length(kept)==length(a))
for(i in seq_along(a))if(is.character(a[[i]]) || any(!is.finite(a[[i]])))cat(jsonlite::toJSON(list(case=kept[[i]],invalid=if(is.character(a[[i]]))a[[i]] else sum(!is.finite(a[[i]]))),auto_unbox=TRUE),'\n')
