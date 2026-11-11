options(pkg.build_extra_flags=FALSE)
pkgload::load_all('univariateML-r',quiet=TRUE)
directory <- 'dev/research/evaluations/2026-11-10-acc-allfree-data'
for(line in readLines(file.path(directory,'sstd-full-tier-cases.jsonl'))) {
 entry <- jsonlite::fromJSON(line)
 if(!as.character(entry$seed) %in% c('5007','replication'))next
 x <- as.numeric(entry$data)
 if(identical(entry$seed,'replication'))x <- rep(x,100)
 fit <- tryCatch(univariateML::mlsstd(x),error=function(e)e)
 result <- list(id=as.character(entry$seed),n=length(x),error='')
 if(inherits(fit,'error'))result$error <- conditionMessage(fit) else
   result$estimates <- as.list(setNames(sprintf('%.17g',as.numeric(fit)),names(fit)))
 cat(jsonlite::toJSON(result,auto_unbox=TRUE,digits=NA),'\n',sep='')
}
