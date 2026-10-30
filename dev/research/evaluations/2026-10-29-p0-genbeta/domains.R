## Probe the zero-shape boundary of the documented references.
suppressPackageStartupMessages(library(actuar))
suppressPackageStartupMessages(library(extraDistr))
for (family in c('genbeta','nsbeta')) {
 p <- if(family=='genbeta') list(shape1=1,shape2=1,shape3=1,scale=1) else list(shape1=1,shape2=1,min=0,max=1)
 for(k in grep('shape',names(p),value=TRUE)) {
  q <- p; q[[k]] <- 0
  v <- suppressWarnings(sapply(c('d','p','q','r'),function(prefix) {
   f <- get(paste0(prefix,family),asNamespace(if(family=='nsbeta') 'extraDistr' else 'actuar'))
   do.call(f,c(list(if(prefix=='r') 1 else .5),q))
  }))
  cat(family,k,paste(names(v),format(v),sep='=',collapse=' '),'\n')
 }
}
