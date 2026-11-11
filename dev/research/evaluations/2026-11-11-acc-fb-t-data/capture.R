options(pkg.build_extra_flags=FALSE)
pkgload::load_all('univariateML-r',quiet=TRUE)
D <- 'dev/research/evaluations/2026-11-11-acc-fb-t-data'
cases <- jsonlite::fromJSON(file.path(D,'cases.json'),simplifyVector=FALSE)
selected <- jsonlite::fromJSON(file.path(D,'spot-indices.json'))
for(c in cases) if(c$index %in% selected) {
 row <- tryCatch({
  fun <- getExportedValue('univariateML',paste0('ml',c$family))
  # Generated exact-name wrappers expose x/... rather than their controls.
  control <- switch(c$family,std='gradtol',lst='gradtol',sstd='gradtol',
                    sged='gradtol',st='reltol',slash='reltol')
  args <- c(list(x=unlist(c$data)),c$held);args[[control]] <- 1e-10
  f <- suppressWarnings(do.call(fun,args))
  list(index=c$index,error='',estimates=as.list(setNames(as.numeric(f),names(f))),certified=attr(f,'certified'))
 },error=function(e)list(index=c$index,error=conditionMessage(e),estimates=list()))
 cat(jsonlite::toJSON(row,auto_unbox=TRUE,digits=17,null='null'),'\n',sep='')
}
