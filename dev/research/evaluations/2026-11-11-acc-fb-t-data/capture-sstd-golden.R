options(pkg.build_extra_flags=FALSE)
pkgload::load_all('univariateML-r',quiet=TRUE)
g <- jsonlite::fromJSON('univariateML-cpp/include/cppunivariate/distributions/sstd/golden.json')
r <- tryCatch({
  f <- suppressWarnings(univariateML::mlsstd(g$data))
  list(index=40001,error='',estimates=as.list(setNames(as.numeric(f),names(f))))
},error=function(e)list(index=40001,error=conditionMessage(e),estimates=list()))
cat(jsonlite::toJSON(r,auto_unbox=TRUE,digits=17,null='null'),'\n',sep='')
