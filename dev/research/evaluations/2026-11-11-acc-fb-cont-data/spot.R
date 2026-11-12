options(pkg.build_extra_flags=FALSE)
pkgload::load_all("univariateML-r",quiet=TRUE)
p <- "dev/research/evaluations/2026-11-11-acc-fb-cont-data"
cases <- jsonlite::fromJSON(file.path(p,"spot-cases.json"),simplifyVector=FALSE)
for(c in cases) {
 args <- c(list(x=as.numeric(unlist(c$data))),c$held,c$control)
 f <- tryCatch(do.call(getExportedValue("univariateML",paste0("ml",c$family)),args),error=identity)
 r <- if(inherits(f,"error")) list(id=c$index,family=c$family,error=conditionMessage(f)) else
  list(id=c$index,family=c$family,estimates=as.list(setNames(sprintf("%.17g",as.numeric(f)),names(f))),logLik=sprintf("%.17g",as.numeric(attr(f,"logLik"))),certified=attr(f,"certified"),error="")
 cat(jsonlite::toJSON(r,auto_unbox=TRUE,digits=NA),"\n",sep="")
}
