options(pkg.build_extra_flags=FALSE)
pkgload::load_all("univariateML-r",quiet=TRUE)
cases <- jsonlite::fromJSON("dev/research/evaluations/2026-11-10-acc-newton-data/held-cases.json",simplifyVector=FALSE)
for(c in cases) {
  unit <- if(identical(c$unit,"denorm")) .Machine$double.xmin*.Machine$double.eps else 1
  x <- as.numeric(unlist(c$data))*unit
  held <- lapply(c$held,as.numeric)
  for(name in unlist(c$unit_params)) held[[name]] <- held[[name]]*unit
  r <- tryCatch(do.call(getExportedValue("univariateML",paste0("ml",c$family)),c(list(x),held)),error=identity)
  if(inherits(r,"error")) out <- list(id=c$id,estimates=list(),error=conditionMessage(r),kind=if(inherits(r,"univariateML_numerical")) "numerical" else "error")
  else out <- list(id=c$id,estimates=as.list(setNames(sprintf("%.17g",as.numeric(r)),names(r))),error="",kind="")
  cat(jsonlite::toJSON(out,auto_unbox=TRUE,digits=NA),"\n",sep="")
}
