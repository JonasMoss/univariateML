options(pkg.build_extra_flags=FALSE)
pkgload::load_all("univariateML-r",quiet=TRUE)
rows <- jsonlite::fromJSON("dev/research/evaluations/2026-11-10-acc-newton-data/replay-cases.json",simplifyVector=FALSE)
for(row in rows) {
 c <- row$case
 held <- c$held
 if (c$family == "cmb" && !"size" %in% names(held)) held <- c(list(size=NULL),held)
 if (c$family == "fatigue" && !"mu" %in% names(held)) held <- c(list(mu=NULL),held)
 r <- tryCatch({
  f <- do.call(getExportedValue("univariateML",paste0("ml",c$family)),c(list(as.numeric(unlist(c$data))),held))
  list(index=c$index,id=c$family,estimates=as.list(setNames(sprintf("%.17g",as.numeric(f)),names(f))),error="")
 },error=function(e) list(index=c$index,id=c$family,estimates=list(),error=conditionMessage(e)))
 cat(jsonlite::toJSON(r,auto_unbox=TRUE,digits=NA),"\n",sep="")
}
