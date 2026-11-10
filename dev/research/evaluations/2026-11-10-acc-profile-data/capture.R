options(pkg.build_extra_flags=FALSE)
pkgload::load_all(Sys.getenv("UMVL_CAPTURE_PACKAGE","univariateML-r"),quiet=TRUE)
ids <- strsplit("sn skellam sgomp dgamma pareto3 zinb zmnbinom ztnbinom ht emg genbeta"," ")[[1]]
root <- "univariateML-cpp/include/cppunivariate/distributions"
for(id in ids) {
 g <- jsonlite::fromJSON(file.path(root,id,"golden.json"))
 args <- lapply(g$fit_args,function(x) if(is.null(x)) NULL else as.numeric(x))
 f <- tryCatch(do.call(getExportedValue("univariateML",paste0("ml",id)),c(list(as.numeric(g$data)),args)),error=identity)
 r <- if(inherits(f,"error")) list(id=id,estimates=list(),error=conditionMessage(f)) else
   list(id=id,estimates=as.list(setNames(sprintf("%.17g",as.numeric(f)),names(f))),logLik=sprintf("%.17g",as.numeric(attr(f,"logLik"))),error="")
 cat(jsonlite::toJSON(r,auto_unbox=TRUE,digits=NA),"\n",sep="")
}
