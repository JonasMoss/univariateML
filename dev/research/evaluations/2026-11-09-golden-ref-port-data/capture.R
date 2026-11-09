options(pkg.build_extra_flags=FALSE)
pkgload::load_all("univariateML-r",quiet=TRUE)
root <- "univariateML-cpp/include/cppunivariate/distributions"
for(p in Sys.glob(file.path(root,"*","golden.json"))) {
 g <- jsonlite::fromJSON(p)
 args <- lapply(g$fit_args,function(x) if(is.null(x)) NULL else as.numeric(x))
 f <- do.call(getExportedValue("univariateML",paste0("ml",g$id)),c(list(as.numeric(g$data)),args))
 cat(jsonlite::toJSON(list(id=g$id,estimates=as.list(setNames(sprintf("%.17g",as.numeric(f)),names(f))),logLik=sprintf("%.17g",as.numeric(attr(f,"logLik"))),error=""),auto_unbox=TRUE,digits=NA),"\n",sep="")
}
g <- jsonlite::fromJSON(file.path(root,"lomax","golden.json"))
x <- rep(g$data,ceiling(4097/length(g$data)))
f <- mllomax(x,reltol=1e-10)
cat(jsonlite::toJSON(list(id="lomax-compressed",lambda_error=sprintf("%.17g",abs(f[["lambda"]]/g$estimates$lambda-1)),kappa_error=sprintf("%.17g",abs(f[["kappa"]]/g$estimates$kappa-1))),auto_unbox=TRUE),"\n",sep="")
