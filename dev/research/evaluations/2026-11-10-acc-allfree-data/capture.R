options(pkg.build_extra_flags=FALSE)
pkgload::load_all("univariateML-r",quiet=TRUE)
root <- "univariateML-cpp/include/cppunivariate/distributions"
for(id in c("slash","sstd","sged","st","bnbinom","gb2")) {
 g <- jsonlite::fromJSON(file.path(root,id,"golden.json"))
 f <- do.call(getExportedValue("univariateML",paste0("ml",id)),list(as.numeric(g$data)))
 cat(jsonlite::toJSON(list(id=id,estimates=as.list(setNames(sprintf("%.17g",as.numeric(f)),names(f))),logLik=sprintf("%.17g",as.numeric(attr(f,"logLik"))),error=""),auto_unbox=TRUE,digits=NA),"\n",sep="")
}
