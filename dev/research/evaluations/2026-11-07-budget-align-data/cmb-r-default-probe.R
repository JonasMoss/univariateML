options(pkg.build_extra_flags=FALSE)
pkgload::load_all("univariateML-r",quiet=TRUE)
set.seed(1142026)
samples <- list(hand=c(0,0,1,1,2,2,2,3,3,4),seeded=rcmb(100,4,.4,.5))
writeLines(paste("seeded counts:",paste(samples$seeded,collapse=",")))
for (label in names(samples)) for (nu in c(-1,0,.5)) {
  for (I in c(1000L,2000L,5000L)) {
    t <- system.time(f <- tryCatch(mlcmb(samples[[label]],size=NULL,nu=nu,iterlim=I),error=identity))
    if(inherits(f,"error"))cat(label,nu,I,class(f)[1],conditionMessage(f),t[["elapsed"]],"\n",sep="\t")
    else cat(label,nu,I,"ok",f[["size"]],attr(f,"logLik"),t[["elapsed"]],"\n",sep="\t")
    if(!inherits(f,"error"))break
  }
}
