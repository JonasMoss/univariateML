# Run after just test: /tmp/umvl-cpu/run Rscript .../genbeta/escape-probe.R
pkgload::load_all("univariateML/package",compile=FALSE,quiet=TRUE)
set.seed(20261029)
generators<-list(beta=function(n)rbeta(n,.4,.3),
  Kumaraswamy=function(n)rkumar(n = n, a = 2, b = 0.3),
  power=function(n)runif(n)^2,
  unit_gamma=function(n)exp(-rgamma(n,2,rate=3)),
  GB1=function(n)rgenbeta(n = n, shape1 = 0.4, shape2 = 0.3, shape3 = 2, scale = 1/1))
for(name in names(generators))for(n in c(50,500)) {
  outcomes<-replicate(20,{
    x<-generators[[name]](n)
    tryCatch({mlgenbeta(x,scale=1);"interior"},error=function(e) {
      msg<-conditionMessage(e)
      if(grepl("mllgamma",msg))"unit-gamma" else if(grepl("mlpower",msg))"power" else paste("ERROR",msg)
    })
  })
  cat(name,n,":",paste(names(table(outcomes)),table(outcomes),collapse=", "),"\n")
}
