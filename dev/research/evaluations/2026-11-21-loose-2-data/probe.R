# Run from the workspace root with the selected assembled package installed.
if (!"package:univariateML" %in% search()) library(univariateML)
golden <- function(id) {
  jsonlite::fromJSON(file.path("univariateML-cpp/include/cppunivariate/distributions",
                              id, "golden.json"))
}
row <- function(id, values) cat(paste(c(id, sprintf("%.17g", values)), collapse = ","), "\n", sep = "")
g <- golden("bhatt")
f <- mlbhatt(g$data)
row("bhatt/golden", c(as.numeric(f), attr(f, "logLik")))
row("bhatt/constant/1", as.numeric(mlbhatt(rep(2,5),sigma=1)))
row("bhatt/constant/2", mlbhatt(rep(2,5),sigma=1,a=1)[["mu"]])
x <- c(-10,rep(0,20),10)
row("bhatt/normal/1", mlbhatt(x)[["a"]])
row("bhatt/normal/2", as.numeric(mlbhatt(x,mu=1,sigma=4,a=0)))
row("bhatt/normal/3", as.numeric(mlbhatt(x,mu=0,sigma=4)))
row("bhatt/normal/4", as.numeric(mlbhatt(c(-3,-1,0,1,3),a=0,tol=1e-30)))
x <- c(.03,.1,.3,.7,1,2,5,20)
f <- mlgb2(x,shape1=2,scale=1)
row("gb2/mixed/0", f[c("shape2","shape3")])
f <- mlgb2(x,shape3=3,scale=1)
row("gb2/mixed/1", f[c("shape1","shape2")])
row("sgomp/exponential", as.numeric(mlsgomp(c(0,0,0,1,10))))
row("sgomp/constant", as.numeric(mlsgomp(c(0,0),b=2)))
for (id in c("genbeta", "sged")) {
  fit <- tryCatch(
    if (id == "genbeta") mlgenbeta(golden(id)$data,scale=1,tol=1e-20)
    else mlsged(c(0,1e100),sd=1,nu=1.01,xi=100,tol=10),
    error = identity
  )
  if (inherits(fit, "error")) {
    cat(paste(c(id, class(fit)[1], fit$kind, conditionMessage(fit),
                format(fit$attainable_tol)), collapse = ","), "\n", sep = "")
  } else {
    row(id, as.numeric(fit))
  }
}
