# Run from repository root, after `just test`, under /tmp/umvl-cpu/run.
# No C++ objective is called by the independent reference oracle.
pkgload::load_all("univariateML/package", compile=FALSE, quiet=TRUE)
helper <- parse("cppunivariate/include/cppunivariate/distributions/bnbinom/test.R")
for (expression in helper) {
  if (is.call(expression) && identical(expression[[1]],as.name("<-")) &&
      as.character(expression[[2]]) %in% c("bnbinom_exact","bnbinom_oracle")) eval(expression)
}
golden <- jsonlite::read_json("cppunivariate/include/cppunivariate/distributions/bnbinom/golden.json",simplifyVector=TRUE)
cat("golden independent oracle logLik:",format(bnbinom_oracle(golden$data),digits=17),"\n")
for (k in c(3,5,10,20)) {
  x <- rep(c(0,k),each=20)
  nb <- mlnbinom(x,reltol=1e-10)
  cat("two-point k:",k,"oracle:",bnbinom_oracle(x),"NB:",attr(nb,"logLik"),"\n")
}
set.seed(20261029)
for (seed in 1:8) {
  set.seed(seed)
  x <- extraDistr::rbnbinom(500,1,2,1)
  fit <- try(mlbnbinom(x),silent=TRUE)
  cat("diagonal experiment R seed:",seed,"\n")
  if (!inherits(fit,"try-error")) {
    print(fit)
    if (abs(log(fit[1]/fit[3]))<1e-4) {
      cat("diagonal independent oracle:",bnbinom_oracle(x),"C++:",attr(fit,"logLik"),"\n")
      break
    }
  }
}
cat("huge-shape reference log masses versus NB limit:\n")
print(extraDistr::dbnbinom(0:3,2,4e14,1e14,log=TRUE));print(dnbinom(0:3,2,.8,log=TRUE))
cat("ordinary reference CDF recurrence discrepancy:\n")
print(max(abs(extraDistr::pbnbinom(0:100,1,2,5)-cumsum(extraDistr::dbnbinom(0:100,1,2,5)))))
cat("huge-shape reference CDF versus NB limit:\n")
print(extraDistr::pbnbinom(0:3,2,4e14,1e14));print(pnbinom(0:3,2,.8))
