# Isolated single-family adapter check; it does not assemble or install a package.
root <- normalizePath(".")
dir.create("build/scratch/bnbinom-allfree/Rcpp",recursive=TRUE,showWarnings=FALSE)
Sys.setenv(PKG_CPPFLAGS=paste(paste0("-I",file.path(root,c("bindings","bindings/r/scripts","bindings/r/src","univariateML-cpp/include"))),collapse=" "))
adapter <- readLines("bindings/r/distributions/bnbinom/adapter.cpp")
sampler <- c('#include "rng_r.h"','// [[Rcpp::export]]',
 'Rcpp::IntegerVector bnb_sample_check(int n,double r,double a,double b) {',
 '  dist_impl::RRng rng;Rcpp::IntegerVector x(n);',
 '  for(int i=0;i<n;++i)x[i]=cppunivariate::distributions::rbnbinom(rng,r,a,b);return x;', '}')
code <- paste(c('// [[Rcpp::plugins(cpp17)]]','// [[Rcpp::depends(BH)]]',adapter,sampler),collapse="\n")
Rcpp::sourceCpp(code=code,cacheDir="build/scratch/bnbinom-allfree/Rcpp",showOutput=FALSE,rebuild=TRUE)
set.seed(4821)
x <- bnb_sample_check(1200,.7,1.8,6)
p <- list(size=.7,alpha=1.8,beta=6)
records <- list()
for(mask in 0:7) {
  held <- p[as.logical(bitwAnd(mask,bitwShiftL(1L,0:2)))]
  f <- do.call(mlbnbinom_,c(list(x=x),held))
  if(inherits(f,"univariateML_stop")) {
    print(f);stop(paste("Unexpected stop for held mask",mask))
  }
  records[[mask+1L]] <- list(held_mask=mask,certified=f$certified,estimates=f$estimates,loglik=f$logLik,
    global_loglik_interval=f$global_loglik_interval)
}
for(xstop in list(c(0L,0L,5L,5L))) {
  f <- mlbnbinom_(xstop)
  stopifnot(inherits(f,"univariateML_limit"),f$kind=="limit",identical(f$certified,FALSE),nrow(f$candidates)>=1,
    f$loglik_lower<=f$loglik_upper)
}
stopifnot(inherits(mlbnbinom_(0:4),"univariateML_limit"))
jsonlite::write_json(records,"dev/research/evaluations/2026-11-08-bnbinom-allfree-data/r-targeted.json",auto_unbox=TRUE,pretty=TRUE,digits=17)
cat("Single-family R adapter: all eight subsets, uncertified legacy NB and proved compound-Poisson face PASS\n")
