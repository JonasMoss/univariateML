#!/usr/bin/env Rscript
# Reproduce the catalogue with the core's standalone RNG and a resumable CSV.
# /tmp/umvl-cpu/run Rscript <this-file> <output.csv> [replicates=100] [n=all]
args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) args[1] else "/tmp/umvl-codex/slashbhatt/escape-study-reproduced.csv"
reps <- if (length(args) >= 2L) as.integer(args[2]) else 100L
sizes <- if (length(args) >= 3L) as.integer(args[3]) else c(20L,100L,1000L)
stopifnot(length(reps)==1L, !is.na(reps), reps>=1L, reps<=100L,
          all(sizes %in% c(20L,100L,1000L)))
script <- sub("^--file=", "", commandArgs()[grepl("^--file=",commandArgs())][1])
headers <- normalizePath(file.path(dirname(script),"../../.."))
Sys.setenv(PKG_CPPFLAGS=paste(Sys.getenv("PKG_CPPFLAGS"),paste0("-I",shQuote(headers))))
Rcpp::sourceCpp(code='
// [[Rcpp::depends(BH)]]
// [[Rcpp::plugins(cpp17)]]
#include <Rcpp.h>
#include "cppunivariate/distributions/bhatt/mle.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
// [[Rcpp::export]]
Rcpp::NumericVector bhatt_escape_case(int n,int model,int rep) {
  cppunivariate::distributions::StdRng<> rng(20261029+n*1000+model*100+rep);
  std::vector<double> x;
  for(int i=0;i<n;++i)x.push_back(model==0?rng.rnorm():(model==1?rng.runif(-std::sqrt(3.0),std::sqrt(3.0)):rng.rnorm()+rng.runif(-2,2)));
  const auto f=cppunivariate::bhatt_mle({x.data(),x.size()});
  return Rcpp::NumericVector::create(static_cast<int>(f.status),f.mu,f.sigma,f.a,f.logLik);
}')
old <- if(file.exists(out)) read.csv(out) else NULL
key <- function(model,n,rep) paste(model,n,rep,sep=":")
done <- if(is.null(old)) character() else key(old$model,old$n,old$rep)
for(n in sizes) for(model in 0:2) for(rep in seq_len(reps)-1L) {
  if(key(model,n,rep) %in% done) next
  fit <- bhatt_escape_case(n,model,rep)
  row <- data.frame(model=model,n=n,rep=rep,status=fit[1],mu=fit[2],
                    sigma=fit[3],a=fit[4],logLik=fit[5])
  write.table(row,out,sep=",",row.names=FALSE,col.names=!file.exists(out),
              append=file.exists(out),quote=FALSE)
  cat("model",model,"n",n,"rep",rep,"status",fit[1],"\n")
}
