#include "common/distributions/foldnorm/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlfoldnorm_(const Rcpp::NumericVector &x,
    Rcpp::Nullable<double> mu=R_NilValue,Rcpp::Nullable<double> sigma=R_NilValue,
    double reltol=1e-10,int iterlim=200) {
  return common_result(umvl_binding::fit_foldnorm(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(mu), common_held(sigma), reltol, iterlim));
}
