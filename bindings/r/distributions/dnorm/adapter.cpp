#include "common/distributions/dnorm/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mldnorm_(const Rcpp::NumericVector &x,
    Rcpp::Nullable<double> mean = R_NilValue,
    Rcpp::Nullable<double> sd = R_NilValue,
    double reltol = 1e-9, int iterlim = 200) {
  return common_result(umvl_binding::fit_dnorm(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(mean), common_held(sd), reltol, iterlim));
}
