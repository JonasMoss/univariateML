#include "common/distributions/dgamma/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mldgamma_(const Rcpp::NumericVector &x,
    Rcpp::Nullable<double> shape = R_NilValue,
    Rcpp::Nullable<double> rate = R_NilValue,
    double reltol = 1e-8, int iterlim = 400) {
  return common_result(umvl_binding::fit_dgamma(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(shape), common_held(rate), reltol, iterlim));
}
