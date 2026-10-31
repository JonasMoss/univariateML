#include "common/distributions/invgamma/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlinvgamma_(const Rcpp::NumericVector &x, double abstol = 1.220703e-4,
                       int iterlim = 100,
    Rcpp::Nullable<double> alpha = R_NilValue,
    Rcpp::Nullable<double> beta = R_NilValue) {
  return common_result(umvl_binding::fit_invgamma(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(alpha), common_held(beta), abstol, iterlim));
}
