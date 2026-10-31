#include "common/distributions/rayleigh/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlrayleigh_(const Rcpp::NumericVector &x,
    Rcpp::Nullable<double> sigma = R_NilValue) {
  return common_result(umvl_binding::fit_rayleigh(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(sigma)));
}
