#include "common/distributions/dlaplace/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mldlaplace_(const Rcpp::IntegerVector &x, Rcpp::Nullable<double> location=R_NilValue, Rcpp::Nullable<double> scale=R_NilValue) {
  return common_result(umvl_binding::fit_dlaplace(cppunivariate::ispan{x.begin(), static_cast<std::size_t>(x.size())}, common_held(location), common_held(scale)));
}
