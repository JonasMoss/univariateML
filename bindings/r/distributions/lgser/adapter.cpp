#include "common/distributions/lgser/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mllgser_(const Rcpp::IntegerVector &x, Rcpp::Nullable<double> theta = R_NilValue) {
  return common_result(umvl_binding::fit_lgser(cppunivariate::ispan{x.begin(), static_cast<std::size_t>(x.size())}, common_held(theta)));
}
