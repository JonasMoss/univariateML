#include "common/distributions/ztpois/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlztpois_(const Rcpp::IntegerVector &x, Rcpp::Nullable<double> lambda = R_NilValue) {
  return common_result(umvl_binding::fit_ztpois(cppunivariate::ispan{x.begin(), static_cast<std::size_t>(x.size())}, common_held(lambda)));
}
