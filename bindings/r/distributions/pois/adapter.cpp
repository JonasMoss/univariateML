#include "common/distributions/pois/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlpois_(const Rcpp::IntegerVector &x, Rcpp::Nullable<double> lambda = R_NilValue) {
  return common_result(umvl_binding::fit_pois(cppunivariate::ispan{x.begin(), static_cast<std::size_t>(x.size())}, common_held(lambda)));
}
