#include "common/distributions/pois/ztpois_fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List modifier_ztpois_(const Rcpp::IntegerVector &x, Rcpp::Nullable<double> lambda = R_NilValue) {
  return common_result(umvl_binding::fit_ztpois(cppunivariate::ispan{x.begin(), static_cast<std::size_t>(x.size())}, common_held(lambda)));
}
