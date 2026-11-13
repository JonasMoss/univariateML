#include "common/distributions/pois/tpois_fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List modifier_tpois_(const Rcpp::IntegerVector &x, Rcpp::Nullable<double> a, Rcpp::Nullable<double> b, Rcpp::Nullable<double> lambda=R_NilValue) {
  return common_result(umvl_binding::fit_tpois(cppunivariate::ispan{x.begin(), static_cast<std::size_t>(x.size())}, common_held(lambda), common_held(a), common_held(b)));
}
