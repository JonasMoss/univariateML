#include "common/distributions/pois/zip_fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List modifier_zip_(const Rcpp::IntegerVector &x, Rcpp::Nullable<double> lambda = R_NilValue, Rcpp::Nullable<double> pi = R_NilValue, int iterlim = 200) {
  return common_result(umvl_binding::fit_zip(cppunivariate::ispan{x.begin(), static_cast<std::size_t>(x.size())}, common_held(lambda), common_held(pi), iterlim));
}
