#include "common/distributions/pois/zmpois_fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List modifier_zmpois_(const Rcpp::IntegerVector &x, Rcpp::Nullable<double> lambda = R_NilValue, Rcpp::Nullable<double> p0 = R_NilValue, double tol = 1e-10, int iterlim = 200) {
  return common_result(umvl_binding::fit_zmpois(cppunivariate::ispan{x.begin(), static_cast<std::size_t>(x.size())}, common_held(lambda), common_held(p0), tol, iterlim));
}
