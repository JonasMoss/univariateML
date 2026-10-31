#include "common/distributions/zmgeom/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlzmgeom_(const Rcpp::IntegerVector &x, Rcpp::Nullable<double> prob = R_NilValue, Rcpp::Nullable<double> p0 = R_NilValue) {
  return common_result(umvl_binding::fit_zmgeom(cppunivariate::ispan{x.begin(), static_cast<std::size_t>(x.size())}, common_held(prob), common_held(p0)));
}
