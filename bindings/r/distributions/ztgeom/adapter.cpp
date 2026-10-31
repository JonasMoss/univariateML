#include "common/distributions/ztgeom/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlztgeom_(const Rcpp::IntegerVector &x, Rcpp::Nullable<double> prob = R_NilValue) {
  return common_result(umvl_binding::fit_ztgeom(cppunivariate::ispan{x.begin(), static_cast<std::size_t>(x.size())}, common_held(prob)));
}
