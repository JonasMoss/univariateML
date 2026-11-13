#include "common/distributions/geom/ztgeom_fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List modifier_ztgeom_(const Rcpp::IntegerVector &x, Rcpp::Nullable<double> prob = R_NilValue) {
  return common_result(umvl_binding::fit_ztgeom(cppunivariate::ispan{x.begin(), static_cast<std::size_t>(x.size())}, common_held(prob)));
}
