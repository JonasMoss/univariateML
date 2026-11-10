#include "common/distributions/pig/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlpig_(const Rcpp::IntegerVector &x, double reltol=1e-10, int iterlim=200,
 Rcpp::Nullable<double> mean=R_NilValue, Rcpp::Nullable<double> shape=R_NilValue) {
  return common_result(umvl_binding::fit_pig(cppunivariate::ispan{x.begin(), static_cast<std::size_t>(x.size())}, common_held(mean), common_held(shape), reltol, iterlim));
}
