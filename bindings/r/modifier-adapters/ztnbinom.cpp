#include "common/distributions/nbinom/ztnbinom_fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List modifier_ztnbinom_(const Rcpp::IntegerVector &x, Rcpp::Nullable<double> size = R_NilValue, double reltol = 1e-10, int iterlim = 200, Rcpp::Nullable<double> prob = R_NilValue) {
  return common_result(umvl_binding::fit_ztnbinom(cppunivariate::ispan{x.begin(), static_cast<std::size_t>(x.size())}, common_held(size), common_held(prob), reltol, iterlim));
}
