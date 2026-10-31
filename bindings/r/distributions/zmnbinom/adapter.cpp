#include "common/distributions/zmnbinom/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlzmnbinom_(const Rcpp::IntegerVector &x, Rcpp::Nullable<double> size = R_NilValue, double reltol = 1e-10, int iterlim = 200, Rcpp::Nullable<double> prob = R_NilValue, Rcpp::Nullable<double> p0 = R_NilValue) {
  return common_result(umvl_binding::fit_zmnbinom(cppunivariate::ispan{x.begin(), static_cast<std::size_t>(x.size())}, common_held(size), common_held(prob), common_held(p0), reltol, iterlim));
}
