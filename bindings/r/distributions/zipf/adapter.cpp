#include "common/distributions/zipf/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlzipf_(const Rcpp::IntegerVector &x,
                   Rcpp::Nullable<double> N=R_NilValue, Rcpp::Nullable<double> s=R_NilValue,
                   double reltol = 1.220703e-4,
                   int iterlim = 100) {
  return common_result(umvl_binding::fit_zipf(cppunivariate::ispan{x.begin(), static_cast<std::size_t>(x.size())}, common_held(N), common_held(s), reltol, iterlim));
}
