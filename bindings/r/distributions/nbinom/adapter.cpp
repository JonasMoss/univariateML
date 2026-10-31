#include "common/distributions/nbinom/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlnbinom_(const Rcpp::IntegerVector &x, double reltol = 1.220703e-4,
                     int iterlim = 100, Rcpp::Nullable<double> size = R_NilValue, Rcpp::Nullable<double> prob = R_NilValue) {
  return common_result(umvl_binding::fit_nbinom(cppunivariate::ispan{x.begin(), static_cast<std::size_t>(x.size())}, common_held(size), common_held(prob), reltol, iterlim));
}
