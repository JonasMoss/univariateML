#include "common/distributions/bbinom/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlbbinom_(const Rcpp::IntegerVector &x, Rcpp::Nullable<double> size,
                     Rcpp::Nullable<double> alpha = R_NilValue,
                     Rcpp::Nullable<double> beta = R_NilValue, double reltol = 1e-10,
                     int iterlim = 500) {
  return common_result(umvl_binding::fit_bbinom(cppunivariate::ispan{x.begin(), static_cast<std::size_t>(x.size())}, common_held(size), common_held(alpha), common_held(beta), reltol, iterlim));
}
