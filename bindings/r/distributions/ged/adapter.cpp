#include "common/distributions/ged/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlged_(const Rcpp::NumericVector &x, double tol = 1e-10,
                  int iterlim = 200, Rcpp::Nullable<double> mean = R_NilValue,
                  Rcpp::Nullable<double> sd = R_NilValue, Rcpp::Nullable<double> nu = R_NilValue) {
  return common_result(umvl_binding::fit_ged(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(mean), common_held(sd), common_held(nu), tol, iterlim));
}
