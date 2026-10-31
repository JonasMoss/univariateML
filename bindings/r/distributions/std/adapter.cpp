#include "common/distributions/std/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlstd_(const Rcpp::NumericVector &x, double gradtol = 1.220703e-4,
                  int iterlim = 200, Rcpp::Nullable<double> nu = R_NilValue, Rcpp::Nullable<double> mean = R_NilValue, Rcpp::Nullable<double> sd = R_NilValue) {
  return common_result(umvl_binding::fit_std(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(mean), common_held(sd), common_held(nu), gradtol, iterlim));
}
