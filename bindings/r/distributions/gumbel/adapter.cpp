#include "common/distributions/gumbel/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlgumbel_(const Rcpp::NumericVector &x, double reltol = 1.220703e-4,
                     int iterlim = 100, Rcpp::Nullable<double> mu = R_NilValue, Rcpp::Nullable<double> sigma = R_NilValue) {
  return common_result(umvl_binding::fit_gumbel(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(mu), common_held(sigma), reltol, iterlim));
}
