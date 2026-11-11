#include "common/distributions/fatigue/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlfatigue_(const Rcpp::NumericVector &x, Rcpp::Nullable<double> mu, double abstol = 1e-10,
                      int iterlim = 100, Rcpp::Nullable<double> alpha = R_NilValue,
                      Rcpp::Nullable<double> beta = R_NilValue) {
  return common_result(umvl_binding::fit_fatigue(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(alpha), common_held(beta), common_held(mu), abstol, iterlim));
}
