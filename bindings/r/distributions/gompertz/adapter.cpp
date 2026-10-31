#include "common/distributions/gompertz/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlgompertz_(const Rcpp::NumericVector &x, double reltol = 1.220703e-4,
                       int iterlim = 100,
    Rcpp::Nullable<double> a = R_NilValue,
    Rcpp::Nullable<double> b = R_NilValue) {
  return common_result(umvl_binding::fit_gompertz(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(a), common_held(b), reltol, iterlim));
}
