#include "common/distributions/sged/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlsged_(const Rcpp::NumericVector &x,
    Rcpp::Nullable<double> mean = R_NilValue,
    Rcpp::Nullable<double> sd = R_NilValue,
    Rcpp::Nullable<double> nu = R_NilValue,
    Rcpp::Nullable<double> xi = R_NilValue, double gradtol = 1.220703e-4, int iterlim = 200) {
  return common_result(umvl_binding::fit_sged(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(mean), common_held(sd), common_held(nu), common_held(xi), gradtol, iterlim));
}
