#include "common/distributions/huber/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlhuber_(const Rcpp::NumericVector &x,
    Rcpp::Nullable<Rcpp::NumericVector> epsilon,
    Rcpp::Nullable<double> mu = R_NilValue,
    Rcpp::Nullable<double> sigma = R_NilValue,
    double gradtol = 1e-10, int iterlim = 200) {
  return common_result(umvl_binding::fit_huber(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(mu), common_held(sigma), common_held(epsilon), gradtol, iterlim));
}
