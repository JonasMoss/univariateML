#include "common/distributions/genbeta/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlgenbeta_(const Rcpp::NumericVector &x, double scale,
    double gradtol = 1e-10, int iterlim = 300,
    Rcpp::Nullable<double> shape1 = R_NilValue,
    Rcpp::Nullable<double> shape2 = R_NilValue,
    Rcpp::Nullable<double> shape3 = R_NilValue) {
  return common_result(umvl_binding::fit_genbeta(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(shape1), common_held(shape2), common_held(shape3), common_held(scale), gradtol, iterlim));
}
