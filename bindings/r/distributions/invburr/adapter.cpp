#include "common/distributions/invburr/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlinvburr_(const Rcpp::NumericVector &x, double gradtol = 1e-8,
 int iterlim = 1000, Rcpp::Nullable<double> shape10 = R_NilValue,
 Rcpp::Nullable<double> shape20 = R_NilValue,
 Rcpp::Nullable<double> scale0 = R_NilValue, double reltol = 1e-12,
 Rcpp::Nullable<double> scale = R_NilValue, double abstol = 1.220703e-4,
 Rcpp::Nullable<double> shape1 = R_NilValue, Rcpp::Nullable<double> shape2 = R_NilValue) {
  return common_result(umvl_binding::fit_invburr(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(shape1), common_held(shape2), common_held(scale), gradtol, iterlim, common_held(shape10), common_held(shape20), common_held(scale0), reltol, abstol));
}
