#include "common/distributions/betapr/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlbetapr_(const Rcpp::NumericVector &x, double gradtol = 1e-6,
                     int iterlim = 2000, double shape10 = 0, double shape20 = 0,
                     double scale0 = 0, double reltol = 1e-13, double abstol = 1e-10,
                     Rcpp::Nullable<double> scale = R_NilValue,
                     Rcpp::Nullable<double> shape1 = R_NilValue,
                     Rcpp::Nullable<double> shape2 = R_NilValue) {
  return common_result(umvl_binding::fit_betapr(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(shape1), common_held(shape2), common_held(scale), gradtol, iterlim, shape10, shape20, scale0, reltol, abstol));
}
