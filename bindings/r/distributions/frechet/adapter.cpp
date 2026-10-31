#include "common/distributions/frechet/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlfrechet_(const Rcpp::NumericVector &x,
 Rcpp::Nullable<double> mu,
 Rcpp::Nullable<double> lambda=R_NilValue,
 Rcpp::Nullable<double> sigma=R_NilValue,
 double reltol=1e-10, int iterlim=100) {
  return common_result(umvl_binding::fit_frechet(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(lambda), common_held(mu), common_held(sigma), reltol, iterlim));
}
