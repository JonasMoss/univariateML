#include "common/distributions/tlambda/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mltlambda_(const Rcpp::NumericVector& x,
                     Rcpp::Nullable<double> lambda=R_NilValue,
                     double reltol=1e-7,int iterlim=1000, Rcpp::Nullable<double> mu=R_NilValue,
                     Rcpp::Nullable<double> sigma=R_NilValue) {
  return common_result(umvl_binding::fit_tlambda(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(mu), common_held(sigma), common_held(lambda), reltol, iterlim));
}
