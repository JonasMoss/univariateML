#include "common/distributions/skellam/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlskellam_(const Rcpp::NumericVector& x,
    Rcpp::Nullable<double> mu1=R_NilValue,Rcpp::Nullable<double> mu2=R_NilValue,
    double reltol=1e-9,int iterlim=150) {
  return common_result(umvl_binding::fit_skellam(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(mu1), common_held(mu2), reltol, iterlim));
}
