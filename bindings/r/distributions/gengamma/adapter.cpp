#include "common/distributions/gengamma/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlgengamma_(const Rcpp::NumericVector& x,Rcpp::Nullable<double> mu=R_NilValue,
    Rcpp::Nullable<double> sigma=R_NilValue,Rcpp::Nullable<double> Q=R_NilValue,
    double tol=1e-10,int iterlim=100) {
  return common_result(umvl_binding::fit_gengamma(
      cppunivariate::span{x.begin(),static_cast<std::size_t>(x.size())},
      common_held(mu),common_held(sigma),common_held(Q),tol,iterlim));
}
