#include "common/distributions/cmp/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlcmp_(const Rcpp::NumericVector &x,
 Rcpp::Nullable<double> lambda=R_NilValue,Rcpp::Nullable<double> nu=R_NilValue,
 double reltol=1e-10,int iterlim=100) {
  return common_result(umvl_binding::fit_cmp(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(lambda), common_held(nu), reltol, iterlim));
}
