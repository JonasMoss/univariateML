#include "common/distributions/triang/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mltriang_(const Rcpp::NumericVector &x,
    Rcpp::Nullable<double> a = R_NilValue,
    Rcpp::Nullable<double> b = R_NilValue,
    Rcpp::Nullable<double> c = R_NilValue,
    double reltol = 1e-12, int iterlim = 100) {
  return common_result(umvl_binding::fit_triang(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(a), common_held(b), common_held(c), reltol, iterlim));
}
