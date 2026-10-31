#include "common/distributions/nhyper/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlnhyper_(const Rcpp::NumericVector &x, Rcpp::Nullable<double> r,
                     Rcpp::Nullable<double> N,
                     Rcpp::Nullable<double> m = R_NilValue) {
  return common_result(umvl_binding::fit_nhyper(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(m), common_held(N), common_held(r)));
}
