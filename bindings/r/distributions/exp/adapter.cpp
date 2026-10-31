#include "common/distributions/exp/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlexp_(const Rcpp::NumericVector &x,
    Rcpp::Nullable<double> rate = R_NilValue) {
  return common_result(umvl_binding::fit_exp(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(rate)), false);
}
