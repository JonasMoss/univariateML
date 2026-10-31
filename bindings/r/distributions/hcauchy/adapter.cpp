#include "common/distributions/hcauchy/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlhcauchy_(const Rcpp::NumericVector &x, Rcpp::Nullable<double> mu,
                       Rcpp::Nullable<double> sigma = R_NilValue) {
  return common_result(umvl_binding::fit_hcauchy(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(mu), common_held(sigma)));
}
