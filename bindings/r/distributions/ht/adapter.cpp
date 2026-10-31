#include "common/distributions/ht/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlht_(const Rcpp::NumericVector &x, Rcpp::Nullable<double> mu,
                 Rcpp::Nullable<double> nu = R_NilValue,
                 Rcpp::Nullable<double> sigma = R_NilValue) {
  return common_result(umvl_binding::fit_ht(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(mu), common_held(nu), common_held(sigma)));
}
