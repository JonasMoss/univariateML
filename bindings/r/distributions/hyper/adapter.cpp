#include "common/distributions/hyper/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlhyper_(const Rcpp::NumericVector &x, Rcpp::Nullable<double> k,
                    Rcpp::Nullable<double> N,
                    Rcpp::Nullable<double> m = R_NilValue) {
  return common_result(umvl_binding::fit_hyper(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(m), common_held(N), common_held(k)));
}
