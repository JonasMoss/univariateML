#include "common/distributions/kumar/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlkumar_(const Rcpp::NumericVector &x, double tol = 1e-10,
                    int iterlim = 100,
    Rcpp::Nullable<double> a = R_NilValue,
    Rcpp::Nullable<double> b = R_NilValue) {
  return common_result(umvl_binding::fit_kumar(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(a), common_held(b), tol, iterlim));
}
