#include "common/distributions/tnorm/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mltnorm_(const Rcpp::NumericVector &x,
                    Rcpp::Nullable<double> a,
                    Rcpp::Nullable<double> b,
                    Rcpp::Nullable<double> mean = R_NilValue,
                    Rcpp::Nullable<double> sd = R_NilValue,
                    double reltol = 1e-12, int iterlim = 100) {
  return common_result(umvl_binding::fit_tnorm(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(mean), common_held(sd), common_held(a), common_held(b), reltol, iterlim));
}
