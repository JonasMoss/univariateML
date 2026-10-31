#include "common/distributions/lst/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mllst_(const Rcpp::NumericVector &x, double gradtol = 1.220703e-4,
                  int iterlim = 200, Rcpp::Nullable<double> df = R_NilValue, Rcpp::Nullable<double> mu = R_NilValue, Rcpp::Nullable<double> sigma = R_NilValue) {
  return common_result(umvl_binding::fit_lst(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(mu), common_held(sigma), common_held(df), gradtol, iterlim));
}
