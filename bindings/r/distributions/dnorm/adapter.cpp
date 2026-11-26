#include "common/distributions/dnorm/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mldnorm_(const Rcpp::NumericVector &x,
    Rcpp::Nullable<double> mean = R_NilValue,
    Rcpp::Nullable<double> sd = R_NilValue,
    double tol = 1e-10, int iterlim = 200) {
  auto fit = umvl_binding::fit_dnorm(
      cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())},
      common_held(mean), common_held(sd), tol, iterlim);
  if (fit.outcome.reason == cppunivariate::OutcomeReason::Tolerance)
    fit.error = "Requested tolerance exceeds binary64 location resolution at the held scale.";
  return common_result(fit);
}
