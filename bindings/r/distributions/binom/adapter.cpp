#include "common/distributions/binom/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlbinom_(const Rcpp::IntegerVector &x,
                    Rcpp::Nullable<double> size,
                    double abstol = 1.220703e-4, int iterlim = 100, Rcpp::Nullable<double> prob = R_NilValue) {
  return common_result(umvl_binding::fit_binom(cppunivariate::ispan{x.begin(), static_cast<std::size_t>(x.size())}, common_held(size), common_held(prob), abstol, iterlim));
}
