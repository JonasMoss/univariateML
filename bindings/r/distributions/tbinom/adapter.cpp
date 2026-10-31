#include "common/distributions/tbinom/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mltbinom_(const Rcpp::IntegerVector &x, Rcpp::Nullable<double> a, Rcpp::Nullable<double> b, Rcpp::Nullable<double> size, Rcpp::Nullable<double> prob=R_NilValue) {
  return common_result(umvl_binding::fit_tbinom(cppunivariate::ispan{x.begin(), static_cast<std::size_t>(x.size())}, common_held(prob), common_held(a), common_held(b), common_held(size)));
}
