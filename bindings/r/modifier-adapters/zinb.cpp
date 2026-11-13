#include "common/distributions/nbinom/zinb_fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List modifier_zinb_(const Rcpp::IntegerVector &x, double reltol = 1e-10,
                   int iterlim = 200, Rcpp::Nullable<double> size = R_NilValue, Rcpp::Nullable<double> prob = R_NilValue, Rcpp::Nullable<double> pi = R_NilValue) {
  return common_result(umvl_binding::fit_zinb(cppunivariate::ispan{x.begin(), static_cast<std::size_t>(x.size())}, common_held(size), common_held(prob), common_held(pi), reltol, iterlim));
}
