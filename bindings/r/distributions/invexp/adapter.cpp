#include "common/distributions/invexp/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlinvexp_(const Rcpp::NumericVector &x,
    Rcpp::Nullable<double> scale = R_NilValue) {
  return common_result(umvl_binding::fit_invexp(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(scale)));
}
