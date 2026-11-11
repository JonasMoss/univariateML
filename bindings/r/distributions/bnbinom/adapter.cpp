#include "common/distributions/bnbinom/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlbnbinom_(const Rcpp::IntegerVector &x,
    Rcpp::Nullable<double> size=R_NilValue,
    Rcpp::Nullable<double> alpha=R_NilValue,
    Rcpp::Nullable<double> beta=R_NilValue,
    double gradtol=1e-10, int iterlim=600) {
  return common_result(umvl_binding::fit_bnbinom(cppunivariate::ispan{x.begin(), static_cast<std::size_t>(x.size())}, common_held(size), common_held(alpha), common_held(beta), gradtol, iterlim));
}
