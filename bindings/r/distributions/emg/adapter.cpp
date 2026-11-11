#include "common/distributions/emg/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlemg_(const Rcpp::NumericVector &x,
    Rcpp::Nullable<double> mu=R_NilValue,
    Rcpp::Nullable<double> sigma=R_NilValue,
    Rcpp::Nullable<double> lambda=R_NilValue,
    double reltol=1e-10,int iterlim=500) {
  return common_result(umvl_binding::fit_emg(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(mu), common_held(sigma), common_held(lambda), reltol, iterlim));
}
