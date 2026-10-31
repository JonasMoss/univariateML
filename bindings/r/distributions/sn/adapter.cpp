#include "common/distributions/sn/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlsn_(const Rcpp::NumericVector& x,
                Rcpp::Nullable<double> xi=R_NilValue,
                Rcpp::Nullable<double> omega=R_NilValue,
                Rcpp::Nullable<double> alpha=R_NilValue,
                double reltol=1e-9,int iterlim=100) {
  return common_result(umvl_binding::fit_sn(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(xi), common_held(omega), common_held(alpha), reltol, iterlim));
}
