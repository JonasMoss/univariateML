#include "common/distributions/cmb/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlcmb_(const Rcpp::IntegerVector &x,Rcpp::Nullable<double> size,
                  Rcpp::Nullable<double> prob=R_NilValue,
                  Rcpp::Nullable<double> nu=R_NilValue,
                  double reltol=1e-10,int iterlim=2000) {
  return common_result(umvl_binding::fit_cmb(cppunivariate::ispan{x.begin(), static_cast<std::size_t>(x.size())}, common_held(size), common_held(prob), common_held(nu), reltol, iterlim));
}
