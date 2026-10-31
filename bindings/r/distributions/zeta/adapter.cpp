#include "common/distributions/zeta/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlzeta_(const Rcpp::IntegerVector &x,Rcpp::Nullable<double> s=R_NilValue,double reltol=1e-10,int iterlim=200) {
  return common_result(umvl_binding::fit_zeta(cppunivariate::ispan{x.begin(), static_cast<std::size_t>(x.size())}, common_held(s), reltol, iterlim));
}
