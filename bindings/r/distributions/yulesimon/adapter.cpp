#include "common/distributions/yulesimon/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlyulesimon_(const Rcpp::IntegerVector &x,Rcpp::Nullable<double> rho=R_NilValue,double reltol=1e-10,int iterlim=200) {
  return common_result(umvl_binding::fit_yulesimon(cppunivariate::ispan{x.begin(), static_cast<std::size_t>(x.size())}, common_held(rho), reltol, iterlim));
}
