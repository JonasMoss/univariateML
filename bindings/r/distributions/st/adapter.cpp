#include "common/distributions/st/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlst_(const Rcpp::NumericVector& x,Rcpp::Nullable<double> xi=R_NilValue,Rcpp::Nullable<double> omega=R_NilValue,Rcpp::Nullable<double> alpha=R_NilValue,Rcpp::Nullable<double> nu=R_NilValue,double reltol=1e-10,int iterlim=200) {
 return common_result(umvl_binding::fit_st(cppunivariate::span{x.begin(),static_cast<std::size_t>(x.size())},common_held(xi),common_held(omega),common_held(alpha),common_held(nu),reltol,iterlim));
}
