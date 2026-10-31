#include "common/distributions/slash/fit.hpp"
#include "common_fit.h"
#include "dpqr.h"
// [[Rcpp::export]]
Rcpp::List mlslash_(const Rcpp::NumericVector& x,
    Rcpp::Nullable<double> sigma=R_NilValue,
    double reltol=1e-8,int iterlim=500, Rcpp::Nullable<double> mu=R_NilValue) {
  return common_result(umvl_binding::fit_slash(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(mu), common_held(sigma), reltol, iterlim));
}
// [[Rcpp::export]]
Rcpp::NumericVector dslash_reference_(Rcpp::NumericVector x,Rcpp::NumericVector mu,Rcpp::NumericVector sigma,bool log=false) {
  return dist_impl::dpqr_apply({x,mu,sigma},[&](const std::vector<double>& v){return cppunivariate::distributions::dslash(v[0],v[1],v[2],log);});
}
// [[Rcpp::export]]
Rcpp::NumericVector pslash_reference_(Rcpp::NumericVector x,Rcpp::NumericVector mu,Rcpp::NumericVector sigma,bool lower_tail=true,bool log_p=false) {
  return dist_impl::dpqr_apply({x,mu,sigma},[&](const std::vector<double>& v){return cppunivariate::distributions::pslash(v[0],v[1],v[2],lower_tail,log_p);});
}
