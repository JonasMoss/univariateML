#include "common/distributions/bhatt/fit.hpp"
#include "common_fit.h"
#include "dpqr.h"
// [[Rcpp::export]]
Rcpp::List mlbhatt_(const Rcpp::NumericVector& x,
    Rcpp::Nullable<double> sigma=R_NilValue,
    Rcpp::Nullable<double> a=R_NilValue,
    double reltol=1e-8,int iterlim=500, Rcpp::Nullable<double> mu=R_NilValue) {
  const auto fit=umvl_binding::fit_bhatt(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(mu), common_held(sigma), common_held(a), reltol, iterlim);
  return common_result(fit);
}
// [[Rcpp::export]]
Rcpp::NumericVector dbhatt_reference_(Rcpp::NumericVector x,Rcpp::NumericVector mu,Rcpp::NumericVector sigma,Rcpp::NumericVector a,bool log=false) {
  return dist_impl::dpqr_apply({x,mu,sigma,a},[&](const std::vector<double>& v){return cppunivariate::distributions::dbhatt(v[0],v[1],v[2],v[3],log);});
}
// [[Rcpp::export]]
Rcpp::NumericVector pbhatt_reference_(Rcpp::NumericVector x,Rcpp::NumericVector mu,Rcpp::NumericVector sigma,Rcpp::NumericVector a,bool lower_tail=true,bool log_p=false) {
  return dist_impl::dpqr_apply({x,mu,sigma,a},[&](const std::vector<double>& v){return cppunivariate::distributions::pbhatt(v[0],v[1],v[2],v[3],lower_tail,log_p);});
}
