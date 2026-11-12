#include "common/modifiers.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List ml_modifier_(std::string family,Rcpp::NumericVector lower,Rcpp::NumericVector upper,
    Rcpp::List held,std::string zero,bool window,double a,double b,bool censored,double tol,int maxit) {
  std::vector<std::pair<std::string,umvl_binding::Held>> holds;
  Rcpp::CharacterVector names=held.size()?Rcpp::CharacterVector(held.names()):Rcpp::CharacterVector();
  for(int i=0;i<held.size();++i)if(!Rf_isNull(held[i])) {
    auto values=Rcpp::as<Rcpp::NumericVector>(held[i]);
    if(values.size()!=1 || !std::isfinite(values[0]))Rcpp::stop("held coefficients must be finite scalars");
    holds.push_back({Rcpp::as<std::string>(names[i]),{true,values[0]}});
  }
  const cppunivariate::span lo{lower.begin(),static_cast<std::size_t>(lower.size())},hi{upper.begin(),static_cast<std::size_t>(upper.size())};
  if(censored)return common_result(umvl_binding::fit_censored(family,{lo,hi},holds,tol,maxit));
  return common_result(umvl_binding::fit_modifier(family,lo,holds,zero,window,a,b,tol,maxit));
}
// [[Rcpp::export]]
Rcpp::NumericVector ml_zeta_window_(std::string kind,Rcpp::NumericVector x,double N,double s,bool lower=true,bool log=false) {
  Rcpp::NumericVector out(x.size());
  if(!std::isfinite(N)||N<1||N>2147483647.||N!=std::floor(N))Rcpp::stop("finite zeta window requires integer N in [1,INT_MAX]");
  for(int i=0;i<x.size();++i)out[i]=cppunivariate::zeta_window_eval(kind[0],x[i],static_cast<int>(N),s,lower,log);
  return out;
}
