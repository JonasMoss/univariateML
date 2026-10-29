#include "as_span.h"
#include "dpqr.h"
#include "cppunivariate/distributions/slash/mle.hpp"
// [[Rcpp::export]]
Rcpp::List mlslash_(const Rcpp::NumericVector& x,
    Rcpp::Nullable<double> sigma=R_NilValue,
    double reltol=1e-8,int iterlim=500) {
  const double f_sigma=sigma.isNotNull()?Rcpp::as<double>(sigma):cppunivariate::special::kNaN;
  if((sigma.isNotNull() && !(std::isfinite(f_sigma) && f_sigma>0)))Rcpp::stop("Fixed scales must be positive finite numbers.");
  const auto f=cppunivariate::slash_mle(dist_impl::as_span(x),reltol,iterlim,f_sigma);
  if(f.status==cppunivariate::SlashStatus::Unbounded)Rcpp::stop("Slash likelihood is unbounded: a value has more than half the sample mass.");
  if(f.status==cppunivariate::SlashStatus::CollapseLimit)Rcpp::stop("Slash likelihood selects scale collapse; no finite fit is returned.");
  if(f.status!=cppunivariate::SlashStatus::Ok)Rcpp::stop("Slash fit failed: check inputs, reltol and iterlim.");
  Rcpp::CharacterVector fixed;
  if(sigma.isNotNull())fixed.push_back("sigma");
  return Rcpp::List::create(Rcpp::_["estimates"]=Rcpp::NumericVector::create(Rcpp::_["mu"]=f.mu,Rcpp::_["sigma"]=f.sigma),Rcpp::_["logLik"]=f.logLik,Rcpp::_["iterations"]=f.iterations,Rcpp::_["fixed"]=fixed);
}
// [[Rcpp::export]]
Rcpp::NumericVector dslash_reference_(Rcpp::NumericVector x,Rcpp::NumericVector mu,Rcpp::NumericVector sigma,bool log=false) {
  return dist_impl::dpqr_apply({x,mu,sigma},[&](const std::vector<double>& v){return cppunivariate::distributions::dslash(v[0],v[1],v[2],log);});
}
// [[Rcpp::export]]
Rcpp::NumericVector pslash_reference_(Rcpp::NumericVector x,Rcpp::NumericVector mu,Rcpp::NumericVector sigma,bool lower_tail=true,bool log_p=false) {
  return dist_impl::dpqr_apply({x,mu,sigma},[&](const std::vector<double>& v){return cppunivariate::distributions::pslash(v[0],v[1],v[2],lower_tail,log_p);});
}
