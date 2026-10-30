#include "as_span.h"
#include "dpqr.h"
#include "cppunivariate/distributions/bhatt/mle.hpp"
// [[Rcpp::export]]
Rcpp::List mlbhatt_(const Rcpp::NumericVector& x,
    Rcpp::Nullable<double> sigma=R_NilValue,
    Rcpp::Nullable<double> a=R_NilValue,
    double reltol=1e-8,int iterlim=500, Rcpp::Nullable<double> mu=R_NilValue) {
  const double f_sigma=sigma.isNotNull()?Rcpp::as<double>(sigma):cppunivariate::special::kNaN;
  const double f_a=a.isNotNull()?Rcpp::as<double>(a):cppunivariate::special::kNaN;
  if((sigma.isNotNull() && !(std::isfinite(f_sigma) && f_sigma>0)) || (a.isNotNull() && !(std::isfinite(f_a) && f_a>=0)))Rcpp::stop("Fixed sigma must be positive and a nonnegative; both must be finite.");
  const double f_mu=mu.isNotNull()?Rcpp::as<double>(mu):cppunivariate::special::kNaN;
  if(mu.isNotNull()&&!std::isfinite(f_mu))Rcpp::stop("Held mu must be finite.");
  const auto f=cppunivariate::bhatt_mle(dist_impl::as_span(x),reltol,iterlim,f_sigma,f_a,f_mu);
  if(f.status==cppunivariate::BhattStatus::Unbounded)Rcpp::stop("Bhattacharjee likelihood is unbounded for a constant sample.");
  if(f.status==cppunivariate::BhattStatus::NormalLimit) {
    if(sigma.isNotNull())Rcpp::stop("Bhattacharjee likelihood selects the normal face with sigma held. Use `mlnorm` for an unrestricted fit; it does not hold sigma.");
    Rcpp::stop("Bhattacharjee likelihood selects the normal face: use `mlnorm`.");
  }
  if(f.status==cppunivariate::BhattStatus::UniformLimit) {
    if(a.isNotNull())Rcpp::stop("Bhattacharjee likelihood selects the uniform face with a held. Use `mlunif` for an unrestricted fit; it does not hold a.");
    Rcpp::stop("Bhattacharjee likelihood selects the uniform face: use `mlunif`.");
  }
  if(f.status!=cppunivariate::BhattStatus::Ok)Rcpp::stop("Bhatt fit failed: check inputs, reltol and iterlim.");
  Rcpp::CharacterVector fixed;
  if(mu.isNotNull())fixed.push_back("mu");
  if(sigma.isNotNull())fixed.push_back("sigma");
  if(a.isNotNull())fixed.push_back("a");
  return Rcpp::List::create(Rcpp::_["estimates"]=Rcpp::NumericVector::create(Rcpp::_["mu"]=f.mu,Rcpp::_["sigma"]=f.sigma,Rcpp::_["a"]=f.a),Rcpp::_["logLik"]=f.logLik,Rcpp::_["iterations"]=f.iterations,Rcpp::_["fixed"]=fixed);
}
// [[Rcpp::export]]
Rcpp::NumericVector dbhatt_reference_(Rcpp::NumericVector x,Rcpp::NumericVector mu,Rcpp::NumericVector sigma,Rcpp::NumericVector a,bool log=false) {
  return dist_impl::dpqr_apply({x,mu,sigma,a},[&](const std::vector<double>& v){return cppunivariate::distributions::dbhatt(v[0],v[1],v[2],v[3],log);});
}
// [[Rcpp::export]]
Rcpp::NumericVector pbhatt_reference_(Rcpp::NumericVector x,Rcpp::NumericVector mu,Rcpp::NumericVector sigma,Rcpp::NumericVector a,bool lower_tail=true,bool log_p=false) {
  return dist_impl::dpqr_apply({x,mu,sigma,a},[&](const std::vector<double>& v){return cppunivariate::distributions::pbhatt(v[0],v[1],v[2],v[3],lower_tail,log_p);});
}
