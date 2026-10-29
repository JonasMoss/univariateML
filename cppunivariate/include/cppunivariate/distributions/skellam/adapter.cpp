#include "as_span.h"
#include "dpqr.h"
#include "cppunivariate/distributions/skellam/mle.hpp"
// [[Rcpp::export]]
Rcpp::List mlskellam_(const Rcpp::NumericVector& x,
    Rcpp::Nullable<double> mu1=R_NilValue,Rcpp::Nullable<double> mu2=R_NilValue,
    double reltol=1e-9,int iterlim=150) {
  const double a=mu1.isNotNull()?Rcpp::as<double>(mu1):R_NaN;
  const double b=mu2.isNotNull()?Rcpp::as<double>(mu2):R_NaN;
  if((mu1.isNotNull() && !(std::isfinite(a) && a>=0)) ||
     (mu2.isNotNull() && !(std::isfinite(b) && b>=0))) Rcpp::stop("Fixed rates must be finite and nonnegative.");
  const auto fit=cppunivariate::skellam_mle(dist_impl::as_span(x),reltol,iterlim,a,b);
  if(fit.status==cppunivariate::SkellamStatus::InvalidInput) Rcpp::stop("Skellam requires finite integer observations, positive reltol and positive iterlim.");
  if(fit.status==cppunivariate::SkellamStatus::Impossible) Rcpp::stop("The fixed rates give zero likelihood for these observations.");
  if(fit.status!=cppunivariate::SkellamStatus::Ok) Rcpp::stop("Skellam search did not converge; increase iterlim.");
  Rcpp::CharacterVector fixed;
  if(mu1.isNotNull()) fixed.push_back("mu1");
  if(mu2.isNotNull()) fixed.push_back("mu2");
  return Rcpp::List::create(Rcpp::_["estimates"]=Rcpp::NumericVector::create(Rcpp::_["mu1"]=fit.mu1,Rcpp::_["mu2"]=fit.mu2),Rcpp::_["logLik"]=fit.logLik,Rcpp::_["iterations"]=fit.iterations,Rcpp::_["fixed"]=fixed);
}
namespace {
Rcpp::NumericVector skellam_evaluate(const std::vector<Rcpp::NumericVector>& args,char op,bool lower=true,bool log=false) {
  return dist_impl::dpqr_apply(args,[&](const std::vector<double>& a) {
    if(op=='d') return cppunivariate::distributions::dskellam(a[0],a[1],a[2],log);
    if(op=='p') return cppunivariate::distributions::pskellam(a[0],a[1],a[2],lower,log);
    if(op=='q') return cppunivariate::distributions::qskellam(a[0],a[1],a[2],lower,log);
    if(!cppunivariate::skellam_detail::valid(a[1],a[2])) return R_NaN;
    const double positive=R::rpois(a[1]);
    const double negative=R::rpois(a[2]);
    return positive-negative;
  },op=='d');
}
}
// [[Rcpp::export]]
Rcpp::NumericVector dskellam_(Rcpp::NumericVector x,Rcpp::NumericVector mu1,Rcpp::NumericVector mu2,bool log=false) {
  return skellam_evaluate({x,mu1,mu2},'d',true,log);
}
// [[Rcpp::export]]
Rcpp::NumericVector pskellam_(Rcpp::NumericVector q,Rcpp::NumericVector mu1,Rcpp::NumericVector mu2,bool lower_tail=true,bool log_p=false) {
  return skellam_evaluate({q,mu1,mu2},'p',lower_tail,log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector qskellam_(Rcpp::NumericVector p,Rcpp::NumericVector mu1,Rcpp::NumericVector mu2,bool lower_tail=true,bool log_p=false) {
  return skellam_evaluate({p,mu1,mu2},'q',lower_tail,log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector rskellam_(int n,Rcpp::NumericVector mu1,Rcpp::NumericVector mu2) {
  return skellam_evaluate({Rcpp::NumericVector(n),mu1,mu2},'r');
}
