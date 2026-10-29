#include "as_span.h"
#include "dpqr.h"
#include "cppunivariate/distributions/tlambda/mle.hpp"
// [[Rcpp::export]]
Rcpp::List mltlambda_(const Rcpp::NumericVector& x,
                     Rcpp::Nullable<double> lambda=R_NilValue,
                     double reltol=1e-7,int iterlim=1000, Rcpp::Nullable<double> mu=R_NilValue,
                     Rcpp::Nullable<double> sigma=R_NilValue) {
  const double l=lambda.isNotNull()?Rcpp::as<double>(lambda):std::numeric_limits<double>::quiet_NaN();
  if(lambda.isNotNull() && !std::isfinite(l)) Rcpp::stop("Fixed lambda must be finite.");
  const double m=mu.isNotNull()?Rcpp::as<double>(mu):std::numeric_limits<double>::quiet_NaN();
  const double s=sigma.isNotNull()?Rcpp::as<double>(sigma):std::numeric_limits<double>::quiet_NaN();
  if(mu.isNotNull()&&!std::isfinite(m))Rcpp::stop("Held mu must be finite.");
  if(sigma.isNotNull()&&!(std::isfinite(s)&&s>0))Rcpp::stop("Held sigma must be positive and finite.");
  const auto f=cppunivariate::tlambda_mle(dist_impl::as_span(x),reltol,iterlim,l,m,s);
  if(f.status==cppunivariate::TlambdaStatus::Unbounded)Rcpp::stop("Tukey lambda likelihood is unbounded through scale collapse at the held location.");
  if(f.status==cppunivariate::TlambdaStatus::CollapseLimit)Rcpp::stop("Tukey lambda selects the scale collapse boundary at the held location.");
  if(f.status==cppunivariate::TlambdaStatus::InvalidSupport)Rcpp::stop("Held Tukey lambda support cannot contain the sample with positive likelihood.");
  if(f.status==cppunivariate::TlambdaStatus::InvalidShape) Rcpp::stop("Fixed lambda must be in [-1, 1], or 2 (uniform alias).");
  if(f.status==cppunivariate::TlambdaStatus::InvalidData) Rcpp::stop("Tukey lambda fitting requires at least three finite observations, every tie multiplicity strictly below half the sample size, positive reltol and positive iterlim. The sample range must be finite and standardization must preserve distinct values.");
  if(f.status!=cppunivariate::TlambdaStatus::Ok) Rcpp::stop("Tukey lambda search did not converge; increase iterlim.");
  Rcpp::CharacterVector fixed;
  if(mu.isNotNull())fixed.push_back("mu");
  if(sigma.isNotNull())fixed.push_back("sigma");
  if(lambda.isNotNull())fixed.push_back("lambda");
  return Rcpp::List::create(Rcpp::_["estimates"]=Rcpp::NumericVector::create(Rcpp::_["mu"]=f.mu,Rcpp::_["sigma"]=f.sigma,Rcpp::_["lambda"]=f.lambda),Rcpp::_["logLik"]=f.logLik,Rcpp::_["iterations"]=f.iterations,Rcpp::_["fixed"]=fixed);
}
namespace {
Rcpp::NumericVector tlambda_evaluate(const std::vector<Rcpp::NumericVector>& args,char op,bool lower=true,bool log=false) {
  return dist_impl::dpqr_apply(args,[&](const std::vector<double>& a) {
    if(op=='d') return cppunivariate::distributions::dtlambda(a[0],a[1],a[2],a[3],log);
    if(op=='p') return cppunivariate::distributions::ptlambda(a[0],a[1],a[2],a[3],lower,log);
    if(op=='q') return cppunivariate::distributions::qtlambda(a[0],a[1],a[2],a[3],lower,log);
    return cppunivariate::distributions::qtlambda(R::unif_rand(),a[1],a[2],a[3]);
  });
}
}
// [[Rcpp::export]]
Rcpp::NumericVector dtlambda_(Rcpp::NumericVector x,Rcpp::NumericVector mu,Rcpp::NumericVector sigma,Rcpp::NumericVector lambda,bool log=false) {
  return tlambda_evaluate({x,mu,sigma,lambda},'d',true,log);
}
// [[Rcpp::export]]
Rcpp::NumericVector ptlambda_(Rcpp::NumericVector q,Rcpp::NumericVector mu,Rcpp::NumericVector sigma,Rcpp::NumericVector lambda,bool lower_tail=true,bool log_p=false) {
  return tlambda_evaluate({q,mu,sigma,lambda},'p',lower_tail,log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector qtlambda_(Rcpp::NumericVector p,Rcpp::NumericVector mu,Rcpp::NumericVector sigma,Rcpp::NumericVector lambda,bool lower_tail=true,bool log_p=false) {
  return tlambda_evaluate({p,mu,sigma,lambda},'q',lower_tail,log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector rtlambda_(int n,Rcpp::NumericVector mu,Rcpp::NumericVector sigma,Rcpp::NumericVector lambda) {
  return tlambda_evaluate({Rcpp::NumericVector(n),mu,sigma,lambda},'r');
}
