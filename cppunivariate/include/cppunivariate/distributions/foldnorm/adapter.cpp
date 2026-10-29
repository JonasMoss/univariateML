#include "as_span.h"
#include "dpqr.h"
#include "cppunivariate/distributions/foldnorm/mle.hpp"
// [[Rcpp::export]]
Rcpp::List mlfoldnorm_(const Rcpp::NumericVector &x,
    Rcpp::Nullable<double> mu=R_NilValue,Rcpp::Nullable<double> sigma=R_NilValue,
    double reltol=1e-10,int iterlim=200) {
  const double m=mu.isNotNull() ? Rcpp::as<double>(mu) : R_NaN;
  const double s=sigma.isNotNull() ? Rcpp::as<double>(sigma) : R_NaN;
  if (mu.isNotNull() && !(std::isfinite(m) && m>=0)) Rcpp::stop("fixed `mu` must be finite and nonnegative.");
  if (sigma.isNotNull() && !(std::isfinite(s) && s>0)) Rcpp::stop("fixed `sigma` must be finite and positive.");
  const auto r=cppunivariate::foldnorm_mle(dist_impl::as_span(x),reltol,iterlim,m,s);
  if (r.status==cppunivariate::FoldnormStatus::Unbounded) Rcpp::stop("The folded-normal likelihood is unbounded as sigma tends to zero for these constant data.");
  if (r.status==cppunivariate::FoldnormStatus::InvalidInput) Rcpp::stop("Folded normal requires finite nonnegative data, positive reltol and positive iterlim.");
  if (r.status!=cppunivariate::FoldnormStatus::Ok) Rcpp::stop("Folded-normal profile search did not converge; increase iterlim.");
  Rcpp::CharacterVector fixed;
  if (mu.isNotNull()) fixed.push_back("mu");
  if (sigma.isNotNull()) fixed.push_back("sigma");
  return Rcpp::List::create(Rcpp::_["estimates"]=Rcpp::NumericVector::create(Rcpp::_["mu"]=r.mu,Rcpp::_["sigma"]=r.sigma),Rcpp::_["logLik"]=r.logLik,Rcpp::_["iterations"]=r.iterations,Rcpp::_["fixed"]=fixed);
}
namespace {
Rcpp::NumericVector foldnorm_evaluate(const std::vector<Rcpp::NumericVector> &args,char op,bool lower=true,bool log=false) {
  return dist_impl::dpqr_apply(args,[&](const std::vector<double> &a) {
    if (!cppunivariate::foldnorm_detail::valid(a[1],a[2])) return R_NaN;
    if (op=='d') return cppunivariate::distributions::dfoldnorm(a[0],a[1],a[2],log);
    if (op=='p') return cppunivariate::distributions::pfoldnorm(a[0],a[1],a[2],lower,log);
    if (op=='q') return cppunivariate::distributions::qfoldnorm(a[0],a[1],a[2],lower,log);
    return std::abs(R::rnorm(a[1],a[2]));
  });
}
}
// [[Rcpp::export]]
Rcpp::NumericVector dfoldnorm_(Rcpp::NumericVector x,Rcpp::NumericVector mu,Rcpp::NumericVector sigma,bool log=false) {
  return foldnorm_evaluate({x,mu,sigma},'d',true,log);
}
// [[Rcpp::export]]
Rcpp::NumericVector pfoldnorm_(Rcpp::NumericVector q,Rcpp::NumericVector mu,Rcpp::NumericVector sigma,bool lower_tail=true,bool log_p=false) {
  return foldnorm_evaluate({q,mu,sigma},'p',lower_tail,log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector qfoldnorm_(Rcpp::NumericVector p,Rcpp::NumericVector mu,Rcpp::NumericVector sigma,bool lower_tail=true,bool log_p=false) {
  return foldnorm_evaluate({p,mu,sigma},'q',lower_tail,log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector rfoldnorm_(int n,Rcpp::NumericVector mu,Rcpp::NumericVector sigma) {
  return foldnorm_evaluate({Rcpp::NumericVector(n),mu,sigma},'r');
}
