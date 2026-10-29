#include "as_span.h"
#include "dpqr.h"
#include "cppunivariate/distributions/sn/mle.hpp"
// [[Rcpp::export]]
Rcpp::List mlsn_(const Rcpp::NumericVector& x,
                Rcpp::Nullable<double> xi=R_NilValue,
                Rcpp::Nullable<double> omega=R_NilValue,
                Rcpp::Nullable<double> alpha=R_NilValue,
                double reltol=1e-9,int iterlim=100) {
  const double nan=cppunivariate::special::kNaN;
  const double c=xi.isNotNull() ? Rcpp::as<double>(xi) : nan;
  const double w=omega.isNotNull() ? Rcpp::as<double>(omega) : nan;
  const double a=alpha.isNotNull() ? Rcpp::as<double>(alpha) : nan;
  if ((xi.isNotNull() && !std::isfinite(c)) ||
      (omega.isNotNull() && !(std::isfinite(w) && w>0)) ||
      (alpha.isNotNull() && !std::isfinite(a))) Rcpp::stop("Fixed xi and alpha must be finite; fixed omega must be finite and positive.");
  const auto fit=cppunivariate::sn_mle(dist_impl::as_span(x),reltol,iterlim,c,w,a);
  if(fit.status==cppunivariate::SnStatus::RightHalfNormalBoundary)
    Rcpp::stop("The skew-normal MLE is infinite: use `mlhnorm`.");
  if(fit.status==cppunivariate::SnStatus::LeftHalfNormalBoundary)
    Rcpp::stop("The skew-normal MLE is infinite: use `mlhnorm` on `-x`.");
  if(fit.status==cppunivariate::SnStatus::InvalidInput) Rcpp::stop("Skew-normal fitting requires distinct finite observations, positive reltol and positive iterlim.");
  if(fit.status!=cppunivariate::SnStatus::Ok) Rcpp::stop("Skew-normal inner fit did not converge; increase iterlim.");
  Rcpp::CharacterVector fixed;
  if(xi.isNotNull()) fixed.push_back("xi");
  if(omega.isNotNull()) fixed.push_back("omega");
  if(alpha.isNotNull()) fixed.push_back("alpha");
  return Rcpp::List::create(Rcpp::_["estimates"]=Rcpp::NumericVector::create(
    Rcpp::_["xi"]=fit.xi,Rcpp::_["omega"]=fit.omega,Rcpp::_["alpha"]=fit.alpha),
    Rcpp::_["logLik"]=fit.logLik,Rcpp::_["iterations"]=fit.iterations,Rcpp::_["fixed"]=fixed);
}
namespace {
Rcpp::NumericVector sn_evaluate(const std::vector<Rcpp::NumericVector>& args,char op,bool lower=true,bool log=false) {
  return dist_impl::dpqr_apply(args,[&](const std::vector<double>& a) {
    if(op=='d') return cppunivariate::distributions::dsn(a[0],a[1],a[2],a[3],log);
    if(op=='p') return cppunivariate::distributions::psn(a[0],a[1],a[2],a[3],lower,log);
    if(op=='q') return cppunivariate::distributions::qsn(a[0],a[1],a[2],a[3],lower,log);
    if(!cppunivariate::sn_detail::valid(a[1],a[2],a[3])) return R_NaN;
    const double den=std::hypot(1.0,a[3]);
    const double u=R::norm_rand(),v=R::norm_rand();
    return a[1]+a[2]*((a[3]/den)*std::abs(u)+v/den);
  });
}
}
// [[Rcpp::export]]
Rcpp::NumericVector dsn_(Rcpp::NumericVector x,Rcpp::NumericVector xi,Rcpp::NumericVector omega,Rcpp::NumericVector alpha,bool log=false) {
  return sn_evaluate({x,xi,omega,alpha},'d',true,log);
}
// [[Rcpp::export]]
Rcpp::NumericVector psn_(Rcpp::NumericVector q,Rcpp::NumericVector xi,Rcpp::NumericVector omega,Rcpp::NumericVector alpha,bool lower_tail=true,bool log_p=false) {
  return sn_evaluate({q,xi,omega,alpha},'p',lower_tail,log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector qsn_(Rcpp::NumericVector p,Rcpp::NumericVector xi,Rcpp::NumericVector omega,Rcpp::NumericVector alpha,bool lower_tail=true,bool log_p=false) {
  return sn_evaluate({p,xi,omega,alpha},'q',lower_tail,log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector rsn_(int n,Rcpp::NumericVector xi,Rcpp::NumericVector omega,Rcpp::NumericVector alpha) {
  return sn_evaluate({Rcpp::NumericVector(n),xi,omega,alpha},'r');
}
