// R shell: vector policy is shared with the other package-defined families.
#include "as_span.h"
#include "dpqr.h"
#include "cppunivariate/distributions/yulesimon/mle.hpp"
#include "cppunivariate/distributions/yulesimon/dpqr.hpp"
// [[Rcpp::export]]
Rcpp::List mlyulesimon_(const Rcpp::IntegerVector &x,Rcpp::Nullable<double> rho=R_NilValue,double reltol=1e-10,int iterlim=200) {
  const double held=rho.isNull()?std::numeric_limits<double>::quiet_NaN():Rcpp::as<double>(rho);
  if (rho.isNotNull() && (!std::isfinite(held) || !(held>0)))
    Rcpp::stop("Invalid held `rho`.");
  const auto fit=cppunivariate::yulesimon_mle(dist_impl::as_ispan(x),reltol,iterlim,held);
  if (fit.status==cppunivariate::YulesimonStatus::PointMass)
    Rcpp::stop("The maximum likelihood estimator does not exist: all observations are one (point mass at one).");
  if (fit.status==cppunivariate::YulesimonStatus::InvalidInput)
    Rcpp::stop("Fitting requires positive integer observations, positive finite reltol and positive iterlim.");
  if (fit.status!=cppunivariate::YulesimonStatus::Ok)
    Rcpp::stop("Maximum likelihood search failed to converge; increase iterlim.");
  return Rcpp::List::create(
    Rcpp::_["estimates"]=Rcpp::NumericVector::create(Rcpp::_["rho"]=fit.rho),
    Rcpp::_["logLik"]=fit.logLik,Rcpp::_["iterations"]=fit.iterations,
    Rcpp::_["fixed"]=rho.isNotNull()?Rcpp::CharacterVector::create("rho"):Rcpp::CharacterVector(0));
}
// [[Rcpp::export]]
Rcpp::NumericVector dyulesimon_(Rcpp::NumericVector x,Rcpp::NumericVector rho,bool log=false) {
  return dist_impl::dpqr_apply({x,rho},[&](const std::vector<double> &a) {
    return cppunivariate::distributions::dyulesimon(a[0],a[1],log);
  },true);
}
// [[Rcpp::export]]
Rcpp::NumericVector pyulesimon_(Rcpp::NumericVector q,Rcpp::NumericVector rho,bool lower_tail=true,bool log_p=false) {
  return dist_impl::dpqr_apply({q,rho},[&](const std::vector<double> &a) {
    return cppunivariate::distributions::pyulesimon(a[0],a[1],lower_tail,log_p);
  });
}
// [[Rcpp::export]]
Rcpp::NumericVector qyulesimon_(Rcpp::NumericVector p,Rcpp::NumericVector rho,bool lower_tail=true,bool log_p=false) {
  return dist_impl::dpqr_apply({p,rho},[&](const std::vector<double> &a) {
    return cppunivariate::distributions::qyulesimon(a[0],a[1],lower_tail,log_p);
  });
}
// [[Rcpp::export]]
Rcpp::NumericVector ryulesimon_(int n,Rcpp::NumericVector rho) {
  return dist_impl::dpqr_apply({Rcpp::NumericVector(n),rho},[&](const std::vector<double> &a) {
    return cppunivariate::distributions::qyulesimon(R::unif_rand(),a[1]);
  });
}
