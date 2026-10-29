// R shell: vector policy is shared with the other package-defined families.
#include "as_span.h"
#include "dpqr.h"
#include "cppunivariate/distributions/zeta/mle.hpp"
#include "cppunivariate/distributions/zeta/dpqr.hpp"
// [[Rcpp::export]]
Rcpp::List mlzeta_(const Rcpp::IntegerVector &x,Rcpp::Nullable<double> s=R_NilValue,double reltol=1e-10,int iterlim=200) {
  const double held=s.isNull()?std::numeric_limits<double>::quiet_NaN():Rcpp::as<double>(s);
  if (s.isNotNull() && (!std::isfinite(held) || !(held>1)))
    Rcpp::stop("Invalid held `s`.");
  const auto fit=cppunivariate::zeta_mle(dist_impl::as_ispan(x),reltol,iterlim,held);
  if (fit.status==cppunivariate::ZetaStatus::PointMass)
    Rcpp::stop("The maximum likelihood estimator does not exist: all observations are one (point mass at one).");
  if (fit.status==cppunivariate::ZetaStatus::InvalidInput)
    Rcpp::stop("Fitting requires positive integer observations, positive finite reltol and positive iterlim.");
  if (fit.status!=cppunivariate::ZetaStatus::Ok)
    Rcpp::stop("Maximum likelihood search failed to converge; increase iterlim.");
  return Rcpp::List::create(
    Rcpp::_["estimates"]=Rcpp::NumericVector::create(Rcpp::_["s"]=fit.s),
    Rcpp::_["logLik"]=fit.logLik,Rcpp::_["iterations"]=fit.iterations,
    Rcpp::_["fixed"]=s.isNotNull()?Rcpp::CharacterVector::create("s"):Rcpp::CharacterVector(0));
}
// [[Rcpp::export]]
Rcpp::NumericVector dzeta_(Rcpp::NumericVector x,Rcpp::NumericVector s,bool log=false) {
  return dist_impl::dpqr_apply({x,s},[&](const std::vector<double> &a) {
    return cppunivariate::distributions::dzeta(a[0],a[1],log);
  },true);
}
// [[Rcpp::export]]
Rcpp::NumericVector pzeta_(Rcpp::NumericVector q,Rcpp::NumericVector s,bool lower_tail=true,bool log_p=false) {
  return dist_impl::dpqr_apply({q,s},[&](const std::vector<double> &a) {
    return cppunivariate::distributions::pzeta(a[0],a[1],lower_tail,log_p);
  });
}
// [[Rcpp::export]]
Rcpp::NumericVector qzeta_(Rcpp::NumericVector p,Rcpp::NumericVector s,bool lower_tail=true,bool log_p=false) {
  return dist_impl::dpqr_apply({p,s},[&](const std::vector<double> &a) {
    return cppunivariate::distributions::qzeta(a[0],a[1],lower_tail,log_p);
  });
}
// [[Rcpp::export]]
Rcpp::NumericVector rzeta_(int n,Rcpp::NumericVector s) {
  return dist_impl::dpqr_apply({Rcpp::NumericVector(n),s},[&](const std::vector<double> &a) {
    return cppunivariate::distributions::qzeta(R::unif_rand(),a[1]);
  });
}
