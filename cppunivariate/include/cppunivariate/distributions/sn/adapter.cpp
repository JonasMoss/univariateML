#include <Rcpp.h>
#include "as_span.h"
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
