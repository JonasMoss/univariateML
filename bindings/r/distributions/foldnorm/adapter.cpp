#include <Rcpp.h>
#include "as_span.h"
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
