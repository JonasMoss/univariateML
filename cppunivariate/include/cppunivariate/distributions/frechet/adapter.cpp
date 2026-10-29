#include "as_span.h"
#include "cppunivariate/distributions/frechet/mle.hpp"
#include <Rcpp.h>
// [[Rcpp::export]]
Rcpp::List mlfrechet_(const Rcpp::NumericVector &x,
 Rcpp::Nullable<double> mu, double reltol=1e-10, int iterlim=100) {
 if(mu.isNotNull() && !std::isfinite(Rcpp::as<double>(mu))) Rcpp::stop("mu must be finite.");
 const auto r=cppunivariate::frechet_mle(dist_impl::as_span(x),reltol,iterlim,
 mu.isNotNull() ? Rcpp::as<double>(mu) : std::numeric_limits<double>::quiet_NaN());
 if(r.status==cppunivariate::FrechetStatus::GumbelLimit) Rcpp::stop("Frechet selects the Gumbel limit. Use `mlgumbel`.");
 if(r.status==cppunivariate::FrechetStatus::InvalidData) Rcpp::stop("Frechet requires finite nonconstant observations strictly above held mu; free mu requires at least three observations and minimum tie multiplicity strictly less than half the sample size. Tolerance and iteration limit must be positive.");
 if(r.status!=cppunivariate::FrechetStatus::Ok) Rcpp::stop("Frechet profile search failed to converge.");
 return Rcpp::List::create(Rcpp::_["estimates"]=Rcpp::NumericVector::create(
 Rcpp::_["lambda"]=r.lambda,Rcpp::_["mu"]=r.mu,Rcpp::_["sigma"]=r.sigma),
 Rcpp::_["logLik"]=r.logLik,Rcpp::_["iterations"]=r.iterations,
 Rcpp::_["fixed"]=mu.isNotNull() ? Rcpp::CharacterVector::create("mu") : Rcpp::CharacterVector());
}
