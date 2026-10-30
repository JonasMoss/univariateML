// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/logis/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <limits>
#include <vector>

// [[Rcpp::export]]
Rcpp::List mllogis_(const Rcpp::NumericVector &x, double abstol = 1.220703e-4,
                    int iterlim = 100, Rcpp::Nullable<double> location = R_NilValue, Rcpp::Nullable<double> scale = R_NilValue) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double held_location=location.isNotNull()?Rcpp::as<double>(location):nan;
  if(location.isNotNull() && (!std::isfinite(held_location)))Rcpp::stop("`location` must be finite.");
  const double held_scale=scale.isNotNull()?Rcpp::as<double>(scale):nan;
  if(scale.isNotNull() && (!std::isfinite(held_scale) || !(held_scale>0)))Rcpp::stop("`scale` must be positive and finite.");
  Rcpp::CharacterVector fixed;
  if(location.isNotNull())fixed.push_back("location");
  if(scale.isNotNull())fixed.push_back("scale");

  if(location.isNotNull() && scale.isNull() && std::all_of(x.begin(),x.end(),[&](double v){return v==held_location;}))
    Rcpp::stop("Logistic scale likelihood is unbounded: all observations equal the held location.");
  cppunivariate::LogisticResult res =
      cppunivariate::logistic_mle(dist_impl::as_span(x),abstol,iterlim,held_location,held_scale);
  if (!std::isfinite(res.mu) || !std::isfinite(res.scale) || !(res.scale > 0.0))
    Rcpp::stop("Logistic MLE failed: non-finite or invalid parameters");
  if(res.status==cppunivariate::LogisticStatus::IterationFailure)Rcpp::stop("Conditional fit did not converge; increase iterlim.");
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["location"] = res.mu, Rcpp::_["scale"] = res.scale),
      Rcpp::_["fixed"] = fixed, Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}
