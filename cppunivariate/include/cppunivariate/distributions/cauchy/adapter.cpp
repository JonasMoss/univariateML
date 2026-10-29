// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/cauchy/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <limits>
#include <vector>

// [[Rcpp::export]]
Rcpp::List mlcauchy_(const Rcpp::NumericVector &x, double abstol = 1.220703e-4,
                     int iterlim = 100, Rcpp::Nullable<double> location = R_NilValue, Rcpp::Nullable<double> scale = R_NilValue) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double held_location=location.isNotNull()?Rcpp::as<double>(location):nan;
  if(location.isNotNull() && (!std::isfinite(held_location)))Rcpp::stop("`location` must be finite.");
  const double held_scale=scale.isNotNull()?Rcpp::as<double>(scale):nan;
  if(scale.isNotNull() && (!std::isfinite(held_scale) || !(held_scale>0)))Rcpp::stop("`scale` must be positive and finite.");
  Rcpp::CharacterVector fixed;
  if(location.isNotNull())fixed.push_back("location");
  if(scale.isNotNull())fixed.push_back("scale");

  cppunivariate::CauchyResult res = cppunivariate::fit_cauchy(dist_impl::as_span(x),abstol,iterlim,held_location,held_scale);
  if(res.status==cppunivariate::CauchyStatus::NoMLE && location.isNotNull())
    Rcpp::stop("With location held the Cauchy scale MLE does not exist when at least half the observations equal that location.");
  if (res.status == cppunivariate::CauchyStatus::NoMLE)
    Rcpp::stop("Cauchy MLE does not exist: a sample atom has at least half the mass, except for balanced two-point samples");
  if (!std::isfinite(res.mu) || !std::isfinite(res.scale) || !(res.scale > 0.0))
    Rcpp::stop("Cauchy MLE failed: non-finite or invalid parameters");
  if(res.status==cppunivariate::CauchyStatus::IterationFailure)Rcpp::stop("Conditional fit did not converge; increase iterlim.");
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["location"] = res.mu, Rcpp::_["scale"] = res.scale),
      Rcpp::_["fixed"] = fixed, Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}
