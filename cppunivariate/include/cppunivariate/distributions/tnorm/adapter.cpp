#include "as_span.h"
#include "cppunivariate/distributions/tnorm/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <cstdio>
#include <limits>
#include <string>

namespace {
std::string tnorm_format(double v) {
  char buffer[32];
  std::snprintf(buffer, sizeof buffer, "%.6g", v);
  return buffer;
}
}  // namespace

// [[Rcpp::export]]
Rcpp::List mltnorm_(const Rcpp::NumericVector &x,
                    Rcpp::Nullable<double> a = R_NilValue,
                    Rcpp::Nullable<double> b = R_NilValue,
                    Rcpp::Nullable<double> mean = R_NilValue,
                    Rcpp::Nullable<double> sd = R_NilValue,
                    double reltol = 1e-12, int iterlim = 100) {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  const double lower = a.isNotNull() ? Rcpp::as<double>(a) : -std::numeric_limits<double>::infinity();
  const double upper = b.isNotNull() ? Rcpp::as<double>(b) : std::numeric_limits<double>::infinity();
  const double held_mean = mean.isNotNull() ? Rcpp::as<double>(mean) : nan;
  const double held_sd = sd.isNotNull() ? Rcpp::as<double>(sd) : nan;
  if (mean.isNotNull() && !std::isfinite(held_mean))
    Rcpp::stop("`mean` must be finite.");
  if (sd.isNotNull() && !(std::isfinite(held_sd) && held_sd > 0))
    Rcpp::stop("`sd` must be finite and positive.");
  const auto res = cppunivariate::tnorm_mle(dist_impl::as_span(x), lower, upper, reltol, iterlim,
                                            held_mean, held_sd);
  const std::string rate = tnorm_format(res.limit_rate);
  const std::string decay = tnorm_format(-res.limit_rate);
  switch (res.status) {
    case cppunivariate::TnormStatus::Ok: break;
    case cppunivariate::TnormStatus::InvalidBounds:
      Rcpp::stop("Truncated normal bounds must satisfy a < b, with a < Inf and b > -Inf.");
    case cppunivariate::TnormStatus::OutsideBounds:
      Rcpp::stop("Truncated normal observations must lie strictly between `a` and `b`; extraDistr::dtnorm is zero at the bounds.");
    case cppunivariate::TnormStatus::DegenerateData:
      Rcpp::stop("Truncated normal MLE with free `sd` requires observations that are not all equal (to `mean`, when it is held); the likelihood is unbounded as sd -> 0.");
    case cppunivariate::TnormStatus::ExponentialBoundary:
      Rcpp::stop("The truncated normal fit on (a, Inf) escapes to, or is numerically tied with, a + Y, Y exponential with rate " + decay + ", as sd -> Inf; no finite MLE is returned (variance >= (mean(x) - a)^2, or a likelihood tie within 1e-8 per observation). Use `mlexp(x - a)` to fit the limit.");
    case cppunivariate::TnormStatus::ReflectedExponentialBoundary:
      Rcpp::stop("The truncated normal fit on (-Inf, b) escapes to, or is numerically tied with, b - Y, Y exponential with rate " + rate + ", as sd -> Inf; no finite MLE is returned (variance >= (b - mean(x))^2, or a likelihood tie within 1e-8 per observation). Use `mlexp(b - x)` to fit the limit.");
    case cppunivariate::TnormStatus::TruncatedExponentialBoundary:
      Rcpp::stop("The truncated normal fit on (a, b) escapes to, or is numerically tied with, the truncated exponential distribution on (a, b) with density proportional to exp(" + rate + " * x) (uniform at rate 0) as sd -> Inf; no finite MLE is returned (an escape or a likelihood tie within 1e-8 per observation). The package has no model for this limit.");
    case cppunivariate::TnormStatus::UniformBoundary:
      Rcpp::stop("With `mean` held fixed, the truncated normal fit escapes to, or is numerically tied with, the uniform distribution on (a, b) as sd -> Inf; no finite MLE is returned (mean squared distance is at least the uniform's, or a likelihood tie within 1e-8 per observation).");
    case cppunivariate::TnormStatus::InvalidFixed:
      Rcpp::stop("`mean` must be finite and `sd` finite and positive.");
    case cppunivariate::TnormStatus::IterationFailure:
      Rcpp::stop("Truncated normal Newton iteration did not converge; increase `iterlim`.");
    case cppunivariate::TnormStatus::InvalidInput:
    default:
      Rcpp::stop("Truncated normal MLE requires finite data, positive `reltol` and positive `iterlim`.");
  }
  Rcpp::CharacterVector fixed;
  if (mean.isNotNull()) fixed.push_back("mean");
  if (sd.isNotNull()) fixed.push_back("sd");
  fixed.push_back("a");
  fixed.push_back("b");
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["mean"] = res.mean, Rcpp::_["sd"] = res.sd,
          Rcpp::_["a"] = res.a, Rcpp::_["b"] = res.b),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations,
      Rcpp::_["fixed"] = fixed);
}
