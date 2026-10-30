// Rcpp adapter; assembled into the R package, outside the header-only core.
// Counts arrive as doubles: discrete Weibull samples can exceed the
// integer range (still exact up to 2^53).
#include "as_span.h"
#include "cppunivariate/distributions/dweibull/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <limits>
#include <string>

// [[Rcpp::export]]
Rcpp::List mldweibull_(const Rcpp::NumericVector &x,
                       Rcpp::Nullable<double> shape1 = R_NilValue,
                       Rcpp::Nullable<double> shape2 = R_NilValue,
                       double reltol = 1e-12, int iterlim = 200) {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  const double q = shape1.isNotNull() ? Rcpp::as<double>(shape1) : nan;
  const double b = shape2.isNotNull() ? Rcpp::as<double>(shape2) : nan;
  if (shape1.isNotNull() && !(q > 0 && q < 1))
    Rcpp::stop("`shape1` must lie strictly between 0 and 1.");
  if (shape2.isNotNull() && !(b > 0 && std::isfinite(b)))
    Rcpp::stop("`shape2` must be finite and positive.");
  const auto res = cppunivariate::dweibull_mle(dist_impl::as_span(x), reltol, iterlim, q, b);
  switch (res.status) {
    case cppunivariate::DweibullStatus::Ok:
    case cppunivariate::DweibullStatus::ShapeUnidentified:
      break;
    case cppunivariate::DweibullStatus::InvalidData:
      Rcpp::stop("Discrete Weibull requires nonempty, finite, nonnegative integer data.");
    case cppunivariate::DweibullStatus::InvalidControl:
      Rcpp::stop("`reltol` must be positive and `iterlim` at least 1.");
    case cppunivariate::DweibullStatus::PointMass:
      if (shape2.isNotNull())
        Rcpp::stop("All observations are 0: with `shape2` held, the likelihood increases "
                   "toward the point mass at 0 as `shape1` -> 0, and the maximum "
                   "likelihood estimator does not exist.");
      Rcpp::stop("All observations equal %.0f: the likelihood increases toward the point "
                 "mass at %.0f, and the maximum likelihood estimator does not exist.",
                 x[0], x[0]);
    case cppunivariate::DweibullStatus::BernoulliLimit:
      Rcpp::stop("The observations are 0 and 1 only: the likelihood increases toward a "
                 "Bernoulli distribution (binomial with size 1) as `shape2` -> Inf, and "
                 "the maximum likelihood estimator does not exist. Use "
                 "`mlbinom(x, size = 1)` instead.");
    case cppunivariate::DweibullStatus::TwoPointLimit: {
      const double lo = Rcpp::min(x);
      Rcpp::stop("The observations take the adjacent values %.0f and %.0f only: the "
                 "likelihood increases toward the two-point distribution on these values "
                 "as `shape2` -> Inf, and the maximum likelihood estimator does not exist.",
                 lo, lo + 1);
    }
    case cppunivariate::DweibullStatus::HeldBernoulliLimit:
      Rcpp::stop("The observations are at most 1: with `shape1` held, the likelihood "
                 "increases toward the Bernoulli distribution with success probability "
                 "`shape1` as `shape2` -> Inf, and the maximum likelihood estimator does "
                 "not exist.");
    case cppunivariate::DweibullStatus::Unrepresentable:
      if (res.shape1 == 0)
        Rcpp::stop("The fitted `shape1` rounds to 0 in double precision and cannot be returned.");
      Rcpp::stop("The fitted `shape1` rounds to 1 in double precision (the latent Weibull "
                 "scale to the power `shape2` exceeds 2^53), so the fit cannot be expressed "
                 "in extraDistr's (shape1, shape2) parametrization.");
    case cppunivariate::DweibullStatus::IterationFailure:
      Rcpp::stop("Discrete Weibull MLE failed to converge; increase `iterlim`.");
  }
  if (res.status == cppunivariate::DweibullStatus::ShapeUnidentified)
    Rcpp::warning("All observations are 0 and `shape1` is held: the likelihood does not "
                  "depend on `shape2`, so the maximum likelihood estimator is not unique; "
                  "returning `shape2 = 1`.");
  Rcpp::CharacterVector fixed(0);
  if (shape1.isNotNull()) fixed.push_back("shape1");
  if (shape2.isNotNull()) fixed.push_back("shape2");
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["shape1"] = res.shape1, Rcpp::_["shape2"] = res.shape2),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations,
      Rcpp::_["fixed"] = fixed);
}
