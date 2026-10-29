// Rcpp adapter; assembled into the R package, outside the header-only core.
#include "as_span.h"
#include "cppunivariate/distributions/bbinom/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <limits>
#include <string>

// [[Rcpp::export]]
Rcpp::List mlbbinom_(const Rcpp::IntegerVector &x, double size,
                     Rcpp::Nullable<double> alpha = R_NilValue,
                     Rcpp::Nullable<double> beta = R_NilValue, double reltol = 1e-10,
                     int iterlim = 100) {
  const double N = size;
  if (!std::isfinite(N) || N < 0 || N != std::floor(N) || N > 1e7)
    Rcpp::stop("`size` must be a nonnegative integer (at most 1e7).");
  if (alpha.isNotNull() && beta.isNotNull())
    Rcpp::stop("Hold at most one of `alpha` and `beta` fixed.");
  const double nan = std::numeric_limits<double>::quiet_NaN();
  const double a = alpha.isNotNull() ? Rcpp::as<double>(alpha) : nan;
  const double b = beta.isNotNull() ? Rcpp::as<double>(beta) : nan;
  if (alpha.isNotNull() && !(std::isfinite(a) && a > 0))
    Rcpp::stop("`alpha` must be a finite positive number.");
  if (beta.isNotNull() && !(std::isfinite(b) && b > 0))
    Rcpp::stop("`beta` must be a finite positive number.");
  const auto res = cppunivariate::bbinom_mle(dist_impl::as_ispan(x), N, reltol, iterlim, a, b);
  const std::string n_txt = std::to_string(static_cast<long long>(N));
  switch (res.status) {
    case cppunivariate::BbinomStatus::Ok:
      break;
    case cppunivariate::BbinomStatus::SizeTooSmall:
      Rcpp::stop("`size` is smaller than `max(x)`.");
    case cppunivariate::BbinomStatus::BinomialLimit:
      Rcpp::stop("The maximum likelihood estimator does not exist: the likelihood increases "
                 "toward the binomial distribution as `alpha + beta` grows (the data are not "
                 "overdispersed enough). Use `mlbinom(x, size = " + n_txt + ")` instead.");
    case cppunivariate::BbinomStatus::TwoPointLimit:
      Rcpp::stop("The maximum likelihood estimator does not exist: every observation is 0 or "
                 "`size`, and the likelihood increases toward the two-point distribution on "
                 "{0, size} as `alpha` and `beta` tend to 0 with `alpha / (alpha + beta)` "
                 "fixed. Its probability of `size` is `mean(x == size)`, the fit of "
                 "`mlbinom(x / " + n_txt + ", size = 1)`.");
    case cppunivariate::BbinomStatus::PointMass:
      Rcpp::stop("The maximum likelihood estimator does not exist: all observations equal 0 "
                 "or all equal `size`, and the likelihood increases toward a point mass. "
                 "Use `mlbinom(x, size = " + n_txt + ")` instead.");
    case cppunivariate::BbinomStatus::Unidentified:
      if (N == 0) Rcpp::stop("With `size = 0` the beta-binomial parameters are not identified.");
      Rcpp::stop("With `size = 1` the beta-binomial is a Bernoulli distribution and "
                 "`alpha + beta` is not identified. Use `mlbinom(x, size = 1)`, or hold "
                 "`alpha` or `beta` fixed.");
    case cppunivariate::BbinomStatus::InvalidInput:
      Rcpp::stop("Beta-binomial fitting needs nonempty nonnegative counts, positive "
                 "`reltol` and positive `iterlim`.");
    default:
      Rcpp::stop("Beta-binomial MLE failed.");
  }
  if (!std::isfinite(res.alpha) || !std::isfinite(res.beta) || !std::isfinite(res.logLik))
    Rcpp::stop("Beta-binomial MLE failed.");
  Rcpp::CharacterVector fixed = Rcpp::CharacterVector::create("size");
  if (alpha.isNotNull()) fixed.push_back("alpha");
  if (beta.isNotNull()) fixed.push_back("beta");
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["size"] = N, Rcpp::_["alpha"] = res.alpha, Rcpp::_["beta"] = res.beta),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations,
      Rcpp::_["fixed"] = fixed);
}
