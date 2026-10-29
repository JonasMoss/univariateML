// Rcpp adapter; copied into the assembled R package.

#include "as_span.h"
#include "cppunivariate/distributions/power/mle.hpp"
#include <Rcpp.h>
#include <limits>
#include <cmath>
#include <algorithm>

// [[Rcpp::export]]
Rcpp::List mlpower_(const Rcpp::NumericVector &x, double epsilon = 1.490116e-08,
    Rcpp::Nullable<double> alpha = R_NilValue,
    Rcpp::Nullable<double> beta = R_NilValue) {
  Rcpp::CharacterVector fixed(0);
  const double alpha_held = alpha.isNull() ? std::numeric_limits<double>::quiet_NaN() : Rcpp::as<double>(alpha);
  if (alpha.isNotNull()) {
    if (!(std::isfinite(alpha_held) && alpha_held > 0.0)) Rcpp::stop("Held `alpha` must be positive and finite.");
    fixed.push_back("alpha");
  }
  const double beta_held = beta.isNull() ? std::numeric_limits<double>::quiet_NaN() : Rcpp::as<double>(beta);
  if (beta.isNotNull()) {
    if (!(std::isfinite(beta_held) && beta_held > 0.0)) Rcpp::stop("Held `beta` must be positive and finite.");
    fixed.push_back("beta");
  }
  if (alpha.isNull() && beta.isNotNull()) Rcpp::stop("The power MLE with beta held does not exist: the reference excludes the maximizing upper endpoint (P3). ");
  if (alpha.isNotNull() && std::any_of(x.begin(),x.end(),[&](double v){return v>=alpha_held;})) Rcpp::stop("Held alpha must exceed the sample maximum: zero likelihood.");

  if (!(epsilon>=0.0) || !std::isfinite(epsilon))
    Rcpp::stop("`epsilon` must be nonnegative and finite.");
  auto res = cppunivariate::power_mle(
      cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, epsilon, alpha_held, beta_held);
  if (!std::isfinite(res.alpha) || !std::isfinite(res.beta) || !std::isfinite(res.logLik)) {
    Rcpp::stop("The power distribution is only defined for positive numbers.");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["alpha"] = res.alpha, Rcpp::_["beta"] = res.beta),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations, Rcpp::_["fixed"] = fixed);
}
