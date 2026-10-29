// Rcpp adapter; copied into the assembled R package.

#include "as_span.h"
#include "cppunivariate/distributions/logitnorm/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mllogitnorm_(const Rcpp::NumericVector &x,
    Rcpp::Nullable<double> mu = R_NilValue,
    Rcpp::Nullable<double> sigma = R_NilValue) {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  auto held = [&](Rcpp::Nullable<double> z, bool positive) {
    if (z.isNull()) return nan;
    const double v = Rcpp::as<double>(z);
    if (!std::isfinite(v) || (positive && !(v > 0)))
      Rcpp::stop("Held parameters must be finite and shapes/scales positive.");
    return v;
  };
  Rcpp::CharacterVector fixed(0);
  if (mu.isNotNull()) fixed.push_back("mu");
  if (sigma.isNotNull()) fixed.push_back("sigma");
  auto res = cppunivariate::logitnorm_mle(
      cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, held(mu, false), held(sigma, true));
  if (!(res.sigma > 0.0) || !std::isfinite(res.sigma)) {
    Rcpp::stop("The logitnorm maximum likelihood estimator does not exist for constant data: the likelihood is unbounded as scale shrinks to zero.");
  }
  if (!std::isfinite(res.logLik))
    Rcpp::stop("Logit-normal MLE failed: likelihood cannot be resolved in double precision.");
  return Rcpp::List::create(
      Rcpp::_["estimates"] =
          Rcpp::NumericVector::create(Rcpp::_["mu"] = res.mu,
                                      Rcpp::_["sigma"] = res.sigma),
      Rcpp::_["fixed"] = fixed, Rcpp::_["logLik"] = res.logLik);
}
