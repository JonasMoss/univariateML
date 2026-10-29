#include "as_span.h"
#include "cppunivariate/distributions/gpd/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlgpd_(const Rcpp::NumericVector &x, Rcpp::Nullable<double> mu,
                  Rcpp::Nullable<double> xi = R_NilValue,
                  double reltol = 1e-12, int iterlim = 200) {
  if (mu.isNull()) Rcpp::stop("Estimating `mu` is not supported: the free-threshold GPD likelihood has no MLE (endpoint and logistic escapes). Supply a held threshold.");
  const double threshold = Rcpp::as<double>(mu);
  const double shape = xi.isNotNull() ? Rcpp::as<double>(xi) :
      std::numeric_limits<double>::quiet_NaN();
  if (xi.isNotNull() && (!std::isfinite(shape) || shape < -0.5))
    Rcpp::stop("`xi` must be finite and at least -0.5.");
  auto res = cppunivariate::gpd_mle(dist_impl::as_span(x), reltol, iterlim, threshold, shape);
  if (res.status == cppunivariate::GpdStatus::InvalidData)
    Rcpp::stop("Generalized Pareto requires finite data strictly above `mu`, a finite threshold, positive `reltol` and positive `iterlim`.");
  if (res.status != cppunivariate::GpdStatus::Ok || !std::isfinite(res.logLik))
    Rcpp::stop("Generalized Pareto MLE failed to converge.");
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["mu"] = res.mu, Rcpp::_["sigma"] = res.sigma, Rcpp::_["xi"] = res.xi),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations,
      Rcpp::_["fixed"] = xi.isNotNull() ? Rcpp::CharacterVector::create("mu", "xi") :
          Rcpp::CharacterVector::create("mu"));
}
