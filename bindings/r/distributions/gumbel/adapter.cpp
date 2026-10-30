// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/gumbel/mle.hpp"
#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>

// [[Rcpp::export]]
Rcpp::List mlgumbel_(const Rcpp::NumericVector &x, double reltol = 1.220703e-4,
                     int iterlim = 100, Rcpp::Nullable<double> mu = R_NilValue, Rcpp::Nullable<double> sigma = R_NilValue) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double held_mu=mu.isNotNull()?Rcpp::as<double>(mu):nan;
  if(mu.isNotNull() && (!std::isfinite(held_mu)))Rcpp::stop("`mu` must be finite.");
  const double held_sigma=sigma.isNotNull()?Rcpp::as<double>(sigma):nan;
  if(sigma.isNotNull() && (!std::isfinite(held_sigma) || !(held_sigma>0)))Rcpp::stop("`sigma` must be positive and finite.");
  Rcpp::CharacterVector fixed;
  if(mu.isNotNull())fixed.push_back("mu");
  if(sigma.isNotNull())fixed.push_back("sigma");

  cppunivariate::GumbelResult res = cppunivariate::fit_gumbel_held(dist_impl::as_span(x),reltol,iterlim,held_mu,held_sigma);
  if (res.status == cppunivariate::GumbelStatus::IterationFailure)
    Rcpp::stop("Gumbel optimizer IterationFailure; increase iterlim.");
  if (!std::isfinite(res.mu) || !std::isfinite(res.sigma) || !std::isfinite(res.logLik)) {
    if (std::all_of(x.begin(), x.end(), [&](double v) { return v == x[0]; }))
      Rcpp::stop("The Gumbel maximum likelihood estimator does not exist for constant data: the likelihood is unbounded.");
    Rcpp::stop("Gumbel MLE failed");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["mu"] = res.mu, Rcpp::_["sigma"] = res.sigma),
      Rcpp::_["fixed"] = fixed, Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}
