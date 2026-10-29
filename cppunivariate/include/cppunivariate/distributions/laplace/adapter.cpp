// Rcpp adapter; copied into the assembled R package.

#include "as_span.h"
#include "cppunivariate/distributions/laplace/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mllaplace_(const Rcpp::NumericVector &x, Rcpp::Nullable<double> mu = R_NilValue, Rcpp::Nullable<double> sigma = R_NilValue) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double held_mu=mu.isNotNull()?Rcpp::as<double>(mu):nan;
  if(mu.isNotNull() && (!std::isfinite(held_mu)))Rcpp::stop("`mu` must be finite.");
  const double held_sigma=sigma.isNotNull()?Rcpp::as<double>(sigma):nan;
  if(sigma.isNotNull() && (!std::isfinite(held_sigma) || !(held_sigma>0)))Rcpp::stop("`sigma` must be positive and finite.");
  Rcpp::CharacterVector fixed;
  if(mu.isNotNull())fixed.push_back("mu");
  if(sigma.isNotNull())fixed.push_back("sigma");

  auto res = cppunivariate::laplace_mle(dist_impl::as_span(x),held_mu,held_sigma);
  if (!(res.sigma>0.0) || !std::isfinite(res.sigma))
    Rcpp::stop("The Laplace maximum likelihood estimator does not exist for constant data; other data may exceed double precision.");
  return Rcpp::List::create(Rcpp::_["estimates"] = Rcpp::NumericVector::create(
                                Rcpp::_["mu"] = res.mu,
                                Rcpp::_["sigma"] = res.sigma),
                            Rcpp::_["fixed"] = fixed, Rcpp::_["logLik"] = res.logLik);
}
