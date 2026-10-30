#include "as_span.h"
#include "cppunivariate/distributions/gpd/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlgpd_(const Rcpp::NumericVector &x, Rcpp::Nullable<double> mu,
                  Rcpp::Nullable<double> xi = R_NilValue,
                  Rcpp::Nullable<double> sigma = R_NilValue,
                  double reltol = 1e-12, int iterlim = 200) {
  if (mu.isNull()) Rcpp::stop("Estimating `mu` is not supported: the free-threshold GPD likelihood has no MLE (endpoint and logistic escapes). Supply a held threshold.");
  const double threshold = Rcpp::as<double>(mu);
  const double shape = xi.isNotNull() ? Rcpp::as<double>(xi) :
      std::numeric_limits<double>::quiet_NaN();
  if (xi.isNotNull() && (!std::isfinite(shape) || (shape < -1 && sigma.isNull())))
    Rcpp::stop("`xi` must be finite and at least -1.");
  const double scale=sigma.isNotNull() ? Rcpp::as<double>(sigma) : std::numeric_limits<double>::quiet_NaN();
  if(sigma.isNotNull() && (!(scale>0) || !std::isfinite(scale))) Rcpp::stop("`sigma` must be positive and finite.");
  Rcpp::CharacterVector fixed=Rcpp::CharacterVector::create("mu");
  if(sigma.isNotNull()) fixed.push_back("sigma");
  if(xi.isNotNull()) fixed.push_back("xi");
  auto res = cppunivariate::gpd_mle(dist_impl::as_span(x), reltol, iterlim, threshold, shape, scale);
  if (res.status == cppunivariate::GpdStatus::InvalidData)
    Rcpp::stop("Generalized Pareto requires finite data at or above `mu`, compatible support and holds, no scale collapse at the threshold, a finite threshold, positive `reltol` and positive `iterlim`.");
  if (res.status != cppunivariate::GpdStatus::Ok || !std::isfinite(res.logLik))
    Rcpp::stop("Generalized Pareto MLE failed to converge.");
  Rcpp::CharacterVector restricted;
  if(res.restricted & 1u) restricted.push_back("lower","mu");
  if(res.restricted & 2u) restricted.push_back("upper","mu");
  if(res.restricted & 4u) restricted.push_back("lower","sigma");
  if(res.restricted & 8u) restricted.push_back("upper","sigma");
  if(res.restricted & 16u) restricted.push_back("lower","xi");
  if(res.restricted & 32u) restricted.push_back("upper","xi");
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["mu"] = res.mu, Rcpp::_["sigma"] = res.sigma, Rcpp::_["xi"] = res.xi),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations,
      Rcpp::_["restricted"]=restricted, Rcpp::_["fixed"] = fixed);
}
