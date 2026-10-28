// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/betapr/mle.hpp"
#include <Rcpp.h>
#include <cmath>
// [[Rcpp::export]]
Rcpp::List mlbetapr_(const Rcpp::NumericVector &x, double gradtol = 1e-6,
                     int iterlim = 1000, double shape10 = 0, double shape20 = 0,
                     double scale0 = 0, double reltol = 1e-13, double abstol = 1e-10,
                     Rcpp::Nullable<double> scale = R_NilValue) {
  const double held = scale.isNotNull() ? Rcpp::as<double>(scale) : 1.0;
  if (!(held > 0) || !std::isfinite(held))
    Rcpp::stop("`scale` must be a positive finite number.");
  auto res = scale.isNotNull()
      ? cppunivariate::betapr_mle_fixed_scale(dist_impl::as_span(x), held, gradtol, iterlim)
      : cppunivariate::betapr_mle(dist_impl::as_span(x), gradtol, iterlim,
                                      shape10, shape20, scale0, reltol, abstol);
  if (res.status == cppunivariate::BetaPrimeStatus::GammaBoundary)
    Rcpp::stop("The beta-prime likelihood selects the gamma limit. Use `mlgamma` to fit this model.");
  if (res.status == cppunivariate::BetaPrimeStatus::InvGammaBoundary)
    Rcpp::stop("The beta-prime likelihood selects the inverse-gamma limit. Use `mlinvgamma` to fit this model.");
  if (res.status == cppunivariate::BetaPrimeStatus::InvalidStart ||
      !std::isfinite(res.alpha) || !std::isfinite(res.beta) || !std::isfinite(res.scale))
    Rcpp::stop("Beta-prime MLE requires positive finite observations with positive log-variance.");
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["shape1"] = res.alpha, Rcpp::_["shape2"] = res.beta,
          Rcpp::_["scale"] = res.scale),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations,
      Rcpp::_["fixed"] = scale.isNotNull() ? Rcpp::CharacterVector::create("scale")
                                           : Rcpp::CharacterVector(0));
}
