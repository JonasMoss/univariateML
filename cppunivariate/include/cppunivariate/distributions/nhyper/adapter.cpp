#include "as_span.h"
#include "cppunivariate/distributions/nhyper/mle.hpp"
#include <Rcpp.h>
#include <limits>
#include <cmath>

// [[Rcpp::export]]
Rcpp::List mlnhyper_(const Rcpp::NumericVector &x, Rcpp::Nullable<double> r,
                     Rcpp::Nullable<double> N,
                     Rcpp::Nullable<double> m = R_NilValue) {
  if (N.isNull() && m.isNull()) Rcpp::stop("Joint estimation of population and composition can escape to the growing-urn limit; use mlnbinom(x - r, size = r).");
  if (N.isNull()) Rcpp::stop("Estimating N with m held is not supported by this fitter.");
  if (r.isNull() && m.isNull()) Rcpp::stop("Estimating r jointly with m is not supported by this fitter.");
  const double design=r.isNull() ? std::numeric_limits<double>::quiet_NaN() : Rcpp::as<double>(r);
  if (r.isNotNull() && std::isnan(design)) Rcpp::stop("`r` must be a whole number or NULL, not NA.");
  const double held = m.isNull() ? std::numeric_limits<double>::quiet_NaN() : Rcpp::as<double>(m);
  if (m.isNotNull() && std::isnan(held)) Rcpp::stop("`m` must be a whole number or NULL, not NA.");
  const auto fit = cppunivariate::nhyper_mle(dist_impl::as_span(x), design, Rcpp::as<double>(N), held);
  switch (fit.status) {
    case cppunivariate::NhyperStatus::InvalidInput:
      Rcpp::stop("Counts must be whole numbers from 0 to 2^53 - 1, with r <= m <= N.");
    case cppunivariate::NhyperStatus::Infeasible:
      Rcpp::stop("No admissible composition gives every observation positive probability; total draws must lie from r through N - m + r (zero when r = 0).");
    case cppunivariate::NhyperStatus::Tie:
      Rcpp::warning("Likelihood tied between adjacent counts; returning the smaller."); break;
    case cppunivariate::NhyperStatus::Unidentified:
      Rcpp::warning("With r = 0 every composition is unidentified; returning m = 0."); break;
    case cppunivariate::NhyperStatus::Ok: break;
  }
  Rcpp::CharacterVector fixed;
  if (m.isNotNull()) fixed.push_back("m");
  if (N.isNotNull()) fixed.push_back("N");
  if (r.isNotNull()) fixed.push_back("r");
  return Rcpp::List::create(
    Rcpp::_["estimates"] = Rcpp::NumericVector::create(Rcpp::_["m"] = fit.m, Rcpp::_["N"] = fit.N, Rcpp::_["r"] = fit.r),
    Rcpp::_["logLik"] = fit.logLik, Rcpp::_["iterations"] = fit.iterations,
    Rcpp::_["fixed"] = fixed);
}
