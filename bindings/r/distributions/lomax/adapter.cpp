// Rcpp adapter for mllomax(). Copied into the R package's src/ by
// bindings/r/scripts/assemble_package.R; not part of the C++ library.

#include "as_span.h"
#include "cppunivariate/distributions/lomax/mle.hpp"

#include <Rcpp.h>
#include <limits>

#include <cmath>

// [[Rcpp::export]]
Rcpp::List mllomax_(const Rcpp::NumericVector &x,
                    Rcpp::Nullable<double> lambda0 = R_NilValue,
                    double reltol = 1.220703e-4, int iterlim = 100,
    Rcpp::Nullable<double> lambda = R_NilValue,
    Rcpp::Nullable<double> kappa = R_NilValue) {
  Rcpp::CharacterVector fixed(0);
  const double lambda_held = lambda.isNull() ? std::numeric_limits<double>::quiet_NaN() : Rcpp::as<double>(lambda);
  if (lambda.isNotNull()) {
    if (!(std::isfinite(lambda_held) && lambda_held > 0.0)) Rcpp::stop("Held `lambda` must be positive and finite.");
    fixed.push_back("lambda");
  }
  const double kappa_held = kappa.isNull() ? std::numeric_limits<double>::quiet_NaN() : Rcpp::as<double>(kappa);
  if (kappa.isNotNull()) {
    if (!(std::isfinite(kappa_held) && kappa_held > 0.0)) Rcpp::stop("Held `kappa` must be positive and finite.");
    fixed.push_back("kappa");
  }

  double start = std::numeric_limits<double>::quiet_NaN();
  if (lambda0.isNotNull()) {
    start = Rcpp::as<double>(lambda0);
    if (!(start > 0.0) || !std::isfinite(start)) {
      Rcpp::stop("`lambda0` must be a positive finite number.");
    }
  }
  auto res = cppunivariate::lomax_mle(dist_impl::as_span(x), reltol, iterlim, start, lambda_held, kappa_held);
  if (res.status == cppunivariate::LomaxStatus::ExponentialLimit) {
    Rcpp::stop("The Lomax maximum likelihood estimator does not exist for data "
               "whose likelihood is maximized on the exponential face. Use `mlexp` to fit an "
               "exponential distribution.");
  }
  if (res.status != cppunivariate::LomaxStatus::Ok || !std::isfinite(res.lambda) ||
      !std::isfinite(res.kappa)) {
    Rcpp::stop("Lomax MLE failed to converge (check dispersion/inputs)");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["lambda"] = res.lambda, Rcpp::_["kappa"] = res.kappa),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations, Rcpp::_["fixed"] = fixed);
}
