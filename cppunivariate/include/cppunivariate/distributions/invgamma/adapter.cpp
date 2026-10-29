// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/invgamma/mle.hpp"
#include <Rcpp.h>
#include <limits>
#include <cmath>
#include <algorithm>
#include <vector>

// [[Rcpp::export]]
Rcpp::List mlinvgamma_(const Rcpp::NumericVector &x, double abstol = 1.220703e-4,
                       int iterlim = 100,
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

  auto res = cppunivariate::invgamma_mle(dist_impl::as_span(x), abstol, iterlim, alpha_held, beta_held);
  if (res.status == cppunivariate::GammaStatus::Degenerate) {
    if (!std::all_of(x.begin(), x.end(), [&](double v) { return v == x[0]; }))
      Rcpp::stop("invgamma MLE failed: transformed observations exceed double precision.");
    Rcpp::stop("The inverse gamma maximum likelihood estimator does not exist for constant "
               "data: the likelihood is unbounded as the shape grows.");
  }
  if (!std::isfinite(res.shape) || !std::isfinite(res.scale) || !std::isfinite(res.logLik)) {
    Rcpp::stop("Inverse gamma MLE failed");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["alpha"] = res.shape, Rcpp::_["beta"] = res.scale),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations, Rcpp::_["fixed"] = fixed);
}
