// Rcpp adapter for mlinvtrgamma(). Copied into the R package's src/ by
// univariateML/scripts/assemble_package.R; not part of the C++ library.

#include "as_span.h"
#include "cppunivariate/distributions/invtrgamma/mle.hpp"
#include "cppunivariate/distributions/lnorm/mle.hpp"

#include <Rcpp.h>

#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlinvtrgamma_(const Rcpp::NumericVector &x,
                         Rcpp::Nullable<double> shape1 = R_NilValue,
                         Rcpp::Nullable<double> shape2 = R_NilValue,
                         double reltol = 1e-10, int iterlim = 100) {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  const double a = shape1.isNotNull() ? Rcpp::as<double>(shape1) : nan;
  const double b = shape2.isNotNull() ? Rcpp::as<double>(shape2) : nan;
  if ((shape1.isNotNull() && (!(a > 0.0) || !std::isfinite(a))) ||
      (shape2.isNotNull() && (!(b > 0.0) || !std::isfinite(b))))
    Rcpp::stop("`shape1` and `shape2` must be positive finite numbers when supplied.");
  const cppunivariate::span xs = dist_impl::as_span(x);
  auto res = cppunivariate::invtrgamma_mle(xs, reltol, iterlim, a, b);
  if (res.status == cppunivariate::InvTrGammaStatus::InvalidData)
    Rcpp::stop("invtrgamma requires at least two distinct positive finite observations, "
               "positive `reltol` and positive `iterlim`.");
  if (res.status == cppunivariate::InvTrGammaStatus::LognormalBoundary)
    Rcpp::stop("The invtrgamma likelihood is maximised at a limit. Use `mllnorm` to fit this model.");
  if (res.status == cppunivariate::InvTrGammaStatus::ParetoBoundary)
    Rcpp::stop("The invtrgamma likelihood is maximised at a limit. Use `mlpareto` to fit this model.");
  if (res.status == cppunivariate::InvTrGammaStatus::ScaleOutOfRange) {
    const double gap = res.logLik - cppunivariate::lognormal_mle(xs).logLik;
    Rcpp::stop("The invtrgamma maximum likelihood fit lies so close to the lognormal limit "
               "that `scale` is not representable with normal double precision (log(scale) = %g). "
               "Use `mllnorm`: its log-likelihood is lower by only %g.",
               res.log_scale, gap);
  }
  if (!std::isfinite(res.shape1) || !std::isfinite(res.shape2) || !std::isfinite(res.scale) ||
      !std::isfinite(res.logLik))
    Rcpp::stop("invtrgamma MLE failed to converge.");
  Rcpp::CharacterVector fixed;
  if (shape1.isNotNull()) fixed.push_back("shape1");
  if (shape2.isNotNull()) fixed.push_back("shape2");
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["shape1"] = res.shape1, Rcpp::_["shape2"] = res.shape2,
          Rcpp::_["scale"] = res.scale),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations,
      Rcpp::_["fixed"] = fixed);
}
