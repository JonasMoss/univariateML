// Rcpp adapter for mlinvpareto(). Copied into the R package's src/ by
// univariateML/scripts/assemble_package.R; not part of the C++ library.

#include "as_span.h"
#include "cppunivariate/distributions/invpareto/mle.hpp"

#include <Rcpp.h>

#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlinvpareto_(const Rcpp::NumericVector &x,
                    Rcpp::Nullable<double> scale0 = R_NilValue,
                    double reltol = 1.220703e-4, int iterlim = 100) {
  double start = std::numeric_limits<double>::quiet_NaN();
  if (scale0.isNotNull()) {
    start = Rcpp::as<double>(scale0);
    if (!(start > 0.0) || !std::isfinite(start)) {
      Rcpp::stop("`scale0` must be a positive finite number.");
    }
  }
  auto res = cppunivariate::invpareto_mle(dist_impl::as_span(x), reltol, iterlim, start);
  if (res.status == cppunivariate::LomaxStatus::MeanVarianceCondition) {
    Rcpp::stop("The inverse Pareto maximum likelihood estimator does not exist for data "
               "whose likelihood is maximized on the inverse exponential face. Use `mlinvexp` to fit an "
               "inverse exponential distribution.");
  }
  if (res.status != cppunivariate::LomaxStatus::Ok || !std::isfinite(res.scale) ||
      !std::isfinite(res.shape)) {
    Rcpp::stop("Inverse Pareto MLE failed to converge (check dispersion/inputs)");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["shape"] = res.shape, Rcpp::_["scale"] = res.scale),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}
