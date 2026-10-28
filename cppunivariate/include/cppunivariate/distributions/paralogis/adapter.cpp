// Rcpp adapter for mlparalogis(). Copied into the R package's src/ by
// univariateML/scripts/assemble_package.R; not part of the C++ library.

#include "as_span.h"
#include "cppunivariate/distributions/paralogis/mle.hpp"

#include <Rcpp.h>

#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlparalogis_(const Rcpp::NumericVector &x,
                        Rcpp::Nullable<double> shape0 = R_NilValue,
                        double gradtol = 1.220703e-4, int iterlim = 100) {
  double start = std::numeric_limits<double>::quiet_NaN();
  if (shape0.isNotNull()) {
    start = Rcpp::as<double>(shape0);
    if (!(start > 0.0) || !std::isfinite(start)) {
      Rcpp::stop("`shape0` must be a positive finite number.");
    }
  }
  auto res = cppunivariate::paralogis_mle(dist_impl::as_span(x), gradtol, iterlim, start);
  if (res.status == cppunivariate::ParalogisStatus::InvalidStart) {
    Rcpp::stop("Paralogistic MLE requires positive, finite observations.");
  }
  if (!std::isfinite(res.shape) || !std::isfinite(res.scale)) {
    Rcpp::stop("Paralogistic MLE failed to converge (check inputs)");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["shape"] = res.shape, Rcpp::_["scale"] = res.scale),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}
