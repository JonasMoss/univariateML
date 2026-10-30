// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/kumar/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlkumar_(const Rcpp::NumericVector &x, Rcpp::Nullable<double> a0 = R_NilValue,
                    double reltol = 1.220703e-4,
                    int iterlim = 100,
    Rcpp::Nullable<double> a = R_NilValue,
    Rcpp::Nullable<double> b = R_NilValue) {
  double start = std::numeric_limits<double>::quiet_NaN();
  if (a0.isNotNull()) {
    start = Rcpp::as<double>(a0);
    if (!(start > 0.0) || !std::isfinite(start))
      Rcpp::stop("`a0` must be a positive finite number.");
  }
  const double nan = std::numeric_limits<double>::quiet_NaN();
  auto held = [&](Rcpp::Nullable<double> z, bool positive) {
    if (z.isNull()) return nan;
    const double v = Rcpp::as<double>(z);
    if (!std::isfinite(v) || (positive && !(v > 0)))
      Rcpp::stop("Held parameters must be finite and shapes/scales positive.");
    return v;
  };
  Rcpp::CharacterVector fixed(0);
  if (a.isNotNull()) fixed.push_back("a");
  if (b.isNotNull()) fixed.push_back("b");
  auto res = cppunivariate::kumar_mle(dist_impl::as_span(x), reltol, iterlim, start, held(a, true), held(b, true));
  if (res.status == cppunivariate::KumarStatus::Unbounded)
    Rcpp::stop("Kumaraswamy likelihood is unbounded for constant samples.");
  if (!std::isfinite(res.a) || !std::isfinite(res.b) || !std::isfinite(res.logLik)) {
    Rcpp::stop("Kumaraswamy MLE failed: iteration limit or shapes cannot be resolved in double precision; increase iterlim.");
  }
  return Rcpp::List::create(Rcpp::_["estimates"] = Rcpp::NumericVector::create(
                                Rcpp::_["a"] = res.a, Rcpp::_["b"] = res.b),
                            Rcpp::_["fixed"] = fixed, Rcpp::_["logLik"] = res.logLik,
                            Rcpp::_["iterations"] = res.iterations);
}
