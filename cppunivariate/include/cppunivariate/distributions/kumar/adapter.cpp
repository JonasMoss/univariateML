// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/kumar/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlkumar_(const Rcpp::NumericVector &x, Rcpp::Nullable<double> a0 = R_NilValue,
                    double reltol = 1.220703e-4,
                    int iterlim = 100) {
  double start = std::numeric_limits<double>::quiet_NaN();
  if (a0.isNotNull()) {
    start = Rcpp::as<double>(a0);
    if (!(start > 0.0) || !std::isfinite(start))
      Rcpp::stop("`a0` must be a positive finite number.");
  }
  auto res = cppunivariate::kumar_mle(dist_impl::as_span(x), reltol, iterlim, start);
  if (res.status == cppunivariate::KumarStatus::Unbounded)
    Rcpp::stop("Kumaraswamy likelihood is unbounded for constant samples.");
  if (!std::isfinite(res.a) || !std::isfinite(res.b)) {
    Rcpp::stop("Kumaraswamy MLE failed");
  }
  return Rcpp::List::create(Rcpp::_["estimates"] = Rcpp::NumericVector::create(
                                Rcpp::_["a"] = res.a, Rcpp::_["b"] = res.b),
                            Rcpp::_["logLik"] = res.logLik,
                            Rcpp::_["iterations"] = res.iterations);
}
