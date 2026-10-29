// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/gompertz/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <algorithm>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlgompertz_(const Rcpp::NumericVector &x, double reltol = 1.220703e-4,
                       int iterlim = 100) {
  if (std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];}))
    Rcpp::stop("The Gompertz maximum likelihood estimator does not exist for constant data: the likelihood is unbounded.");
  auto res = cppunivariate::gompertz_mle(dist_impl::as_span(x), reltol, iterlim);
  if (res.status == cppunivariate::GompertzStatus::Overdispersed) {
    Rcpp::stop("The Gompertz maximum likelihood estimator escapes to an exponential "
               "when mean(x^2) >= 2*mean(x)^2. Use `mlexp` instead.");
  }
  if (res.status != cppunivariate::GompertzStatus::Ok || !std::isfinite(res.a) ||
      !std::isfinite(res.b)) {
    Rcpp::stop("Gompertz MLE failed");
  }
  return Rcpp::List::create(Rcpp::_["estimates"] = Rcpp::NumericVector::create(
                                Rcpp::_["a"] = res.a, Rcpp::_["b"] = res.b),
                            Rcpp::_["logLik"] = res.logLik,
                            Rcpp::_["iterations"] = res.iterations);
}
