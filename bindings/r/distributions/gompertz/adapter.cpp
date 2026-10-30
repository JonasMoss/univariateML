// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/gompertz/mle.hpp"
#include <Rcpp.h>
#include <limits>
#include <cmath>
#include <algorithm>

// [[Rcpp::export]]
Rcpp::List mlgompertz_(const Rcpp::NumericVector &x, double reltol = 1.220703e-4,
                       int iterlim = 100,
    Rcpp::Nullable<double> a = R_NilValue,
    Rcpp::Nullable<double> b = R_NilValue) {
  Rcpp::CharacterVector fixed(0);
  const double a_held = a.isNull() ? std::numeric_limits<double>::quiet_NaN() : Rcpp::as<double>(a);
  if (a.isNotNull()) {
    if (!(std::isfinite(a_held) && a_held > 0.0)) Rcpp::stop("Held `a` must be positive and finite.");
    fixed.push_back("a");
  }
  const double b_held = b.isNull() ? std::numeric_limits<double>::quiet_NaN() : Rcpp::as<double>(b);
  if (b.isNotNull()) {
    if (!(std::isfinite(b_held) && b_held > 0.0)) Rcpp::stop("Held `b` must be positive and finite.");
    fixed.push_back("b");
  }

  auto res = cppunivariate::gompertz_mle(dist_impl::as_span(x), reltol, iterlim, a_held, b_held);
  if(res.status==cppunivariate::GompertzStatus::Unbounded)
    Rcpp::stop("The Gompertz maximum likelihood estimator does not exist for constant data: the likelihood is unbounded.");

  if (res.status == cppunivariate::GompertzStatus::Overdispersed) {
    Rcpp::stop("The Gompertz maximum likelihood estimator escapes to an exponential "
               "when the score at b=0 is nonpositive. Use `mlexp` instead.");
  }
  if (res.status != cppunivariate::GompertzStatus::Ok || !std::isfinite(res.a) ||
      !std::isfinite(res.b)) {
    if (a.isNull() && std::all_of(x.begin(),x.end(),[](double v){return v==0;}))
      Rcpp::stop("The Gompertz likelihood is unbounded for all-zero data with a free.");
    Rcpp::stop("Gompertz MLE failed to converge or data exceed double precision.");
  }
  return Rcpp::List::create(Rcpp::_["estimates"] = Rcpp::NumericVector::create(
                                Rcpp::_["a"] = res.a, Rcpp::_["b"] = res.b),
                            Rcpp::_["logLik"] = res.logLik,
                            Rcpp::_["iterations"] = res.iterations, Rcpp::_["fixed"] = fixed);
}
