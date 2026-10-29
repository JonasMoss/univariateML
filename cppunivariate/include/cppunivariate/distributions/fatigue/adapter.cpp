// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/fatigue/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <algorithm>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlfatigue_(const Rcpp::NumericVector &x, double abstol = 1.220703e-4,
                      int iterlim = 100) {
  auto res = cppunivariate::fatigue_mle(dist_impl::as_span(x), abstol, iterlim);
  if (!(res.alpha>0.0) || !std::isfinite(res.alpha) || !std::isfinite(res.beta) || !std::isfinite(res.logLik)) {
    if (std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];}))
      Rcpp::stop("The fatigue maximum likelihood estimator does not exist for constant data: the likelihood is unbounded.");
    Rcpp::stop("Fatigue MLE failed: data exceed double precision.");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["alpha"] = res.alpha, Rcpp::_["beta"] = res.beta,
          Rcpp::_["mu"] = 0.0),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations,
      Rcpp::_["fixed"] = Rcpp::CharacterVector::create("mu"));
}
