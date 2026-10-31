// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/fatigue/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <algorithm>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlfatigue_(const Rcpp::NumericVector &x, Rcpp::Nullable<double> mu, double abstol = 1.220703e-4,
                      int iterlim = 100, Rcpp::Nullable<double> alpha = R_NilValue,
                      Rcpp::Nullable<double> beta = R_NilValue) {
  if (mu.isNull()) Rcpp::stop("The threshold mu cannot be estimated. With mu free, the likelihood is unbounded for every sample with a unique minimum, so no maximum likelihood estimate exists. Supply mu.");
  const double threshold = Rcpp::as<double>(mu);
  if (!std::isfinite(threshold)) Rcpp::stop("`mu` must be finite.");
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double a=alpha.isNotNull()?Rcpp::as<double>(alpha):nan;
  const double b=beta.isNotNull()?Rcpp::as<double>(beta):nan;
  if ((alpha.isNotNull()&&(!(a>0)||!std::isfinite(a))) ||
      (beta.isNotNull()&&(!(b>0)||!std::isfinite(b))))
    Rcpp::stop("Held `alpha` and `beta` must be positive finite numbers.");
  auto res = cppunivariate::fatigue_mle_shifted(dist_impl::as_span(x), threshold, abstol, iterlim,a,b);
  if (res.status == cppunivariate::FatigueStatus::InvalidInput)
    Rcpp::stop("Fatigue observations must be finite and strictly above `mu`.");
  if (res.status == cppunivariate::FatigueStatus::SearchFailure)
    Rcpp::stop("Fatigue optimizer SearchFailure: profile search did not converge; increase iterlim.");
  if (!(res.alpha>0.0) || !std::isfinite(res.alpha) || !std::isfinite(res.beta) || !std::isfinite(res.logLik)) {
    if (res.status==cppunivariate::FatigueStatus::Unbounded)
      Rcpp::stop("The fatigue maximum likelihood estimator does not exist for constant data: the likelihood is unbounded.");
    Rcpp::stop("Fatigue MLE failed: data exceed double precision or profile search did not converge; increase iterlim.");
  }
  Rcpp::CharacterVector fixed(0);
  if(alpha.isNotNull()) fixed.push_back("alpha");
  if(beta.isNotNull()) fixed.push_back("beta");
  fixed.push_back("mu");
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["alpha"] = res.alpha, Rcpp::_["beta"] = res.beta,
          Rcpp::_["mu"] = threshold),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations,
      Rcpp::_["fixed"] = fixed);
}
