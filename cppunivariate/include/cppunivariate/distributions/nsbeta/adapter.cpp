#include "as_span.h"
#include "cppunivariate/distributions/nsbeta/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <algorithm>
// [[Rcpp::export]]
Rcpp::List mlnsbeta_(const Rcpp::NumericVector &x, double min, double max,
 double reltol = 1e-10, int iterlim = 1000, Rcpp::Nullable<double> shape1 = R_NilValue, Rcpp::Nullable<double> shape2 = R_NilValue) {
 if (!std::isfinite(min) || !std::isfinite(max) || !(max>min) || !std::isfinite(max-min)) Rcpp::stop("Held bounds must be finite and ordered.");
 for(double v:x) if (!(v>min) || !(v<max)) Rcpp::stop("Observations must lie strictly inside the held support.");
 Rcpp::CharacterVector fixed=Rcpp::CharacterVector::create("min", "max");
 if(shape1.isNotNull() || shape2.isNotNull()) Rcpp::stop("Holding additional parameters is not supported for this family.");
 auto res=cppunivariate::nsbeta_mle(dist_impl::as_span(x),min,max,reltol,iterlim);
  if (res.status == cppunivariate::BetaStatus::Degenerate) {
    Rcpp::stop("The beta maximum likelihood estimator does not exist for constant data: "
               "the likelihood is unbounded as both shapes grow.");
  }
  if(res.status==cppunivariate::BetaStatus::IterationFailure) Rcpp::stop("Nonstandard beta delegate search failed to converge.");
  if (!std::isfinite(res.shape1) || !std::isfinite(res.shape2) || !std::isfinite(res.logLik)) {
    Rcpp::stop("Beta MLE failed: the data are too close to a point mass to resolve the shapes in double precision.");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(Rcpp::_["shape1"] = res.shape1, Rcpp::_["shape2"] = res.shape2, Rcpp::_["min"] = min, Rcpp::_["max"] = max),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations, Rcpp::_["fixed"] = fixed);
}
