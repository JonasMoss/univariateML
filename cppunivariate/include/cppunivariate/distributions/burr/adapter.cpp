// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/burr/mle.hpp"
#include <Rcpp.h>
// [[Rcpp::export]]
Rcpp::List mlburr_(const Rcpp::NumericVector &x, double gradtol = 1e-8,
 int iterlim = 1000, Rcpp::Nullable<double> shape10 = R_NilValue,
 Rcpp::Nullable<double> shape20 = R_NilValue,
 Rcpp::Nullable<double> scale0 = R_NilValue, double reltol = 1e-12,
 Rcpp::Nullable<double> scale = R_NilValue, double abstol = 1.220703e-4) {
 auto start = [](const Rcpp::Nullable<double> &v) {
   return v.isNotNull() ? Rcpp::as<double>(v) : NA_REAL;
 };
 auto res = scale.isNotNull() ?
 cppunivariate::burr_mle_fixed_scale(dist_impl::as_span(x),start(scale),abstol,iterlim) :
 cppunivariate::burr_mle(dist_impl::as_span(x),gradtol,iterlim,
 start(shape10),start(shape20),start(scale0),reltol);
 if (res.status == cppunivariate::BurrStatus::Unbounded) Rcpp::stop("Likelihood is unbounded: all observations equal the held scale; no MLE.");
 if (res.status == cppunivariate::BurrStatus::AllGeOne) Rcpp::stop("The fixed-scale burr MLE does not exist when all observations >= scale. Use `mlpareto` to fit this model with endpoint scale.");
 if (res.status == cppunivariate::BurrStatus::WeibullBoundary) Rcpp::stop("The burr likelihood is maximised at a limit. Use `mlweibull` to fit this model.");
 if (res.status == cppunivariate::BurrStatus::ParetoBoundary) Rcpp::stop("The burr likelihood is maximised at a limit. Use `mlpareto` to fit this model.");
 if (res.status == cppunivariate::BurrStatus::InvalidInit ||
 !std::isfinite(res.shape1) || !std::isfinite(res.shape2) || !std::isfinite(res.scale))
 Rcpp::stop("burr MLE requires positive finite nonconstant data and positive starts and a positive finite fixed scale");
 return Rcpp::List::create(
 Rcpp::_["estimates"] = Rcpp::NumericVector::create(
 Rcpp::_["shape1"] = res.shape1, Rcpp::_["shape2"] = res.shape2,
 Rcpp::_["scale"] = res.scale), Rcpp::_["logLik"] = res.logLik,
 Rcpp::_["iterations"] = res.iterations,
 Rcpp::_["fixed"] = scale.isNotNull() ? Rcpp::CharacterVector::create("scale") : Rcpp::CharacterVector(0));
}
