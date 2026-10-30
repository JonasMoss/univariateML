// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/burr/mle.hpp"
#include <Rcpp.h>
// [[Rcpp::export]]
Rcpp::List mlburr_(const Rcpp::NumericVector &x, double gradtol = 1e-8,
 int iterlim = 1000, Rcpp::Nullable<double> shape10 = R_NilValue,
 Rcpp::Nullable<double> shape20 = R_NilValue,
 Rcpp::Nullable<double> scale0 = R_NilValue, double reltol = 1e-12,
 Rcpp::Nullable<double> scale = R_NilValue, double abstol = 1.220703e-4,
 Rcpp::Nullable<double> shape1 = R_NilValue, Rcpp::Nullable<double> shape2 = R_NilValue) {
 if(shape1.isNotNull() && (!(Rcpp::as<double>(shape1)>0)||!std::isfinite(Rcpp::as<double>(shape1)))) Rcpp::stop("`shape1` must be positive and finite.");
 if(shape2.isNotNull() && (!(Rcpp::as<double>(shape2)>0)||!std::isfinite(Rcpp::as<double>(shape2)))) Rcpp::stop("`shape2` must be positive and finite.");
 if(scale.isNotNull() && (!(Rcpp::as<double>(scale)>0)||!std::isfinite(Rcpp::as<double>(scale)))) Rcpp::stop("burr requires a positive finite fixed scale.");

 auto start = [](const Rcpp::Nullable<double> &v) {
   return v.isNotNull() ? Rcpp::as<double>(v) : NA_REAL;
 };
 auto res = (shape1.isNotNull() || shape2.isNotNull()) ?
 cppunivariate::burr_mle_holds(dist_impl::as_span(x),start(shape1),start(shape2),start(scale),gradtol,iterlim) : scale.isNotNull() ?
 cppunivariate::burr_mle_fixed_scale(dist_impl::as_span(x),start(scale),abstol,iterlim) :
 cppunivariate::burr_mle(dist_impl::as_span(x),gradtol,iterlim,
 start(shape10),start(shape20),start(scale0),reltol);
 if (res.status == cppunivariate::BurrStatus::Unbounded) Rcpp::stop("Likelihood is unbounded for this constant sample; no MLE.");
 if (res.status == cppunivariate::BurrStatus::AllGeOne) Rcpp::stop("The fixed-scale burr MLE does not exist when all observations >= scale. Use `mlpareto` to fit this model with endpoint scale.");
 if (res.status == cppunivariate::BurrStatus::WeibullBoundary) Rcpp::stop("The burr likelihood is maximised at a limit. Use `mlweibull` to fit this model.");
 if (res.status == cppunivariate::BurrStatus::ParetoBoundary) Rcpp::stop("The burr likelihood is maximised at a limit. Use `mlpareto` to fit this model.");
 if (res.status == cppunivariate::BurrStatus::IterationFailure) Rcpp::stop("Constrained likelihood search failed to converge.");
 if (res.status == cppunivariate::BurrStatus::InvalidInit ||
 !std::isfinite(res.shape1) || !std::isfinite(res.shape2) || !std::isfinite(res.scale))
 Rcpp::stop("burr MLE requires positive finite nonconstant data and positive starts and a positive finite fixed scale");
 Rcpp::CharacterVector fixed;
 if(shape1.isNotNull()) fixed.push_back("shape1");
 if(shape2.isNotNull()) fixed.push_back("shape2");
 if(scale.isNotNull()) fixed.push_back("scale");
 return Rcpp::List::create(
 Rcpp::_["estimates"] = Rcpp::NumericVector::create(
 Rcpp::_["shape1"] = res.shape1, Rcpp::_["shape2"] = res.shape2,
 Rcpp::_["scale"] = res.scale), Rcpp::_["logLik"] = res.logLik,
 Rcpp::_["iterations"] = res.iterations,
 Rcpp::_["fixed"] = fixed);
}
