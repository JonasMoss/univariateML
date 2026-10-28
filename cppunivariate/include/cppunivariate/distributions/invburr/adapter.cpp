// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/invburr/mle.hpp"
#include <Rcpp.h>
// [[Rcpp::export]]
Rcpp::List mlinvburr_(const Rcpp::NumericVector &x, double gradtol = 1e-8,
 int iterlim = 1000, Rcpp::Nullable<double> shape10 = R_NilValue,
 Rcpp::Nullable<double> shape20 = R_NilValue,
 Rcpp::Nullable<double> scale0 = R_NilValue, double reltol = 1e-12) {
 auto start = [](const Rcpp::Nullable<double> &v) {
   return v.isNotNull() ? Rcpp::as<double>(v) : NA_REAL;
 };
 auto res = cppunivariate::invburr_mle(dist_impl::as_span(x),gradtol,iterlim,
 start(shape10),start(shape20),start(scale0),reltol);
 if (res.status == cppunivariate::InvBurrStatus::InvWeibullBoundary) Rcpp::stop("The invburr likelihood is maximised at a limit. Use `mlinvweibull` to fit this model.");
 if (res.status == cppunivariate::InvBurrStatus::PowerBoundary) Rcpp::stop("The invburr likelihood is maximised at a limit. Use `mlpower` to fit this model.");
 if (res.status == cppunivariate::InvBurrStatus::InvalidInit ||
 !std::isfinite(res.shape1) || !std::isfinite(res.shape2) || !std::isfinite(res.scale))
 Rcpp::stop("invburr MLE requires positive finite nonconstant data and positive starts");
 return Rcpp::List::create(
 Rcpp::_["estimates"] = Rcpp::NumericVector::create(
 Rcpp::_["shape1"] = res.shape1, Rcpp::_["shape2"] = res.shape2,
 Rcpp::_["scale"] = res.scale), Rcpp::_["logLik"] = res.logLik,
 Rcpp::_["iterations"] = res.iterations);
}
