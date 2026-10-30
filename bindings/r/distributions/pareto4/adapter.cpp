#include "as_span.h"
#include "cppunivariate/distributions/pareto4/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <algorithm>
// [[Rcpp::export]]
Rcpp::List mlpareto4_(const Rcpp::NumericVector &x, double min,
 double reltol = 1e-10, int iterlim = 1000, Rcpp::Nullable<double> scale = R_NilValue, Rcpp::Nullable<double> shape1 = R_NilValue, Rcpp::Nullable<double> shape2 = R_NilValue) {
 if(shape1.isNotNull() && (!(Rcpp::as<double>(shape1)>0)||!std::isfinite(Rcpp::as<double>(shape1)))) Rcpp::stop("`shape1` must be positive and finite.");
 if(shape2.isNotNull() && (!(Rcpp::as<double>(shape2)>0)||!std::isfinite(Rcpp::as<double>(shape2)))) Rcpp::stop("`shape2` must be positive and finite.");

 if (!std::isfinite(min)) Rcpp::stop("Held bounds must be finite and ordered.");
 for(double v:x) if (!(v>min)) Rcpp::stop("Observations must lie strictly inside the held support.");
 Rcpp::CharacterVector fixed=Rcpp::CharacterVector::create("min");
 if(scale.isNotNull()) { if(!(Rcpp::as<double>(scale)>0) || !std::isfinite(Rcpp::as<double>(scale))) Rcpp::stop("Held parameters must be positive and finite."); fixed.push_back("scale"); }
 if(shape1.isNotNull()) fixed.push_back("shape1");
 if(shape2.isNotNull()) fixed.push_back("shape2");
 auto res=cppunivariate::pareto4_mle(dist_impl::as_span(x),min,reltol,iterlim,scale.isNotNull() ? Rcpp::as<double>(scale) : std::numeric_limits<double>::quiet_NaN(),shape1.isNotNull()?Rcpp::as<double>(shape1):NA_REAL,shape2.isNotNull()?Rcpp::as<double>(shape2):NA_REAL);
 if (res.status == cppunivariate::BurrStatus::Unbounded) Rcpp::stop("Likelihood is unbounded for this constant sample; no MLE.");
 if (res.status == cppunivariate::BurrStatus::AllGeOne) Rcpp::stop("The fixed-scale Pareto IV MLE does not exist when all observations >= min + scale. The limit is a shifted Pareto with its endpoint held at min + scale; the package has no held-endpoint Pareto fitter.");
 if (res.status == cppunivariate::BurrStatus::WeibullBoundary) Rcpp::stop("The burr likelihood is maximised at a limit. Use `mlweibull(x - min)` to fit this model.");
 if (res.status == cppunivariate::BurrStatus::ParetoBoundary) Rcpp::stop("The burr likelihood is maximised at a limit. Use `mlpareto(x - min)` to fit this model.");
 if (res.status == cppunivariate::BurrStatus::IterationFailure) Rcpp::stop("Pareto IV delegate search failed to converge.");
 if (res.status == cppunivariate::BurrStatus::InvalidInit ||
 !std::isfinite(res.shape1) || !std::isfinite(res.shape2) || !std::isfinite(res.scale))
 Rcpp::stop("burr MLE requires positive finite nonconstant data and positive starts and a positive finite fixed scale");
 return Rcpp::List::create(
 Rcpp::_["estimates"] = Rcpp::NumericVector::create(Rcpp::_["shape1"] = res.shape1, Rcpp::_["shape2"] = res.shape2, Rcpp::_["scale"] = res.scale, Rcpp::_["min"] = min), Rcpp::_["logLik"] = res.logLik,
 Rcpp::_["iterations"] = res.iterations,
 Rcpp::_["fixed"] = fixed);
}
