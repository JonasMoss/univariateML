// Rcpp adapter for mlparalogis(). Copied into the R package's src/ by
// univariateML/scripts/assemble_package.R; not part of the C++ library.

#include "as_span.h"
#include "cppunivariate/distributions/paralogis/mle.hpp"

#include <Rcpp.h>

#include <cmath>
#include <algorithm>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlparalogis_(const Rcpp::NumericVector &x,
                        Rcpp::Nullable<double> shape0 = R_NilValue,
                        double gradtol = 1.220703e-4, int iterlim = 100,
                        Rcpp::Nullable<double> scale = R_NilValue,
                        Rcpp::Nullable<double> shape = R_NilValue) {
 if(shape.isNotNull() && (!(Rcpp::as<double>(shape)>0)||!std::isfinite(Rcpp::as<double>(shape)))) Rcpp::stop("`shape` must be positive and finite.");

  double start = std::numeric_limits<double>::quiet_NaN();
  if (shape0.isNotNull()) {
    start = Rcpp::as<double>(shape0);
    if (!(start > 0.0) || !std::isfinite(start)) {
      Rcpp::stop("`shape0` must be a positive finite number.");
    }
  }
  const double held = scale.isNotNull() ? Rcpp::as<double>(scale) : 1.0;
  if (!(held > 0) || !std::isfinite(held))
    Rcpp::stop("`scale` must be a positive finite number.");
  if (shape.isNull() && scale.isNull() && std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];}))
    Rcpp::stop("The paralogis maximum likelihood estimator does not exist for constant data: the likelihood is unbounded.");
  auto res = shape.isNotNull() ? cppunivariate::paralogis_mle_holds(dist_impl::as_span(x),Rcpp::as<double>(shape),scale.isNotNull()?held:NA_REAL,gradtol,iterlim) : scale.isNotNull()
      ? cppunivariate::paralogis_mle_fixed_scale(dist_impl::as_span(x), held, gradtol, iterlim, start)
      : cppunivariate::paralogis_mle(dist_impl::as_span(x), gradtol, iterlim, start);
  if (res.status == cppunivariate::ParalogisStatus::BfgsFailure)
    Rcpp::stop("Paralogis optimizer BfgsFailure: score check failed; increase iterlim.");
  if (res.status == cppunivariate::ParalogisStatus::InvalidStart) {
    Rcpp::stop("Paralogistic MLE requires positive, finite observations.");
  }
  if (!std::isfinite(res.shape) || !std::isfinite(res.scale) || !std::isfinite(res.logLik)) {
    Rcpp::stop("Paralogistic MLE failed: fitted parameters or likelihood exceed double precision");
  }
  Rcpp::CharacterVector fixed;
  if(shape.isNotNull()) fixed.push_back("shape");
  if(scale.isNotNull()) fixed.push_back("scale");
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["shape"] = res.shape, Rcpp::_["scale"] = res.scale),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations,
      Rcpp::_["fixed"] = fixed);
}
