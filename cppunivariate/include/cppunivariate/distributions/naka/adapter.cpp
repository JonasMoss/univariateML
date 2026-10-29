// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/naka/mle.hpp"
#include <Rcpp.h>
#include <limits>
#include <cmath>
#include <algorithm>
#include <vector>

// [[Rcpp::export]]
Rcpp::List mlnaka_(const Rcpp::NumericVector &x, double abstol = 1.220703e-4,
                   int iterlim = 100,
    Rcpp::Nullable<double> shape = R_NilValue,
    Rcpp::Nullable<double> scale = R_NilValue) {
  Rcpp::CharacterVector fixed(0);
  const double shape_held = shape.isNull() ? std::numeric_limits<double>::quiet_NaN() : Rcpp::as<double>(shape);
  if (shape.isNotNull()) {
    if (!(std::isfinite(shape_held) && shape_held > 0.0)) Rcpp::stop("Held `shape` must be positive and finite.");
    fixed.push_back("shape");
  }
  const double scale_held = scale.isNull() ? std::numeric_limits<double>::quiet_NaN() : Rcpp::as<double>(scale);
  if (scale.isNotNull()) {
    if (!(std::isfinite(scale_held) && scale_held > 0.0)) Rcpp::stop("Held `scale` must be positive and finite.");
    fixed.push_back("scale");
  }

  auto res = cppunivariate::naka_mle(dist_impl::as_span(x), abstol, iterlim, shape_held, scale_held);
  if (res.status == cppunivariate::GammaStatus::IterationFailure)
    Rcpp::stop("Gamma delegate optimizer IterationFailure; increase iterlim.");
  if (res.status == cppunivariate::GammaStatus::Degenerate) {
    if (!std::all_of(x.begin(), x.end(), [&](double v) { return v == x[0]; }))
      Rcpp::stop("naka MLE failed: transformed observations exceed double precision.");
    Rcpp::stop("The Nakagami maximum likelihood estimator does not exist for constant data: "
               "the likelihood is unbounded as the shape grows.");
  }
  if (!std::isfinite(res.shape) || !std::isfinite(res.scale) || !std::isfinite(res.logLik)) {
    Rcpp::stop("Nakagami MLE failed");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["shape"] = res.shape, Rcpp::_["scale"] = res.scale),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations, Rcpp::_["fixed"] = fixed);
}

#include "dpqr.h"
#include "cppunivariate/distributions/naka/dpqr.hpp"
// [[Rcpp::export]]
Rcpp::NumericVector dnaka_(Rcpp::NumericVector x, Rcpp::NumericVector shape, Rcpp::NumericVector scale, bool log=false) {
  return dist_impl::dpqr_apply({x,shape,scale},[&](const std::vector<double> &v) {
    return cppunivariate::distributions::dnaka(v[0],v[1],v[2],log);
  });
}
// [[Rcpp::export]]
Rcpp::NumericVector pnaka_(Rcpp::NumericVector q, Rcpp::NumericVector shape, Rcpp::NumericVector scale, bool lower_tail=true, bool log_p=false) {
  return dist_impl::dpqr_apply({q,shape,scale},[&](const std::vector<double> &v) {
    return cppunivariate::distributions::pnaka(v[0],v[1],v[2],lower_tail,log_p);
  });
}
// [[Rcpp::export]]
Rcpp::NumericVector qnaka_(Rcpp::NumericVector p, Rcpp::NumericVector shape, Rcpp::NumericVector scale, bool lower_tail=true, bool log_p=false) {
  return dist_impl::dpqr_apply({p,shape,scale},[&](const std::vector<double> &v) {
    return cppunivariate::distributions::qnaka(v[0],v[1],v[2],lower_tail,log_p);
  });
}
// [[Rcpp::export]]
Rcpp::NumericVector rnaka_(int n, Rcpp::NumericVector shape, Rcpp::NumericVector scale) {
  return dist_impl::dpqr_apply({Rcpp::NumericVector(n),shape,scale},[&](const std::vector<double> &v) {
    if (!cppunivariate::distributions::naka_valid(v[1],v[2])) return R_NaN;
    const double gs=v[2]/v[1];
    if (gs>0 && std::isfinite(gs)) return std::sqrt(R::rgamma(v[1],gs));
    return std::sqrt(R::rgamma(v[1],1))*std::exp(0.5*(std::log(v[2])-std::log(v[1])));
  });
}
