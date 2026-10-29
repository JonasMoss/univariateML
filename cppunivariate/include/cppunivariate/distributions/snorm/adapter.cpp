// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/snorm/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlsnorm_(const Rcpp::NumericVector &x,
    Rcpp::Nullable<double> mean = R_NilValue,
    Rcpp::Nullable<double> sd = R_NilValue,
    Rcpp::Nullable<double> xi = R_NilValue) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double held_mean=mean.isNotNull() ? Rcpp::as<double>(mean) : nan;
  if(mean.isNotNull() && (!std::isfinite(held_mean))) Rcpp::stop("Invalid held `mean`: requires a finite value.");
  const double held_sd=sd.isNotNull() ? Rcpp::as<double>(sd) : nan;
  if(sd.isNotNull() && (!(std::isfinite(held_sd) && held_sd>0))) Rcpp::stop("Invalid held `sd`: requires a finite value in its parameter domain.");
  const double held_xi=xi.isNotNull() ? Rcpp::as<double>(xi) : nan;
  if(xi.isNotNull() && (!(std::isfinite(held_xi) && held_xi>0))) Rcpp::stop("Invalid held `xi`: requires a finite value in its parameter domain.");
  auto res=cppunivariate::snorm_mle(dist_impl::as_span(x),1.220703e-4,200,held_mean,held_sd,held_xi);
  if(res.status==cppunivariate::SnormStatus::Unbounded) Rcpp::stop("Scale collapse: the conditional skew-normal likelihood is unbounded; no MLE.");
  if(res.status==cppunivariate::SnormStatus::BfgsFailure) Rcpp::stop("Conditional skew-normal fit did not converge.");
  if (res.status == cppunivariate::SnormStatus::RightHalfNormalBoundary) {
    Rcpp::stop("The likelihood is maximised by a translated half-normal (mode at the sample minimum); the skew-normal MLE does not exist. Use `mlhnorm` to fit a translated half-normal.");
  }
  if (res.status == cppunivariate::SnormStatus::LeftHalfNormalBoundary) {
    Rcpp::stop("The likelihood is maximised by a translated half-normal (mode at the sample maximum); the skew-normal MLE does not exist. Use `mlhnorm` on `-x` (a half-normal reflected at the sample maximum).");
  }
  if (res.status == cppunivariate::SnormStatus::InvalidStart) {
    Rcpp::stop("Skew-normal MLE requires at least 2 finite observations with "
               "positive variance.");
  }
  if (!std::isfinite(res.mu) || !std::isfinite(res.sigma) ||
      !std::isfinite(res.xi)) {
    Rcpp::stop("Skew-normal MLE failed to converge (check inputs)");
  }
  Rcpp::CharacterVector fixed;
  if(mean.isNotNull()) fixed.push_back("mean");
  if(sd.isNotNull()) fixed.push_back("sd");
  if(xi.isNotNull()) fixed.push_back("xi");
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["mean"] = res.mu, Rcpp::_["sd"] = res.sigma,
          Rcpp::_["xi"] = res.xi),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations,
      Rcpp::_["fixed"] = fixed);
}

#include "dpqr.h"
#include "cppunivariate/distributions/snorm/dpqr.hpp"
// [[Rcpp::export]]
Rcpp::NumericVector dsnorm_(Rcpp::NumericVector x, Rcpp::NumericVector mean, Rcpp::NumericVector sd, Rcpp::NumericVector xi, bool log=false) {
  return dist_impl::dpqr_apply({x, mean, sd, xi}, [&](const std::vector<double> &a) {
    return cppunivariate::distributions::dsnorm(a[0], a[1], a[2], a[3], log);
  });
}
// [[Rcpp::export]]
Rcpp::NumericVector psnorm_(Rcpp::NumericVector q, Rcpp::NumericVector mean, Rcpp::NumericVector sd, Rcpp::NumericVector xi, bool lower_tail=true, bool log_p=false) {
  return dist_impl::dpqr_apply({q, mean, sd, xi}, [&](const std::vector<double> &a) {
    return cppunivariate::distributions::psnorm(a[0], a[1], a[2], a[3], lower_tail, log_p);
  });
}
// [[Rcpp::export]]
Rcpp::NumericVector qsnorm_(Rcpp::NumericVector p, Rcpp::NumericVector mean, Rcpp::NumericVector sd, Rcpp::NumericVector xi, bool lower_tail=true, bool log_p=false) {
  return dist_impl::dpqr_apply({p, mean, sd, xi}, [&](const std::vector<double> &a) {
    return cppunivariate::distributions::qsnorm(a[0], a[1], a[2], a[3], lower_tail, log_p);
  });
}
struct SNORMRng {
  double runif(double a, double b) { return R::runif(a,b); }
  double rnorm() { return R::rnorm(0,1); }
  double rt(double nu) { return R::rt(nu); }
  double rgamma(double a, double b) { return R::rgamma(a,b); }
};
// [[Rcpp::export]]
Rcpp::NumericVector rsnorm_(int n, Rcpp::NumericVector mean, Rcpp::NumericVector sd, Rcpp::NumericVector xi) {
  SNORMRng rng;
  return dist_impl::dpqr_apply({Rcpp::NumericVector(n), mean, sd, xi}, [&](const std::vector<double> &a) {
    return cppunivariate::distributions::rsnorm(rng, a[1], a[2], a[3]);
  });
}
