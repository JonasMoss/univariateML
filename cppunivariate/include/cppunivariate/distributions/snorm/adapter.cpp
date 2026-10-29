// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/snorm/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlsnorm_(const Rcpp::NumericVector &x) {
  auto res = cppunivariate::snorm_mle(dist_impl::as_span(x), 1.220703e-4, 200);
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
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["mean"] = res.mu, Rcpp::_["sd"] = res.sigma,
          Rcpp::_["xi"] = res.xi),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
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
