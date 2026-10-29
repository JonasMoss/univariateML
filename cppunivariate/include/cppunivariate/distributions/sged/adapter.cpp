// Rcpp adapter; assembled into the R package, outside the header-only core.
#include "as_span.h"
#include "cppunivariate/distributions/sged/mle.hpp"
#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlsged_(const Rcpp::NumericVector &x, double gradtol = 1.220703e-4,
                   int iterlim = 200) {
  auto res = cppunivariate::sged_mle(dist_impl::as_span(x), gradtol, iterlim);
  if (res.status == cppunivariate::SgedStatus::InvalidStart) {
    Rcpp::stop("Skew GED MLE requires at least 2 finite observations with "
               "positive variance.");
  }
  if (res.status == cppunivariate::SgedStatus::UniformBoundary) {
    Rcpp::stop("SGED likelihood is maximised as nu -> Inf, i.e. by the uniform distribution. Use `mlunif` instead.");
  }
  if (res.status == cppunivariate::SgedStatus::RightHalfGedBoundary) {
    Rcpp::stop("The likelihood is maximised by a translated half-GED with mode at the sample minimum; no MLE in the skewed family.");
  }
  if (res.status == cppunivariate::SgedStatus::LeftHalfGedBoundary) {
    Rcpp::stop("The likelihood is maximised by a translated half-GED with mode at the sample maximum; no MLE in the skewed family.");
  }
  if (!std::isfinite(res.mu) || !std::isfinite(res.sigma) ||
      !std::isfinite(res.nu) || !std::isfinite(res.xi)) {
    Rcpp::stop("Skew GED MLE failed to converge (check inputs)");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["mean"] = res.mu, Rcpp::_["sd"] = res.sigma,
          Rcpp::_["nu"] = res.nu, Rcpp::_["xi"] = res.xi),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}

#include "dpqr.h"
#include "cppunivariate/distributions/sged/dpqr.hpp"
// [[Rcpp::export]]
Rcpp::NumericVector dsged_(Rcpp::NumericVector x, Rcpp::NumericVector mean, Rcpp::NumericVector sd, Rcpp::NumericVector nu, Rcpp::NumericVector xi, bool log=false) {
  return dist_impl::dpqr_apply({x, mean, sd, nu, xi}, [&](const std::vector<double> &a) {
    return cppunivariate::distributions::dsged(a[0], a[1], a[2], a[3], a[4], log);
  });
}
// [[Rcpp::export]]
Rcpp::NumericVector psged_(Rcpp::NumericVector q, Rcpp::NumericVector mean, Rcpp::NumericVector sd, Rcpp::NumericVector nu, Rcpp::NumericVector xi, bool lower_tail=true, bool log_p=false) {
  return dist_impl::dpqr_apply({q, mean, sd, nu, xi}, [&](const std::vector<double> &a) {
    return cppunivariate::distributions::psged(a[0], a[1], a[2], a[3], a[4], lower_tail, log_p);
  });
}
// [[Rcpp::export]]
Rcpp::NumericVector qsged_(Rcpp::NumericVector p, Rcpp::NumericVector mean, Rcpp::NumericVector sd, Rcpp::NumericVector nu, Rcpp::NumericVector xi, bool lower_tail=true, bool log_p=false) {
  return dist_impl::dpqr_apply({p, mean, sd, nu, xi}, [&](const std::vector<double> &a) {
    return cppunivariate::distributions::qsged(a[0], a[1], a[2], a[3], a[4], lower_tail, log_p);
  });
}
struct SGEDRng {
  double runif(double a, double b) { return R::runif(a,b); }
  double rnorm() { return R::rnorm(0,1); }
  double rt(double nu) { return R::rt(nu); }
  double rgamma(double a, double b) { return R::rgamma(a,b); }
};
// [[Rcpp::export]]
Rcpp::NumericVector rsged_(int n, Rcpp::NumericVector mean, Rcpp::NumericVector sd, Rcpp::NumericVector nu, Rcpp::NumericVector xi) {
  SGEDRng rng;
  return dist_impl::dpqr_apply({Rcpp::NumericVector(n), mean, sd, nu, xi}, [&](const std::vector<double> &a) {
    return cppunivariate::distributions::rsged(rng, a[1], a[2], a[3], a[4]);
  });
}
