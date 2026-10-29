// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/ged/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlged_(const Rcpp::NumericVector &x, double gradtol = 1.220703e-4,
                  int iterlim = 200) {
  auto res = cppunivariate::ged_mle(dist_impl::as_span(x), gradtol, iterlim);
  if (res.status == cppunivariate::GedStatus::InvalidStart) {
    Rcpp::stop("GED MLE requires at least 2 finite observations with positive "
               "variance.");
  }
  if (res.status == cppunivariate::GedStatus::UniformBoundary) {
    Rcpp::stop("GED likelihood is maximised as nu -> Inf, i.e. by the uniform distribution. Use `mlunif` instead.");
  }
  if (!std::isfinite(res.mu) || !std::isfinite(res.sigma) ||
      !std::isfinite(res.nu)) {
    Rcpp::stop("GED MLE failed to converge (check inputs)");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["mean"] = res.mu, Rcpp::_["sd"] = res.sigma,
          Rcpp::_["nu"] = res.nu),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}

#include "dpqr.h"
#include "cppunivariate/distributions/ged/dpqr.hpp"
// [[Rcpp::export]]
Rcpp::NumericVector dged_(Rcpp::NumericVector x, Rcpp::NumericVector mean, Rcpp::NumericVector sd, Rcpp::NumericVector nu, bool log=false) {
  return dist_impl::dpqr_apply({x, mean, sd, nu}, [&](const std::vector<double> &a) {
    return cppunivariate::distributions::dged(a[0], a[1], a[2], a[3], log);
  });
}
// [[Rcpp::export]]
Rcpp::NumericVector pged_(Rcpp::NumericVector q, Rcpp::NumericVector mean, Rcpp::NumericVector sd, Rcpp::NumericVector nu, bool lower_tail=true, bool log_p=false) {
  return dist_impl::dpqr_apply({q, mean, sd, nu}, [&](const std::vector<double> &a) {
    return cppunivariate::distributions::pged(a[0], a[1], a[2], a[3], lower_tail, log_p);
  });
}
// [[Rcpp::export]]
Rcpp::NumericVector qged_(Rcpp::NumericVector p, Rcpp::NumericVector mean, Rcpp::NumericVector sd, Rcpp::NumericVector nu, bool lower_tail=true, bool log_p=false) {
  return dist_impl::dpqr_apply({p, mean, sd, nu}, [&](const std::vector<double> &a) {
    return cppunivariate::distributions::qged(a[0], a[1], a[2], a[3], lower_tail, log_p);
  });
}
struct GEDRng {
  double runif(double a, double b) { return R::runif(a,b); }
  double rnorm() { return R::rnorm(0,1); }
  double rt(double nu) { return R::rt(nu); }
  double rgamma(double a, double b) { return R::rgamma(a,b); }
};
// [[Rcpp::export]]
Rcpp::NumericVector rged_(int n, Rcpp::NumericVector mean, Rcpp::NumericVector sd, Rcpp::NumericVector nu) {
  GEDRng rng;
  return dist_impl::dpqr_apply({Rcpp::NumericVector(n), mean, sd, nu}, [&](const std::vector<double> &a) {
    return cppunivariate::distributions::rged(rng, a[1], a[2], a[3]);
  });
}
