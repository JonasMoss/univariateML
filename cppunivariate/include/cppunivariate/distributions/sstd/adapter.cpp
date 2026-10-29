// Rcpp adapter; assembled into the R package, outside the header-only core.
#include "as_span.h"
#include "cppunivariate/distributions/sstd/mle.hpp"
#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlsstd_(const Rcpp::NumericVector &x, double gradtol = 1.220703e-4,
                   int iterlim = 200) {
  auto res = cppunivariate::sstd_mle(dist_impl::as_span(x), gradtol, iterlim);
  if (res.status == cppunivariate::SstdStatus::Unbounded)
    Rcpp::stop("Likelihood is unbounded at a tie of multiplicity k > 2*(n-k); no MLE.");
  if (res.status == cppunivariate::SstdStatus::ScaleBoundary)
    Rcpp::stop("Tie equality scale-collapse boundary; a regular MLE is not certified.");
  if (res.status == cppunivariate::SstdStatus::InvalidStart) {
    Rcpp::stop("Skew Student-t MLE requires at least 2 finite observations "
               "with positive variance.");
  }
  if (res.status == cppunivariate::SstdStatus::SnormBoundary) {
    Rcpp::stop("The skew Student-t likelihood is maximised as nu -> Inf, i.e. by "
               "the skew-normal distribution. Use `mlsnorm` to fit this model.");
  }
  if (res.status == cppunivariate::SstdStatus::RightHalfNormalBoundary) {
    Rcpp::stop("The skew Student-t likelihood is maximised as nu -> Inf and xi -> Inf, "
               "i.e. by a translated half-normal with endpoint at the sample minimum. "
               "Use `mlhnorm` to fit this model.");
  }
  if (res.status == cppunivariate::SstdStatus::LeftHalfNormalBoundary) {
    Rcpp::stop("The skew Student-t likelihood is maximised as nu -> Inf and xi -> 0, "
               "i.e. by a reflected translated half-normal with endpoint at the sample "
               "maximum. Use `mlhnorm` on `-x` to fit this model.");
  }
  if (res.status == cppunivariate::SstdStatus::RightHalfTBoundary ||
      res.status == cppunivariate::SstdStatus::LeftHalfTBoundary) {
    const bool right = res.status == cppunivariate::SstdStatus::RightHalfTBoundary;
    Rcpp::stop("The skew Student-t likelihood is maximised as xi -> %s, i.e. by a "
               "translated half-t with %.4g degrees of freedom and endpoint at the "
               "sample %s; the MLE does not exist.",
               right ? "Inf" : "0", res.nu, right ? "minimum" : "maximum");
  }
  if (res.status == cppunivariate::SstdStatus::DfTwoBoundary) {
    Rcpp::stop("The skew Student-t likelihood is maximised as nu -> 2, i.e. by a "
               "two-piece t with 2 degrees of freedom (infinite variance); the MLE "
               "does not exist in the mean/sd parameterisation.");
  }
  if (!std::isfinite(res.mu) || !std::isfinite(res.sigma) ||
      !std::isfinite(res.nu) || !std::isfinite(res.xi)) {
    Rcpp::stop("Skew Student-t MLE failed to converge (check inputs)");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["mean"] = res.mu, Rcpp::_["sd"] = res.sigma,
          Rcpp::_["nu"] = res.nu, Rcpp::_["xi"] = res.xi),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}

#include "dpqr.h"
#include "cppunivariate/distributions/sstd/dpqr.hpp"
// [[Rcpp::export]]
Rcpp::NumericVector dsstd_(Rcpp::NumericVector x, Rcpp::NumericVector mean, Rcpp::NumericVector sd, Rcpp::NumericVector nu, Rcpp::NumericVector xi, bool log=false) {
  return dist_impl::dpqr_apply({x, mean, sd, nu, xi}, [&](const std::vector<double> &a) {
    return cppunivariate::distributions::dsstd(a[0], a[1], a[2], a[3], a[4], log);
  });
}
// [[Rcpp::export]]
Rcpp::NumericVector psstd_(Rcpp::NumericVector q, Rcpp::NumericVector mean, Rcpp::NumericVector sd, Rcpp::NumericVector nu, Rcpp::NumericVector xi, bool lower_tail=true, bool log_p=false) {
  return dist_impl::dpqr_apply({q, mean, sd, nu, xi}, [&](const std::vector<double> &a) {
    return cppunivariate::distributions::psstd(a[0], a[1], a[2], a[3], a[4], lower_tail, log_p);
  });
}
// [[Rcpp::export]]
Rcpp::NumericVector qsstd_(Rcpp::NumericVector p, Rcpp::NumericVector mean, Rcpp::NumericVector sd, Rcpp::NumericVector nu, Rcpp::NumericVector xi, bool lower_tail=true, bool log_p=false) {
  return dist_impl::dpqr_apply({p, mean, sd, nu, xi}, [&](const std::vector<double> &a) {
    return cppunivariate::distributions::qsstd(a[0], a[1], a[2], a[3], a[4], lower_tail, log_p);
  });
}
struct SSTDRng {
  double runif(double a, double b) { return R::runif(a,b); }
  double rnorm() { return R::rnorm(0,1); }
  double rt(double nu) { return R::rt(nu); }
  double rgamma(double a, double b) { return R::rgamma(a,b); }
};
// [[Rcpp::export]]
Rcpp::NumericVector rsstd_(int n, Rcpp::NumericVector mean, Rcpp::NumericVector sd, Rcpp::NumericVector nu, Rcpp::NumericVector xi) {
  SSTDRng rng;
  return dist_impl::dpqr_apply({Rcpp::NumericVector(n), mean, sd, nu, xi}, [&](const std::vector<double> &a) {
    return cppunivariate::distributions::rsstd(rng, a[1], a[2], a[3], a[4]);
  });
}
