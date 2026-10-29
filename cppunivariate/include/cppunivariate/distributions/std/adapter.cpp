// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/std/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlstd_(const Rcpp::NumericVector &x, double gradtol = 1.220703e-4,
                  int iterlim = 200, Rcpp::Nullable<double> nu = R_NilValue) {
  auto res = nu.isNotNull()
    ? cppunivariate::student_t_mle_fixed_nu(dist_impl::as_span(x), Rcpp::as<double>(nu), gradtol, iterlim)
    : cppunivariate::student_t_mle(dist_impl::as_span(x), gradtol, iterlim);
  if (res.status == cppunivariate::StudentTStatus::InvalidNu)
    Rcpp::stop("`nu` must be finite and greater than 2.");
  if (res.status == cppunivariate::StudentTStatus::Unbounded)
    Rcpp::stop("Likelihood is unbounded at a tie of excessive multiplicity k > nu*(n-k) (free df: k > 2*(n-k)); no MLE.");
  if (res.status == cppunivariate::StudentTStatus::ScaleBoundary)
    Rcpp::stop("Tie equality scale-collapse boundary; a regular MLE is not certified.");
  if (res.status == cppunivariate::StudentTStatus::IterationFailure)
    Rcpp::stop("Fixed `nu` EM did not converge to the score tolerance; increase iterlim.");
  if (res.status == cppunivariate::StudentTStatus::NormalBoundary) {
    Rcpp::stop("The Student-t likelihood is maximised as nu -> Inf, i.e. by the "
               "normal distribution. Use `mlnorm` to fit this model.");
  }
  if (res.status == cppunivariate::StudentTStatus::DfTwoBoundary) {
    Rcpp::stop("The Student-t likelihood is maximised as nu -> 2 (infinite "
               "variance); the MLE does not exist in the mean/sd parameterisation. "
               "Use `mllst` to fit a location-scale t distribution with free degrees of freedom.");
  }
  if (res.status == cppunivariate::StudentTStatus::InvalidStart) {
    Rcpp::stop("Student-t MLE requires at least 2 finite observations with "
               "positive variance.");
  }
  if (!std::isfinite(res.mu) || !std::isfinite(res.sigma) ||
      !std::isfinite(res.nu)) {
    Rcpp::stop("Student-t MLE failed to converge (check inputs)");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["mean"] = res.mu, Rcpp::_["sd"] = res.sigma,
          Rcpp::_["nu"] = res.nu),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations,
      Rcpp::_["fixed"] = nu.isNotNull() ? Rcpp::CharacterVector::create("nu") : Rcpp::CharacterVector(0));
}

#include "dpqr.h"
#include "cppunivariate/distributions/std/dpqr.hpp"
// [[Rcpp::export]]
Rcpp::NumericVector dstd_(Rcpp::NumericVector x, Rcpp::NumericVector mean, Rcpp::NumericVector sd, Rcpp::NumericVector nu, bool log=false) {
  return dist_impl::dpqr_apply({x, mean, sd, nu}, [&](const std::vector<double> &a) {
    return cppunivariate::distributions::dstd(a[0], a[1], a[2], a[3], log);
  });
}
// [[Rcpp::export]]
Rcpp::NumericVector pstd_(Rcpp::NumericVector q, Rcpp::NumericVector mean, Rcpp::NumericVector sd, Rcpp::NumericVector nu, bool lower_tail=true, bool log_p=false) {
  return dist_impl::dpqr_apply({q, mean, sd, nu}, [&](const std::vector<double> &a) {
    return cppunivariate::distributions::pstd(a[0], a[1], a[2], a[3], lower_tail, log_p);
  });
}
// [[Rcpp::export]]
Rcpp::NumericVector qstd_(Rcpp::NumericVector p, Rcpp::NumericVector mean, Rcpp::NumericVector sd, Rcpp::NumericVector nu, bool lower_tail=true, bool log_p=false) {
  return dist_impl::dpqr_apply({p, mean, sd, nu}, [&](const std::vector<double> &a) {
    return cppunivariate::distributions::qstd(a[0], a[1], a[2], a[3], lower_tail, log_p);
  });
}
struct STDRng {
  double runif(double a, double b) { return R::runif(a,b); }
  double rnorm() { return R::rnorm(0,1); }
  double rt(double nu) { return R::rt(nu); }
  double rgamma(double a, double b) { return R::rgamma(a,b); }
};
// [[Rcpp::export]]
Rcpp::NumericVector rstd_(int n, Rcpp::NumericVector mean, Rcpp::NumericVector sd, Rcpp::NumericVector nu) {
  STDRng rng;
  return dist_impl::dpqr_apply({Rcpp::NumericVector(n), mean, sd, nu}, [&](const std::vector<double> &a) {
    return cppunivariate::distributions::rstd(rng, a[1], a[2], a[3]);
  });
}
