// Rcpp adapter; assembled into the R package, outside the header-only core.
#include "as_span.h"
#include "cppunivariate/distributions/sstd/mle.hpp"
#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlsstd_(const Rcpp::NumericVector &x,
    Rcpp::Nullable<double> mean = R_NilValue,
    Rcpp::Nullable<double> sd = R_NilValue,
    Rcpp::Nullable<double> nu = R_NilValue,
    Rcpp::Nullable<double> xi = R_NilValue, double gradtol = 1.220703e-4, int iterlim = 200) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double held_mean=mean.isNotNull() ? Rcpp::as<double>(mean) : nan;
  if(mean.isNotNull() && (!std::isfinite(held_mean))) Rcpp::stop("Invalid held `mean`: requires a finite value.");
  const double held_sd=sd.isNotNull() ? Rcpp::as<double>(sd) : nan;
  if(sd.isNotNull() && (!(std::isfinite(held_sd) && held_sd>0))) Rcpp::stop("Invalid held `sd`: requires a finite value in its parameter domain.");
  const double held_nu=nu.isNotNull() ? Rcpp::as<double>(nu) : nan;
  if(nu.isNotNull() && (!(std::isfinite(held_nu) && held_nu>2))) Rcpp::stop("Invalid held `nu`: requires a finite value in its parameter domain.");
  const double held_xi=xi.isNotNull() ? Rcpp::as<double>(xi) : nan;
  if(xi.isNotNull() && (!(std::isfinite(held_xi) && held_xi>0))) Rcpp::stop("Invalid held `xi`: requires a finite value in its parameter domain.");
  auto res=cppunivariate::sstd_mle(dist_impl::as_span(x),gradtol,iterlim,held_mean,held_sd,held_nu,held_xi);
  if(res.status==cppunivariate::SstdStatus::UnsupportedHold) Rcpp::stop("Hold subset existence remains unresolved (P0); currently hold `nu`, `xi` and at least one of `mean`, `sd`.");
  if (res.status == cppunivariate::SstdStatus::Unbounded)
    Rcpp::stop("Likelihood is unbounded at a tie of multiplicity k > nu*(n-k) with nu held, or k > 2*(n-k) with nu free; no MLE.");
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
  if(res.status==cppunivariate::SstdStatus::BfgsFailure) {
    cppunivariate::SstdCost cost{dist_impl::as_span(x)};
    const double phi[4]={res.mu,std::log(res.sigma),std::log(res.nu-2),std::log(res.xi)};
    double score[4];
    cppunivariate::finite_diff_gradient(cost,phi,score);
    double norm=0;
    for(double v:score) norm+=v*v;
    if(!(std::sqrt(norm)<1e-3)) Rcpp::stop("Skew fit optimizer failed its score check; increase `iterlim`.");
  }
  Rcpp::CharacterVector fixed;
  if(mean.isNotNull()) fixed.push_back("mean");
  if(sd.isNotNull()) fixed.push_back("sd");
  if(nu.isNotNull()) fixed.push_back("nu");
  if(xi.isNotNull()) fixed.push_back("xi");
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["mean"] = res.mu, Rcpp::_["sd"] = res.sigma,
          Rcpp::_["nu"] = res.nu, Rcpp::_["xi"] = res.xi),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations,
      Rcpp::_["fixed"] = fixed);
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
