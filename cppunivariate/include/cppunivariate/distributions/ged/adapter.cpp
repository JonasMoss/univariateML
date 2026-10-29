// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/ged/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlged_(const Rcpp::NumericVector &x, double gradtol = 1.220703e-4,
                  int iterlim = 200, Rcpp::Nullable<double> mean = R_NilValue,
                  Rcpp::Nullable<double> sd = R_NilValue, Rcpp::Nullable<double> nu = R_NilValue) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  Rcpp::CharacterVector fixed;
  const double held_mean=mean.isNotNull()?Rcpp::as<double>(mean):nan;
  if(mean.isNotNull() && (!std::isfinite(held_mean)))Rcpp::stop("Invalid held `mean`.");
  if(mean.isNotNull())fixed.push_back("mean");
  const double held_sd=sd.isNotNull()?Rcpp::as<double>(sd):nan;
  if(sd.isNotNull() && (!std::isfinite(held_sd)||!(held_sd>0)))Rcpp::stop("Invalid held `sd`.");
  if(sd.isNotNull())fixed.push_back("sd");
  const double held_nu=nu.isNotNull()?Rcpp::as<double>(nu):nan;
  if(nu.isNotNull() && (!std::isfinite(held_nu)||!(held_nu>0)))Rcpp::stop("Invalid held `nu`.");
  if(nu.isNotNull())fixed.push_back("nu");
  auto res = cppunivariate::ged_mle(dist_impl::as_span(x), gradtol, iterlim, held_mean, held_sd, held_nu);
  if(res.status==cppunivariate::GedStatus::InvalidShape)Rcpp::stop("Held `nu` must be at least 1 under the existing GED restriction.");
  if(res.sigma==0 && (mean.isNotNull()||sd.isNotNull()||nu.isNotNull()))
    Rcpp::stop("GED scale likelihood is unbounded: all observations equal the fitted or held location.");
  if (res.status == cppunivariate::GedStatus::InvalidStart) {
    Rcpp::stop("GED MLE requires at least 2 finite observations with positive "
               "variance.");
  }
  if (res.status == cppunivariate::GedStatus::UniformBoundary) {
    Rcpp::stop("GED likelihood is maximised as nu -> Inf, i.e. by the uniform distribution. Use `mlunif` instead.");
  }
  if(res.status==cppunivariate::GedStatus::BfgsFailure)Rcpp::stop("Conditional GED search did not converge; increase iterlim.");
  if (!std::isfinite(res.mu) || !std::isfinite(res.sigma) ||
      !std::isfinite(res.nu)) {
    Rcpp::stop("GED MLE failed to converge (check inputs)");
  }
  Rcpp::CharacterVector restricted;
  if(res.restricted & 1u) restricted.push_back("lower","mean");
  if(res.restricted & 2u) restricted.push_back("upper","mean");
  if(res.restricted & 4u) restricted.push_back("lower","sd");
  if(res.restricted & 8u) restricted.push_back("upper","sd");
  if(res.restricted & 16u) restricted.push_back("lower","nu");
  if(res.restricted & 32u) restricted.push_back("upper","nu");
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["mean"] = res.mu, Rcpp::_["sd"] = res.sigma,
          Rcpp::_["nu"] = res.nu),
      Rcpp::_["restricted"]=restricted, Rcpp::_["fixed"] = fixed, Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
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
