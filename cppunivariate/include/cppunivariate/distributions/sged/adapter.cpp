// Rcpp adapter; assembled into the R package, outside the header-only core.
#include "as_span.h"
#include "cppunivariate/distributions/sged/mle.hpp"
#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlsged_(const Rcpp::NumericVector &x,
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
  if(nu.isNotNull() && (!(std::isfinite(held_nu) && held_nu>0))) Rcpp::stop("Invalid held `nu`: requires a finite value in its parameter domain.");
  const double held_xi=xi.isNotNull() ? Rcpp::as<double>(xi) : nan;
  if(xi.isNotNull() && (!(std::isfinite(held_xi) && held_xi>0))) Rcpp::stop("Invalid held `xi`: requires a finite value in its parameter domain.");
  auto res=cppunivariate::sged_mle(dist_impl::as_span(x),gradtol,iterlim,held_mean,held_sd,held_nu,held_xi);
  if(res.status==cppunivariate::SgedStatus::UnsupportedHold) Rcpp::stop("Unsupported conditional fit.");
  if(res.status==cppunivariate::SgedStatus::Unbounded) Rcpp::stop("Scale collapse: likelihood is unbounded at the held mean; no MLE.");
  if(res.status==cppunivariate::SgedStatus::ScaleBoundary) Rcpp::stop("Scale-collapse or constrained uniform endpoint boundary could not be certified; no finite MLE returned.");
  if (res.status == cppunivariate::SgedStatus::InvalidStart) {
    Rcpp::stop("Skew GED MLE requires at least 2 finite observations with "
               "positive variance.");
  }
  if (res.status == cppunivariate::SgedStatus::UniformBoundary) {
    Rcpp::stop("SGED likelihood is maximised as nu -> Inf, i.e. by the uniform distribution. Use `mlunif` instead.");
  }
  if (res.status == cppunivariate::SgedStatus::RightHalfGedBoundary) {
    if(mean.isNotNull() || sd.isNotNull())
      Rcpp::stop("The likelihood is maximised by a translated half-GED with endpoint constrained by the held moments; no MLE in the skewed family.");
    Rcpp::stop("The likelihood is maximised by a translated half-GED with mode at the sample minimum; no MLE in the skewed family.");
  }
  if (res.status == cppunivariate::SgedStatus::LeftHalfGedBoundary) {
    if(mean.isNotNull() || sd.isNotNull())
      Rcpp::stop("The likelihood is maximised by a translated half-GED with reflected endpoint constrained by the held moments; no MLE in the skewed family.");
    Rcpp::stop("The likelihood is maximised by a translated half-GED with mode at the sample maximum; no MLE in the skewed family.");
  }
  if (!std::isfinite(res.mu) || !std::isfinite(res.sigma) ||
      !std::isfinite(res.nu) || !std::isfinite(res.xi)) {
    Rcpp::stop("Skew GED MLE failed to converge (check inputs)");
  }
  if(res.status==cppunivariate::SgedStatus::BfgsFailure) Rcpp::stop("Skew fit optimizer failed its score check; increase `iterlim`.");
  Rcpp::CharacterVector fixed;
  if(mean.isNotNull()) fixed.push_back("mean");
  if(sd.isNotNull()) fixed.push_back("sd");
  if(nu.isNotNull()) fixed.push_back("nu");
  if(xi.isNotNull()) fixed.push_back("xi");
  Rcpp::CharacterVector restricted;
  if(res.restricted & 1u) restricted.push_back("lower","mean");
  if(res.restricted & 2u) restricted.push_back("upper","mean");
  if(res.restricted & 4u) restricted.push_back("lower","sd");
  if(res.restricted & 8u) restricted.push_back("upper","sd");
  if(res.restricted & 16u) restricted.push_back("lower","nu");
  if(res.restricted & 32u) restricted.push_back("upper","nu");
  if(res.restricted & 64u) restricted.push_back("lower","xi");
  if(res.restricted & 128u) restricted.push_back("upper","xi");
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["mean"] = res.mu, Rcpp::_["sd"] = res.sigma,
          Rcpp::_["nu"] = res.nu, Rcpp::_["xi"] = res.xi),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations,
      Rcpp::_["restricted"]=restricted, Rcpp::_["fixed"] = fixed);
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
