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
  if(res.status==cppunivariate::SstdStatus::UnsupportedHold) Rcpp::stop("Unsupported conditional fit.");
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
    if(mean.isNotNull() || sd.isNotNull())
      Rcpp::stop("The skew Student-t likelihood is maximised as xi -> %s by a translated half-t "
                 "with %.4g degrees of freedom and endpoint constrained by held moments; "
                 "the MLE does not exist. Use `mlht` on signed distances from this endpoint.",
                 right ? "Inf" : "0",res.nu);
    Rcpp::stop("The skew Student-t likelihood is maximised as xi -> %s, i.e. by a "
               "translated half-t with %.4g degrees of freedom and endpoint at the "
               "sample %s; the MLE does not exist. Use `mlht` on signed distances from this endpoint.",
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
  if(res.status==cppunivariate::SstdStatus::BfgsFailure)
    Rcpp::stop("Skew fit optimizer failed its free-coordinate score check; increase `iterlim`.");
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
