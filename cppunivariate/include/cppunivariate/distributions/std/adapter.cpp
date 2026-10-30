// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/std/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlstd_(const Rcpp::NumericVector &x, double gradtol = 1.220703e-4,
                  int iterlim = 200, Rcpp::Nullable<double> nu = R_NilValue, Rcpp::Nullable<double> mean = R_NilValue, Rcpp::Nullable<double> sd = R_NilValue) {
  if(nu.isNotNull()&&!std::isfinite(Rcpp::as<double>(nu)))Rcpp::stop("`nu` must be finite and greater than 2.");
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double held_mean=mean.isNotNull()?Rcpp::as<double>(mean):nan;
  if(mean.isNotNull() && (!std::isfinite(held_mean)))Rcpp::stop("Invalid held `mean`.");
  const double held_sd=sd.isNotNull()?Rcpp::as<double>(sd):nan;
  if(sd.isNotNull() && (!std::isfinite(held_sd) || !(held_sd>0)))Rcpp::stop("Invalid held `sd`.");
  Rcpp::CharacterVector fixed;
  if(mean.isNotNull())fixed.push_back("mean");
  if(sd.isNotNull())fixed.push_back("sd");
  if(nu.isNotNull())fixed.push_back("nu");
  auto res=cppunivariate::student_t_mle(dist_impl::as_span(x),gradtol,iterlim,held_mean,held_sd,nu.isNotNull()?Rcpp::as<double>(nu):nan);
  if (res.status == cppunivariate::StudentTStatus::InvalidNu)
    Rcpp::stop("`nu` must be finite and greater than 2.");
  if (res.status == cppunivariate::StudentTStatus::Unbounded)
    Rcpp::stop("Likelihood is unbounded at a tie of excessive multiplicity k > nu*(n-k) (free df: k > 2*(n-k)); no MLE.");
  if (res.status == cppunivariate::StudentTStatus::ScaleBoundary)
    Rcpp::stop("Tie equality scale-collapse boundary; a regular MLE is not certified.");
  if (res.status == cppunivariate::StudentTStatus::IterationFailure)
    Rcpp::stop("Student-t optimizer IterationFailure: did not converge to the score tolerance; increase iterlim.");
  if (res.status == cppunivariate::StudentTStatus::NormalBoundary) {
    Rcpp::stop("The Student-t likelihood is maximised as nu -> Inf, i.e. by the "
               "normal distribution. Use `mlnorm` to fit this model.");
  }
  if (res.status == cppunivariate::StudentTStatus::DfTwoBoundary) {
    Rcpp::stop("The Student-t likelihood is maximised as nu -> 2 (infinite "
               "variance); the MLE does not exist in the mean/sd parameterisation. "
               "Use `mllst` to fit a location-scale t distribution with free degrees of freedom.");
  }
  if (res.status == cppunivariate::StudentTStatus::BfgsFailure)
    Rcpp::stop("StudentT optimizer BfgsFailure: score check failed; increase iterlim.");
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
      Rcpp::_["fixed"] = fixed);
}
