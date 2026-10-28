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
    Rcpp::stop("Fixed `nu` likelihood is unbounded through scale collapse: excessive ties.");
  if (res.status == cppunivariate::StudentTStatus::ScaleBoundary)
    Rcpp::stop("Fixed `nu` is at the tie equality scale-collapse boundary; a regular MLE is not certified.");
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
