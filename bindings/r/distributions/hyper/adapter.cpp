// Rcpp adapter; assembled into the R package, outside the header-only core.
#include "as_span.h"
#include "cppunivariate/distributions/hyper/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <iomanip>
#include <limits>
#include <sstream>
#include <string>

namespace {

double held_value(const Rcpp::Nullable<double> &value, const char *name) {
  if (value.isNull()) return std::numeric_limits<double>::quiet_NaN();
  const double v = Rcpp::as<double>(value);
  if (std::isnan(v)) Rcpp::stop("`%s` must be a whole number or NULL, not NA.", name);
  return v;
}

std::string whole(double v) {
  std::ostringstream out;
  out << std::fixed << std::setprecision(0) << v;
  return out.str();
}

}  // namespace

// [[Rcpp::export]]
Rcpp::List mlhyper_(const Rcpp::NumericVector &x, Rcpp::Nullable<double> k,
                    Rcpp::Nullable<double> N,
                    Rcpp::Nullable<double> m = R_NilValue) {
  if (N.isNull() && m.isNull() && k.isNull())
    Rcpp::stop("Joint estimation of m, N and k is not implemented: the interior and binomial and Poisson limit faces require a certified global comparison.");
  if (N.isNull() && m.isNull()) Rcpp::stop("Joint estimation of N and m is not implemented; it can escape to the binomial limit, for which use mlbinom(x, size = k).");
  if (k.isNull() && N.isNull()) Rcpp::stop("Joint estimation with k free is not supported by this fitter.");
  const double design=held_value(k, "k");
  const double N_held = held_value(N, "N"), m_held = held_value(m, "m");
  const auto res = cppunivariate::hyper_mle(dist_impl::as_span(x), design, N_held, m_held);
  const char *estimated = k.isNull() ? "k" : (N.isNull() ? "N" : "m");
  switch (res.status) {
    case cppunivariate::HyperStatus::NumericalFailure:
      Rcpp::stop("Finite-grid numerical failure: 1e7 candidate evaluation budget or representability/nonfinite likelihood limit exceeded.");
    case cppunivariate::HyperStatus::BothFree:
      Rcpp::stop("Supply the population size `N` or the number of successes `m` "
                 "as well as `k`: estimating both is not supported. As `N` grows "
                 "with `m / N` fixed the model tends to the binomial; use "
                 "`mlbinom(x, size = k)`.");
    case cppunivariate::HyperStatus::InvalidInput:
      Rcpp::stop("`k`, `N` and `m` must be whole numbers from 0 to 2^53 - 1, with "
                 "`k <= N` and `m <= N`; so must the observations.");
    case cppunivariate::HyperStatus::Infeasible:
      Rcpp::stop("No admissible value of `%s` gives every observation positive "
                 "probability: observations must lie between max(0, k - (N - m)) "
                 "and min(k, m).", estimated);
    case cppunivariate::HyperStatus::AllZero:
      Rcpp::stop("All observations are 0: with `m` and `k` positive the likelihood "
                 "increases toward a point mass at zero as `N` grows, so the "
                 "maximum likelihood estimator of `N` does not exist.");
    case cppunivariate::HyperStatus::TooLarge:
      Rcpp::stop("The required population size or likelihood maximiser exceeds "
                 "2^53 - 1; double precision does not represent every integer "
                 "beyond that, so the "
                 "estimate of `N` cannot be computed exactly.");
    case cppunivariate::HyperStatus::Tie:
      Rcpp::warning("The likelihood is tied (to rounding) between `%s` = %s and %s; "
                    "returning the smaller.", estimated,
                    whole(k.isNull() ? res.k : (N.isNull() ? res.N : res.m)), whole(res.tie));
      break;
    case cppunivariate::HyperStatus::Unidentified:
      if (std::isinf(res.tie)) {
        Rcpp::warning("Every `N` from %s upwards attains the maximum likelihood "
                      "(`k = 0` or `m = 0`); returning `N` = %s.",
                      whole(res.N), whole(res.N));
      } else if (k.isNull()) {
        Rcpp::warning("Every `k` from %s to %s attains the maximum likelihood; returning the smaller.", whole(res.k), whole(res.tie));
      } else {
        Rcpp::warning("With `k = 0` every `m` from %s to %s attains the maximum "
                      "likelihood; returning `m` = %s.", whole(res.m),
                      whole(res.tie), whole(res.m));
      }
      break;
    case cppunivariate::HyperStatus::Ok:
      break;
  }
  Rcpp::CharacterVector fixed;
  if (m.isNotNull()) fixed.push_back("m");
  if (N.isNotNull()) fixed.push_back("N");
  if (k.isNotNull()) fixed.push_back("k");
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["m"] = res.m, Rcpp::_["N"] = res.N, Rcpp::_["k"] = res.k),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations,
      Rcpp::_["fixed"] = fixed);
}
