// Rcpp adapter; assembled into the R package, outside the header-only core.
#include "as_span.h"
#include "cppunivariate/distributions/binom/mle.hpp"
#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlbinom_(const Rcpp::IntegerVector &x,
                    Rcpp::Nullable<double> size = R_NilValue,
                    double abstol = 1.220703e-4, int iterlim = 100) {
  const double size_or_nan = size.isNotNull()
                                 ? Rcpp::as<double>(size)
                                 : std::numeric_limits<double>::quiet_NaN();
  // The fitter reads a non-finite size as "estimate it"; a held size that
  // is NA, NaN, infinite or fractional must not slip through that way.
  if (size.isNotNull() && (!std::isfinite(size_or_nan) || size_or_nan < 0.0 ||
                           size_or_nan != std::floor(size_or_nan))) {
    Rcpp::stop("`size` must be a nonnegative integer.");
  }
  auto res = cppunivariate::binom_mle(dist_impl::as_ispan(x), size_or_nan, abstol, iterlim);
  if (res.status == cppunivariate::BinomStatus::SizeTooSmall) {
    Rcpp::stop("`size` is smaller than `max(x)`.");
  }
  if (res.status == cppunivariate::BinomStatus::Underdispersed) {
    Rcpp::stop(
        "The maximum likelihood estimator does not exist. Use `mlpois` to fit "
        "a Poisson or supply a `size` argument.");
  }
  if (res.status != cppunivariate::BinomStatus::Ok || !std::isfinite(res.size)) {
    Rcpp::stop("Binomial MLE failed (check dispersion/inputs)");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["size"] = res.size, Rcpp::_["prob"] = res.prob),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations,
      Rcpp::_["fixed"] = size.isNotNull() ? Rcpp::CharacterVector::create("size")
                                          : Rcpp::CharacterVector(0));
}
