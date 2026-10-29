// Rcpp adapter; assembled into the R package, outside the header-only core.
#include "as_span.h"
#include "cppunivariate/distributions/nbinom/mle.hpp"
#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlnbinom_(const Rcpp::IntegerVector &x, double reltol = 1.220703e-4,
                     int iterlim = 100, Rcpp::Nullable<double> size = R_NilValue) {
  const int n = x.size();
  if (n == 0) {
    Rcpp::stop("x must have positive length");
  }
  if (n > 0 && std::all_of(x.begin(), x.end(), [](int v) { return v == 0; })) {
    Rcpp::warning("All observations are 0; the maximum likelihood estimator is "
                  "not unique.");
  }
  double sum_x = 0.0;
  for (int v : x) sum_x += static_cast<double>(v);
  const double x_bar = sum_x / static_cast<double>(x.size());

  double size_out = NA_REAL;
  double prob_out = NA_REAL;
  double loglik_out = NA_REAL;
  int iterations_out = 0;

  if (size.isNotNull()) {
    size_out = Rcpp::as<double>(size);
    if (!(size_out > 0.0) || !std::isfinite(size_out)) {
      Rcpp::stop("`size` must be a positive finite number.");
    }
    prob_out = size_out / (x_bar + size_out);
    if (prob_out == 1.0 && x_bar > 0.0) {
      Rcpp::stop("Negative binomial MLE probability rounds to one: held size exceeds double precision for these data.");
    }
    double ll = 0.0;
    for (int v : x) {
      ll += cppunivariate::special::dnbinom(static_cast<double>(v), size_out, prob_out,
                                     /*log=*/true);
    }
    loglik_out = ll;
  } else {
    if (x_bar == 0.0) {
      size_out = 1.0;
      prob_out = 1.0;
      loglik_out = 0.0;
    } else {
      auto res = cppunivariate::nbinom_mle(dist_impl::as_ispan(x), reltol, iterlim);
      if (res.status == cppunivariate::NbinomStatus::Underdispersed) {
        Rcpp::stop("The maximum likelihood estimator does not exist for "
                   "underdispersed data, but converges to a Poisson. Use "
                   "`mlpois` instead.");
      }
      if (!std::isfinite(res.size) || !std::isfinite(res.prob)) {
        Rcpp::stop("Negative binomial MLE failed (check dispersion/inputs)");
      }
      size_out = res.size;
      prob_out = res.prob;
      loglik_out = res.logLik;
      iterations_out = res.iterations;
    }
  }

  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["size"] = size_out, Rcpp::_["prob"] = prob_out),
      Rcpp::_["logLik"] = loglik_out, Rcpp::_["iterations"] = iterations_out,
      Rcpp::_["fixed"] = size.isNotNull() ? Rcpp::CharacterVector::create("size")
                                          : Rcpp::CharacterVector(0));
}
