// R shell adapter.
#include "as_span.h"
#include "cppunivariate/distributions/zinb/mle.hpp"
#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::List mlzinb_(const Rcpp::IntegerVector &x, double reltol = 1e-10,
                   int iterlim = 200, Rcpp::Nullable<double> size = R_NilValue) {
  if (size.isNotNull() && (!std::isfinite(Rcpp::as<double>(size)) || Rcpp::as<double>(size) <= 0))
    Rcpp::stop("size must be a finite positive number");
  const auto fit = cppunivariate::zinb_mle(dist_impl::as_ispan(x), reltol, iterlim,
      size.isNotNull() ? Rcpp::as<double>(size) : NA_REAL);
  if (fit.status == cppunivariate::ZinbStatus::InvalidSize)
    Rcpp::stop("size must be a finite positive number");
  if (fit.status == cppunivariate::ZinbStatus::InvalidInput)
    Rcpp::stop("ZINB requires nonempty nonnegative count data, positive reltol and iterlim");
  if (fit.status == cppunivariate::ZinbStatus::ZipLimit)
    Rcpp::stop("The maximum likelihood estimator does not exist, but converges to a zero-inflated Poisson. Use `mlzip` instead.");
  if (fit.status == cppunivariate::ZinbStatus::PoissonLimit)
    Rcpp::stop("The maximum likelihood estimator does not exist, but converges to a Poisson. Use `mlpois` instead.");
  if (fit.status != cppunivariate::ZinbStatus::Ok)
    Rcpp::stop("Zero-inflated negative binomial MLE failed");
  if (fit.prob == 1)
    Rcpp::warning("All observations are 0; the maximum likelihood estimator is not unique.");
  return Rcpp::List::create(
    Rcpp::_["estimates"] = Rcpp::NumericVector::create(Rcpp::_["size"] = fit.size,
      Rcpp::_["prob"] = fit.prob, Rcpp::_["pi"] = fit.pi),
    Rcpp::_["logLik"] = fit.logLik, Rcpp::_["iterations"] = fit.iterations,
    Rcpp::_["fixed"] = size.isNotNull() ? Rcpp::CharacterVector::create("size") : Rcpp::CharacterVector(0));
}
