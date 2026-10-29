// R shell adapter.
#include "as_span.h"
#include "cppunivariate/distributions/zinb/mle.hpp"
#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::List mlzinb_(const Rcpp::IntegerVector &x, double reltol = 1e-10,
                   int iterlim = 200, Rcpp::Nullable<double> size = R_NilValue, Rcpp::Nullable<double> prob = R_NilValue, Rcpp::Nullable<double> pi = R_NilValue) {
  if(prob.isNotNull() && !std::isfinite(Rcpp::as<double>(prob)))Rcpp::stop("`prob` must be finite and in its parameter domain.");
  if(pi.isNotNull() && !std::isfinite(Rcpp::as<double>(pi)))Rcpp::stop("`pi` must be finite and in its parameter domain.");

  if (size.isNotNull() && (!std::isfinite(Rcpp::as<double>(size)) || Rcpp::as<double>(size) <= 0))
    Rcpp::stop("size must be a finite positive number");
  const auto fit = cppunivariate::zinb_mle(dist_impl::as_ispan(x), reltol, iterlim,
      size.isNotNull() ? Rcpp::as<double>(size) : NA_REAL,prob.isNotNull()?Rcpp::as<double>(prob):std::numeric_limits<double>::quiet_NaN(),pi.isNotNull()?Rcpp::as<double>(pi):std::numeric_limits<double>::quiet_NaN());
  if(fit.status==cppunivariate::ZinbStatus::ZeroLimit)Rcpp::stop("With prob and pi held below one and all-zero data, size tends to zero; no MLE on the current positive-size domain. Use `mlpois`.");
  if (fit.status == cppunivariate::ZinbStatus::InvalidSize)
    Rcpp::stop("size must be a finite positive number");
  if (fit.status == cppunivariate::ZinbStatus::InvalidInput)
    Rcpp::stop("ZINB requires nonempty nonnegative count data, positive reltol and iterlim, prob in (0,1], pi in [0,1], and compatible holds");
  if (fit.status == cppunivariate::ZinbStatus::ZipLimit)
    Rcpp::stop("The maximum likelihood estimator does not exist, but converges to a zero-inflated Poisson. Use `mlzip` instead.");
  if (fit.status == cppunivariate::ZinbStatus::PoissonLimit)
    Rcpp::stop("The maximum likelihood estimator does not exist, but converges to a Poisson. Use `mlpois` instead.");
  if (fit.status != cppunivariate::ZinbStatus::Ok)
    Rcpp::stop("Zero-inflated negative binomial MLE failed");
  if (fit.prob == 1)
    Rcpp::warning("All observations are 0; the maximum likelihood estimator is not unique.");
  Rcpp::CharacterVector fixed(0);
  if(size.isNotNull())fixed.push_back("size");
  if(prob.isNotNull())fixed.push_back("prob");
  if(pi.isNotNull())fixed.push_back("pi");
  return Rcpp::List::create(
    Rcpp::_["estimates"] = Rcpp::NumericVector::create(Rcpp::_["size"] = fit.size,
      Rcpp::_["prob"] = fit.prob, Rcpp::_["pi"] = fit.pi),
    Rcpp::_["logLik"] = fit.logLik, Rcpp::_["iterations"] = fit.iterations,
    Rcpp::_["fixed"] = fixed);
}
