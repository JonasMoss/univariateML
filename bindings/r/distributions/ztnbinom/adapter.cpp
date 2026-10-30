#include "as_span.h"
#include "cppunivariate/distributions/ztnbinom/mle.hpp"
#include <Rcpp.h>
// [[Rcpp::export]]
Rcpp::List mlztnbinom_(const Rcpp::IntegerVector &x, Rcpp::Nullable<double> size = R_NilValue, double reltol = 1e-10, int iterlim = 200, Rcpp::Nullable<double> prob = R_NilValue) {
  if(prob.isNotNull() && !std::isfinite(Rcpp::as<double>(prob)))Rcpp::stop("`prob` must be finite and in its parameter domain.");

  const double fixed_size=size.isNotNull()?Rcpp::as<double>(size):std::numeric_limits<double>::quiet_NaN();
  if(size.isNotNull() && !std::isfinite(fixed_size))Rcpp::stop("`size` must be finite and in its parameter domain.");
  const auto res=cppunivariate::ztnbinom_mle(dist_impl::as_ispan(x), reltol, iterlim, fixed_size,prob.isNotNull()?Rcpp::as<double>(prob):std::numeric_limits<double>::quiet_NaN());
  if(res.status==cppunivariate::ZtnbinomStatus::InvalidInput)Rcpp::stop("requires nonempty positive integer count data and valid controls and parameter domains; held values must be compatible with the observations");
  if(res.status==cppunivariate::ZtnbinomStatus::InvalidSize)Rcpp::stop("`size` must be a nonnegative finite value.");
  if(res.status==cppunivariate::ZtnbinomStatus::PoissonLimit)Rcpp::stop("The maximum likelihood estimator escapes to infinite size. Use `mlztpois`.");
  if(res.status==cppunivariate::ZtnbinomStatus::IterationFailure)Rcpp::stop("Profile search exhausted floating-point resolution; no capped size estimate is returned.");
  if(!std::isfinite(res.logLik))Rcpp::stop("MLE cannot be resolved at floating-point precision.");
  Rcpp::CharacterVector fixed(0);
  if(size.isNotNull())fixed.push_back("size");
  if(prob.isNotNull())fixed.push_back("prob");
  return Rcpp::List::create(Rcpp::_["estimates"]=Rcpp::NumericVector::create(Rcpp::_["size"]=res.size,Rcpp::_["prob"]=res.prob),
    Rcpp::_["logLik"]=res.logLik,Rcpp::_["iterations"]=res.iterations,
    Rcpp::_["fixed"]=fixed);
}
