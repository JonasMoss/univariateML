#include "as_span.h"
#include "cppunivariate/distributions/zmnbinom/mle.hpp"
#include <Rcpp.h>
// [[Rcpp::export]]
Rcpp::List mlzmnbinom_(const Rcpp::IntegerVector &x, Rcpp::Nullable<double> size = R_NilValue, double reltol = 1e-10, int iterlim = 200, Rcpp::Nullable<double> prob = R_NilValue, Rcpp::Nullable<double> p0 = R_NilValue) {
  if(prob.isNotNull() && !std::isfinite(Rcpp::as<double>(prob)))Rcpp::stop("`prob` must be finite and in its parameter domain.");
  if(p0.isNotNull() && !std::isfinite(Rcpp::as<double>(p0)))Rcpp::stop("`p0` must be finite and in its parameter domain.");

  const double fixed_size=size.isNotNull()?Rcpp::as<double>(size):std::numeric_limits<double>::quiet_NaN();
  if(size.isNotNull() && !std::isfinite(fixed_size))Rcpp::stop("`size` must be finite and in its parameter domain.");
  const auto res=cppunivariate::zmnbinom_mle(dist_impl::as_ispan(x), reltol, iterlim, fixed_size,prob.isNotNull()?Rcpp::as<double>(prob):std::numeric_limits<double>::quiet_NaN(),p0.isNotNull()?Rcpp::as<double>(p0):std::numeric_limits<double>::quiet_NaN());
  if(res.status==cppunivariate::ZmnbinomStatus::InvalidInput)Rcpp::stop("requires nonempty nonnegative integer count data and valid controls and parameter domains; held values must be compatible with the observations");
  if(res.status==cppunivariate::ZmnbinomStatus::InvalidSize)Rcpp::stop("`size` must be a nonnegative finite value.");
  if(res.status==cppunivariate::ZmnbinomStatus::PoissonLimit)Rcpp::stop("The maximum likelihood estimator escapes to infinite size. Use `mlzmpois`.");
  if(res.status==cppunivariate::ZmnbinomStatus::AllZero)Rcpp::warning("all observations are zero: base parameters are unidentified; supplied values are preserved.");
  if(res.status==cppunivariate::ZmnbinomStatus::TwoPoint)Rcpp::warning("positives are all one: the attained two-point law is also fitted exactly by mlbinom(x, size = 1).");
  if(res.status==cppunivariate::ZmnbinomStatus::IterationFailure)Rcpp::stop("Profile search exhausted floating-point resolution; no capped size estimate is returned.");
  if(!std::isfinite(res.logLik))Rcpp::stop("MLE cannot be resolved at floating-point precision.");
  Rcpp::CharacterVector fixed(0);
  if(size.isNotNull())fixed.push_back("size");
  if(prob.isNotNull())fixed.push_back("prob");
  if(p0.isNotNull())fixed.push_back("p0");
  return Rcpp::List::create(Rcpp::_["estimates"]=Rcpp::NumericVector::create(Rcpp::_["size"]=res.size,Rcpp::_["prob"]=res.prob,Rcpp::_["p0"]=res.p0),
    Rcpp::_["logLik"]=res.logLik,Rcpp::_["iterations"]=res.iterations,
    Rcpp::_["fixed"]=fixed);
}
