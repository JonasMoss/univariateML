#include "as_span.h"
#include "cppunivariate/distributions/zmnbinom/mle.hpp"
#include <Rcpp.h>
// [[Rcpp::export]]
Rcpp::List mlzmnbinom_(const Rcpp::IntegerVector &x, Rcpp::Nullable<double> size = R_NilValue, double reltol = 1e-10, int iterlim = 200) {
  const double fixed_size=size.isNotNull()?Rcpp::as<double>(size):std::numeric_limits<double>::quiet_NaN();
  if(size.isNotNull() && !std::isfinite(fixed_size))Rcpp::stop("`size` must be finite and in its parameter domain.");
  const auto res=cppunivariate::zmnbinom_mle(dist_impl::as_ispan(x), reltol, iterlim, fixed_size);
  if(res.status==cppunivariate::ZmnbinomStatus::InvalidInput)Rcpp::stop("requires nonempty nonnegative integer count data and valid controls");
  if(res.status==cppunivariate::ZmnbinomStatus::InvalidSize)Rcpp::stop("`size` must be a nonnegative finite value.");
  if(res.status==cppunivariate::ZmnbinomStatus::PoissonLimit)Rcpp::stop("The maximum likelihood estimator escapes to infinite size. Use `mlzmpois`.");
  if(res.status==cppunivariate::ZmnbinomStatus::AllZero)Rcpp::warning("all observations are zero: base parameters are unidentified; returning p0 = 1.");
  if(res.status==cppunivariate::ZmnbinomStatus::TwoPoint)Rcpp::warning("positives are all one: the attained two-point law is also fitted exactly by mlbinom(x, size = 1).");
  if(res.status==cppunivariate::ZmnbinomStatus::IterationFailure)Rcpp::stop("Profile search exhausted floating-point resolution; no capped size estimate is returned.");
  if(!std::isfinite(res.logLik))Rcpp::stop("MLE cannot be resolved at floating-point precision.");
  return Rcpp::List::create(Rcpp::_["estimates"]=Rcpp::NumericVector::create(Rcpp::_["size"]=res.size,Rcpp::_["prob"]=res.prob,Rcpp::_["p0"]=res.p0),
    Rcpp::_["logLik"]=res.logLik,Rcpp::_["iterations"]=res.iterations,
    Rcpp::_["fixed"]=size.isNotNull()?Rcpp::CharacterVector::create("size"):Rcpp::CharacterVector(0));
}
