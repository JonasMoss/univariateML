#include "as_span.h"
#include "cppunivariate/distributions/ztbinom/mle.hpp"
#include <Rcpp.h>
// [[Rcpp::export]]
Rcpp::List mlztbinom_(const Rcpp::IntegerVector &x, Rcpp::Nullable<double> size, double reltol = 1e-10, int iterlim = 200, Rcpp::Nullable<double> prob = R_NilValue) {
  const double fixed_size=size.isNotNull()?Rcpp::as<double>(size):std::numeric_limits<double>::quiet_NaN();
  if(size.isNotNull() && !std::isfinite(fixed_size))Rcpp::stop("`size` must be finite and in its parameter domain.");
  const double held_prob=prob.isNotNull()?Rcpp::as<double>(prob):std::numeric_limits<double>::quiet_NaN();
  if(prob.isNotNull() && (!std::isfinite(held_prob)||held_prob<0||held_prob>1))Rcpp::stop("`prob` must be finite and in [0, 1].");
  const auto res=cppunivariate::ztbinom_mle(dist_impl::as_ispan(x), reltol, iterlim, fixed_size, held_prob);
  if(res.status==cppunivariate::ZtbinomStatus::InvalidInput)Rcpp::stop("requires nonempty positive integer count data and valid controls; held parameters must give positive likelihood");
  if(res.status==cppunivariate::ZtbinomStatus::InvalidSize)Rcpp::stop("`size` must be a positive integer at least max(x).");
  if(res.status==cppunivariate::ZtbinomStatus::PoissonLimit)Rcpp::stop("The maximum likelihood estimator escapes to infinite size. Use `mlztpois`.");
  if(res.status==cppunivariate::ZtbinomStatus::IterationFailure)Rcpp::stop("Profile optimizer IterationFailure: iteration limit or floating-point resolution exhausted; increase iterlim. No capped size estimate is returned.");
  if(!std::isfinite(res.logLik))Rcpp::stop("MLE cannot be resolved at floating-point precision.");
  Rcpp::CharacterVector fixed(0);
  if(size.isNotNull())fixed.push_back("size");
  if(prob.isNotNull())fixed.push_back("prob");
  return Rcpp::List::create(Rcpp::_["estimates"]=Rcpp::NumericVector::create(Rcpp::_["size"]=res.size,Rcpp::_["prob"]=res.prob),
    Rcpp::_["logLik"]=res.logLik,Rcpp::_["iterations"]=res.iterations,
    Rcpp::_["fixed"]=fixed);
}
