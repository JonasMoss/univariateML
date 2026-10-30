// Rcpp adapter; assembled into the R package, outside the header-only core.
#include "as_span.h"
#include "cppunivariate/distributions/nbinom/mle.hpp"
#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlnbinom_(const Rcpp::IntegerVector &x, double reltol = 1.220703e-4,
                     int iterlim = 100, Rcpp::Nullable<double> size = R_NilValue, Rcpp::Nullable<double> prob = R_NilValue) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  if ((size.isNotNull() && !std::isfinite(Rcpp::as<double>(size))) ||
      (prob.isNotNull() && !std::isfinite(Rcpp::as<double>(prob))))
    Rcpp::stop("Held size and prob must be finite.");
  if(size.isNotNull() && prob.isNull()) {
    const double held=Rcpp::as<double>(size);
    double total=0;for(int v:x)total+=v;
    if(held>0 && total>0 && held/(held+total/x.size())==1)
      Rcpp::stop("Negative binomial MLE probability rounds to one: held size exceeds double precision for these data.");
  }
  const auto r=cppunivariate::nbinom_mle(dist_impl::as_ispan(x),reltol,iterlim,
      size.isNotNull()?Rcpp::as<double>(size):nan,prob.isNotNull()?Rcpp::as<double>(prob):nan);
  if(r.status==cppunivariate::NbinomStatus::Underdispersed)
    Rcpp::stop("The maximum likelihood estimator does not exist for underdispersed data, but converges to a Poisson. Use `mlpois` instead.");
  if(r.status!=cppunivariate::NbinomStatus::Ok || !std::isfinite(r.logLik))
    Rcpp::stop("Negative binomial MLE failed: invalid or incompatible holds, inputs or convergence.");
  Rcpp::CharacterVector fixed;
  if(size.isNotNull())fixed.push_back("size");
  if(prob.isNotNull())fixed.push_back("prob");
  return Rcpp::List::create(Rcpp::_["estimates"]=Rcpp::NumericVector::create(
      Rcpp::_["size"]=r.size,Rcpp::_["prob"]=r.prob),Rcpp::_["logLik"]=r.logLik,
      Rcpp::_["iterations"]=r.iterations,Rcpp::_["fixed"]=fixed);
}
