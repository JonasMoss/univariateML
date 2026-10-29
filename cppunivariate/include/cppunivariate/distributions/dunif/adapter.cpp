// Rcpp adapter; assembled into the R package, outside the header-only core.
#include "as_span.h"
#include "cppunivariate/distributions/dunif/mle.hpp"
#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mldunif_(const Rcpp::IntegerVector &x,
    Rcpp::Nullable<double> min=R_NilValue,Rcpp::Nullable<double> max=R_NilValue) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double a=min.isNull()?nan:Rcpp::as<double>(min), b=max.isNull()?nan:Rcpp::as<double>(max);
  if ((min.isNotNull()&&!std::isfinite(a)) || (max.isNotNull()&&!std::isfinite(b)))
    Rcpp::stop("Held endpoints must be finite integers.");
  auto res = cppunivariate::dunif_mle(
      cppunivariate::ispan{x.begin(), static_cast<std::size_t>(x.size())},a,b);
  if (res.status!=cppunivariate::DiscreteUnifStatus::Ok) Rcpp::stop("Held endpoints must be integers covering every observation.");
  Rcpp::CharacterVector fixed(0);
  if(min.isNotNull())fixed.push_back("min");
  if(max.isNotNull())fixed.push_back("max");
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["min"] = res.min, Rcpp::_["max"] = res.max),
      Rcpp::_["logLik"] = res.logLik,Rcpp::_["fixed"]=fixed);
}
