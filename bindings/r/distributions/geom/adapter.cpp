// Rcpp adapter; assembled into the R package, outside the header-only core.
#include "as_span.h"
#include "cppunivariate/distributions/geom/mle.hpp"
#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlgeom_(const Rcpp::IntegerVector &x, Rcpp::Nullable<double> prob = R_NilValue) {
  if(prob.isNotNull() && !std::isfinite(Rcpp::as<double>(prob)))Rcpp::stop("`prob` must be finite and in its parameter domain.");

  auto res = cppunivariate::geom_mle(
      cppunivariate::ispan{x.begin(), static_cast<std::size_t>(x.size())}, prob.isNotNull()?Rcpp::as<double>(prob):std::numeric_limits<double>::quiet_NaN());
  if(!std::isfinite(res.logLik))Rcpp::stop("Held prob is outside its domain or incompatible with the observations.");
  Rcpp::CharacterVector fixed(0);if(prob.isNotNull())fixed.push_back("prob");
  return Rcpp::List::create(Rcpp::_["estimates"] = Rcpp::NumericVector::create(
                                Rcpp::_["prob"] = res.prob),
                            Rcpp::_["logLik"] = res.logLik, Rcpp::_["fixed"]=fixed);
}
