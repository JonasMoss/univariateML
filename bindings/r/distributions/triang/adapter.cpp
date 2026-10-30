// Thin R shell: all subset fitting is in the core.
#include "as_span.h"
#include "cppunivariate/distributions/triang/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mltriang_(const Rcpp::NumericVector &x,
    Rcpp::Nullable<double> a = R_NilValue,
    Rcpp::Nullable<double> b = R_NilValue,
    Rcpp::Nullable<double> c = R_NilValue,
    double reltol = 1e-12, int iterlim = 100) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  auto held = [&](Rcpp::Nullable<double> z) {
    if(z.isNull()) return nan;
    const double v=Rcpp::as<double>(z);
    if(!std::isfinite(v)) Rcpp::stop("Fixed parameters must be finite.");
    return v;
  };
  auto r=cppunivariate::triang_mle(dist_impl::as_span(x),reltol,iterlim,held(a),held(b),held(c));
  if(r.status==cppunivariate::TriangStatus::Unbounded)
    Rcpp::stop("Triangular likelihood is unbounded for constant data with these fixed parameters.");
  if(r.status==cppunivariate::TriangStatus::Incompatible)
    Rcpp::stop("Data incompatible with held triangular parameters (outside a held endpoint or zero likelihood).");
  if(r.status==cppunivariate::TriangStatus::InvalidInput)
    Rcpp::stop("Invalid triangular inputs: require finite data, a < b, a <= c <= b, positive reltol and iterlim.");
  if(r.status==cppunivariate::TriangStatus::IterationFailure)
    Rcpp::stop("Triangular endpoint solve did not converge; increase iterlim.");
  if(r.not_unique) Rcpp::warning("Triangular maximum likelihood estimate is not unique (candidate log likelihoods within tolerance).");
  Rcpp::CharacterVector fixed(0);
  if(a.isNotNull()) fixed.push_back("a");
  if(b.isNotNull()) fixed.push_back("b");
  if(c.isNotNull()) fixed.push_back("c");
  return Rcpp::List::create(
    Rcpp::_["estimates"]=Rcpp::NumericVector::create(Rcpp::_["a"]=r.a,Rcpp::_["b"]=r.b,Rcpp::_["c"]=r.c),
    Rcpp::_["logLik"]=r.logLik,Rcpp::_["iterations"]=r.iterations,Rcpp::_["fixed"]=fixed);
}
