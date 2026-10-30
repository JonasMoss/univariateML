#include <Rcpp.h>
#include "as_span.h"
#include "cppunivariate/distributions/cmb/mle.hpp"
#include <limits>
// [[Rcpp::export]]
Rcpp::List mlcmb_(const Rcpp::IntegerVector &x,Rcpp::Nullable<double> size,
                  Rcpp::Nullable<double> prob=R_NilValue,
                  Rcpp::Nullable<double> nu=R_NilValue,
                  double reltol=1e-10,int iterlim=100) {
  if(size.isNull())Rcpp::stop("Free size awaits resolution of the P0 reference-cap assumption (1e7 is an allocation limit).");
  const double trials=Rcpp::as<double>(size);
  if (!std::isfinite(trials) || trials<0 || trials>1e7 || trials!=std::floor(trials))
    Rcpp::stop("`size` must be a nonnegative integer at most 1e7.");
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double p=prob.isNotNull() ? Rcpp::as<double>(prob) : nan;
  const double v=nu.isNotNull() ? Rcpp::as<double>(nu) : nan;
  if (prob.isNotNull() && !(std::isfinite(p) && p>=0 && p<=1))
    Rcpp::stop("fixed `prob` must be between zero and one inclusive.");
  if (nu.isNotNull() && !std::isfinite(v)) Rcpp::stop("fixed `nu` must be finite.");
  const auto res=cppunivariate::cmb_mle(dist_impl::as_ispan(x),static_cast<int>(trials),reltol,iterlim,p,v);
  switch (res.status) {
    case cppunivariate::CmbStatus::Ok: break;
    case cppunivariate::CmbStatus::PointMass:
      Rcpp::stop("The MLE does not exist: the likelihood approaches a point mass.");
    case cppunivariate::CmbStatus::AdjacentLimit:
      Rcpp::stop("The MLE does not exist: the likelihood approaches an adjacent two-point law as nu tends to positive infinity.");
    case cppunivariate::CmbStatus::EndpointLimit:
      Rcpp::stop("The MLE does not exist: the likelihood approaches an endpoint two-point law as nu tends to negative infinity.");
    case cppunivariate::CmbStatus::Unidentified:
      Rcpp::stop("With size = 0 both parameters are unidentified.");
    case cppunivariate::CmbStatus::InvalidInput:
      Rcpp::stop("CMB requires counts between zero and size, positive reltol and positive iterlim.");
    default: Rcpp::stop("CMB MLE failed to converge.");
  }
  Rcpp::CharacterVector fixed=Rcpp::CharacterVector::create("size");
  if (prob.isNotNull()) fixed.push_back("prob");
  if (nu.isNotNull() || trials==1) fixed.push_back("nu");
  if (trials==1 && nu.isNull() && res.prob>0 && res.prob<1) Rcpp::warning("nu is not identified with size = 1; returning nu = 1.");
  return Rcpp::List::create(
    Rcpp::_["estimates"]=Rcpp::NumericVector::create(Rcpp::_["size"]=trials,Rcpp::_["prob"]=res.prob,Rcpp::_["nu"]=res.nu),
    Rcpp::_["logLik"]=res.logLik,Rcpp::_["iterations"]=res.iterations,Rcpp::_["fixed"]=fixed);
}
