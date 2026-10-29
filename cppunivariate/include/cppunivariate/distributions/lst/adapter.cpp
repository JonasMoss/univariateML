#include "as_span.h"
#include "cppunivariate/distributions/lst/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <limits>
// [[Rcpp::export]]
Rcpp::List mllst_(const Rcpp::NumericVector &x, double gradtol = 1.220703e-4,
                  int iterlim = 200, Rcpp::Nullable<double> df = R_NilValue, Rcpp::Nullable<double> mu = R_NilValue, Rcpp::Nullable<double> sigma = R_NilValue) {
  if(df.isNotNull()&&!std::isfinite(Rcpp::as<double>(df)))Rcpp::stop("`df` must be finite and positive.");
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double held_mu=mu.isNotNull()?Rcpp::as<double>(mu):nan;
  if(mu.isNotNull() && (!std::isfinite(held_mu)))Rcpp::stop("Invalid held `mu`.");
  const double held_sigma=sigma.isNotNull()?Rcpp::as<double>(sigma):nan;
  if(sigma.isNotNull() && (!std::isfinite(held_sigma) || !(held_sigma>0)))Rcpp::stop("Invalid held `sigma`.");
  Rcpp::CharacterVector fixed;
  if(mu.isNotNull())fixed.push_back("mu");
  if(sigma.isNotNull())fixed.push_back("sigma");
  if(df.isNotNull())fixed.push_back("df");
  auto r=cppunivariate::lst_mle(dist_impl::as_span(x),gradtol,iterlim,held_mu,held_sigma,df.isNotNull()?Rcpp::as<double>(df):nan);
  if (r.status == cppunivariate::LstStatus::InvalidDf)
    Rcpp::stop("`df` must be finite and positive.");
  if (r.status == cppunivariate::LstStatus::Unbounded)
    Rcpp::stop("Fixed `df` likelihood is unbounded through scale collapse: excessive ties.");
  if (r.status == cppunivariate::LstStatus::ScaleBoundary)
    Rcpp::stop("Fixed `df` is at the tie equality scale-collapse boundary; a regular MLE is not certified.");
  if (r.status == cppunivariate::LstStatus::IterationFailure)
    Rcpp::stop("Fixed `df` EM did not converge to the score tolerance; increase iterlim.");
  if (r.status == cppunivariate::LstStatus::NoMLE)
    Rcpp::stop("Location-scale t MLE on df >= 1 requires at least 3 observations and no value repeated n/2 or more times; scale collapse otherwise prevents a regular MLE.");
  if (r.status == cppunivariate::LstStatus::NormalBoundary)
    Rcpp::stop("The location-scale t likelihood is maximised as df -> Inf. Use `mlnorm` to fit the normal distribution.");
  if(r.status==cppunivariate::LstStatus::BfgsFailure) Rcpp::stop("Location-scale t optimizer failed its score check; increase iterlim.");
  if (!std::isfinite(r.mu) || !std::isfinite(r.sigma) || !std::isfinite(r.df))
    Rcpp::stop("Location-scale t MLE requires finite data with positive spread.");
  Rcpp::CharacterVector restricted;
  if(r.restricted & 1u) restricted.push_back("lower","mu");
  if(r.restricted & 2u) restricted.push_back("upper","mu");
  if(r.restricted & 4u) restricted.push_back("lower","sigma");
  if(r.restricted & 8u) restricted.push_back("upper","sigma");
  if(r.restricted & 16u) restricted.push_back("lower","df");
  if(r.restricted & 32u) restricted.push_back("upper","df");
  return Rcpp::List::create(
    Rcpp::_["estimates"] = Rcpp::NumericVector::create(
      Rcpp::_["mu"] = r.mu, Rcpp::_["sigma"] = r.sigma, Rcpp::_["df"] = r.df),
    Rcpp::_["logLik"] = r.logLik, Rcpp::_["iterations"] = r.iterations,
      Rcpp::_["restricted"]=restricted, Rcpp::_["fixed"] = fixed);
}
