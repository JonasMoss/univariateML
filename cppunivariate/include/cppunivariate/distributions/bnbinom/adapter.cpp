#include "as_span.h"
#include "cppunivariate/distributions/bnbinom/mle.hpp"
#include <Rcpp.h>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlbnbinom_(const Rcpp::IntegerVector &x,
    Rcpp::Nullable<double> size=R_NilValue,
    Rcpp::Nullable<double> alpha=R_NilValue,
    Rcpp::Nullable<double> beta=R_NilValue,
    double gradtol=1e-8, int iterlim=600) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for (const auto &parameter : {size,alpha,beta}) {
    if (parameter.isNotNull()) {
      const double value=Rcpp::as<double>(parameter);
      if (!(value>0) || !std::isfinite(value))
        Rcpp::stop("Fixed parameters must be positive finite numbers.");
    }
  }
  const auto fit=cppunivariate::bnbinom_mle(dist_impl::as_ispan(x),gradtol,iterlim,
    size.isNotNull()?Rcpp::as<double>(size):nan,
    alpha.isNotNull()?Rcpp::as<double>(alpha):nan,
    beta.isNotNull()?Rcpp::as<double>(beta):nan);
  switch (fit.status) {
    case cppunivariate::BnbinomStatus::Ok: break;
    case cppunivariate::BnbinomStatus::NegativeBinomialLimit:
      Rcpp::stop("The negative binomial face wins (including likelihood ties). Use `mlnbinom`; hold its size at the supplied `size` or `beta`, if either was held.");
    case cppunivariate::BnbinomStatus::PoissonLimit:
      Rcpp::stop("The Poisson face wins (including likelihood ties). Use `mlpois` instead.");
    case cppunivariate::BnbinomStatus::PointMass:
      Rcpp::stop("The maximum likelihood estimator does not exist: all observations are zero (point mass). Use `mlpois` instead.");
    case cppunivariate::BnbinomStatus::InvalidInput:
      Rcpp::stop("Beta negative binomial fitting requires nonempty nonnegative counts, positive finite fixed parameters and gradtol, and positive iterlim.");
    default: Rcpp::stop("Beta negative binomial search failed: unresolved stationary point or search range; try increasing iterlim.");
  }
  Rcpp::CharacterVector fixed;
  if (size.isNotNull()) fixed.push_back("size");
  if (alpha.isNotNull()) fixed.push_back("alpha");
  if (beta.isNotNull()) fixed.push_back("beta");
  return Rcpp::List::create(
    Rcpp::_["estimates"]=Rcpp::NumericVector::create(Rcpp::_["size"]=fit.size,
      Rcpp::_["alpha"]=fit.alpha,Rcpp::_["beta"]=fit.beta),
    Rcpp::_["logLik"]=fit.logLik,Rcpp::_["iterations"]=fit.iterations,
    Rcpp::_["fixed"]=fixed);
}
