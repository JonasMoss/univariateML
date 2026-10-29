#include "as_span.h"
#include "cppunivariate/distributions/huber/mle.hpp"
#include <Rcpp.h>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlhuber_(const Rcpp::NumericVector &x,
    Rcpp::Nullable<Rcpp::NumericVector> epsilon,
    Rcpp::Nullable<double> mu = R_NilValue,
    Rcpp::Nullable<double> sigma = R_NilValue,
    double gradtol = 1e-10, int iterlim = 200) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double m=mu.isNotNull() ? Rcpp::as<double>(mu) : nan;
  const double s=sigma.isNotNull() ? Rcpp::as<double>(sigma) : nan;
  if (mu.isNotNull() && !std::isfinite(m)) Rcpp::stop("`mu` must be finite.");
  if (sigma.isNotNull() && (!(s>0) || !std::isfinite(s)))
    Rcpp::stop("`sigma` must be positive and finite.");
  if (epsilon.isNotNull() && Rcpp::as<Rcpp::NumericVector>(epsilon).size()!=1) Rcpp::stop("`epsilon` must be a scalar or NULL.");
  const double e=epsilon.isNotNull() ? Rcpp::as<double>(epsilon) : nan;
  if (epsilon.isNotNull() && (!(e>0) || !std::isfinite(e)))
    Rcpp::stop("`epsilon` must be positive and finite.");
  auto r=cppunivariate::huber_mle(dist_impl::as_span(x),gradtol,iterlim,e,m,s);
  if (r.status==cppunivariate::HuberStatus::InvalidData)
    Rcpp::stop("Huber requires finite data, positive finite `gradtol` and positive `iterlim`.");
  if (r.status==cppunivariate::HuberStatus::UnboundedLikelihood)
    Rcpp::stop("Huber likelihood is unbounded for constant data at the fitted or held location.");
  if (r.status==cppunivariate::HuberStatus::NormalLimit)
    Rcpp::stop("Huber shape reaches the normal face; use `mlnorm`.");
  if (r.status==cppunivariate::HuberStatus::LaplaceLimit)
    Rcpp::stop("Huber shape reaches the Laplace face; use `mllaplace`.");
  if (r.status!=cppunivariate::HuberStatus::Ok || !std::isfinite(r.logLik))
    Rcpp::stop("Huber MLE failed to converge.");
  Rcpp::CharacterVector fixed;
  if (epsilon.isNotNull()) fixed.push_back("epsilon");
  if (mu.isNotNull()) fixed.push_back("mu");
  if (sigma.isNotNull()) fixed.push_back("sigma");
  return Rcpp::List::create(
    Rcpp::_["estimates"]=Rcpp::NumericVector::create(Rcpp::_["mu"]=r.mu,
      Rcpp::_["sigma"]=r.sigma,Rcpp::_["epsilon"]=r.epsilon),
    Rcpp::_["logLik"]=r.logLik,Rcpp::_["iterations"]=r.iterations,
    Rcpp::_["fixed"]=fixed);
}
