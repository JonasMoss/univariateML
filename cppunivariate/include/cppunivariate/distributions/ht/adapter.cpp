#include "as_span.h"
#include "cppunivariate/distributions/ht/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <limits>
#include <sstream>
#include <string>

namespace {
// A held mu prints as a whole number when it is one (the common case,
// e.g. `mu = 0`), matching the hint users would type back in.
std::string whole_mu(double v) {
  std::ostringstream out;
  if (v == std::floor(v) && std::abs(v) < 1e15) out << static_cast<long long>(v);
  else out << v;
  return out.str();
}
}  // namespace

// [[Rcpp::export]]
Rcpp::List mlht_(const Rcpp::NumericVector &x, Rcpp::Nullable<double> mu,
                 Rcpp::Nullable<double> nu = R_NilValue,
                 Rcpp::Nullable<double> sigma = R_NilValue) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const bool mu_held = mu.isNotNull();
  const double held_mu=mu_held?Rcpp::as<double>(mu):nan;
  if (mu_held && !std::isfinite(held_mu)) Rcpp::stop("`mu` must be finite.");
  const double df=nu.isNotNull()?Rcpp::as<double>(nu):nan;
  const double sc=sigma.isNotNull()?Rcpp::as<double>(sigma):nan;
  if ((nu.isNotNull() && (!(df>0)||!std::isfinite(df))) ||
      (sigma.isNotNull() && (!(sc>0)||!std::isfinite(sc))))
    Rcpp::stop("Held `nu` and `sigma` must be positive finite numbers.");
  const auto r=cppunivariate::ht_mle(dist_impl::as_span(x),held_mu,df,sc);
  if(mu_held && r.status==cppunivariate::HtStatus::InvalidData)
    Rcpp::stop("Half-t observations must be finite and at least the held mu.");
  if(r.status==cppunivariate::HtStatus::Unbounded)
    Rcpp::stop("Half-t likelihood is unbounded along sigma -> 0 with nu*(n-k) < k (k ties at the free or held endpoint); hold sigma, hold nu with nu*(n-k) > k, or (with a free endpoint) hold mu.");
  if(r.status==cppunivariate::HtStatus::ScaleBoundary)
    Rcpp::stop("Half-t likelihood has a finite unattained supremum as sigma -> 0: nu*(n-k) = k.");
  const std::string mu_hint = mu_held ? whole_mu(held_mu) : std::string("NULL");
  if(r.status==cppunivariate::HtStatus::NormalLimit && sigma.isNotNull())
    Rcpp::stop("Half-t MLE escapes as nu -> infinity to the half-normal with sigma held; "
               "use `mlhnorm(x, mu = " + mu_hint + ", sigma = " + whole_mu(sc) + ")`.");
  if(r.status==cppunivariate::HtStatus::NormalLimit)
    Rcpp::stop("Half-t MLE escapes as nu -> infinity; use `mlhnorm(x, mu = " + mu_hint + ")`.");
  if(r.status!=cppunivariate::HtStatus::Ok)
    Rcpp::stop("Half-t MLE failed: check finite data at least the endpoint and numerical search range.");
  Rcpp::CharacterVector fixed;
  if(mu_held) fixed.push_back("mu");
  if(nu.isNotNull()) fixed.push_back("nu");
  if(sigma.isNotNull()) fixed.push_back("sigma");
  Rcpp::CharacterVector restricted;
  if(r.restricted & 4u) restricted.push_back("lower","nu");
  if(r.restricted & 8u) restricted.push_back("upper","nu");
  return Rcpp::List::create(Rcpp::_["estimates"]=Rcpp::NumericVector::create(
    Rcpp::_["mu"]=r.mu,Rcpp::_["nu"]=r.nu,Rcpp::_["sigma"]=r.sigma),Rcpp::_["logLik"]=r.logLik,
    Rcpp::_["restricted"]=restricted, Rcpp::_["fixed"]=fixed,Rcpp::_["iterations"]=r.iterations);
}
