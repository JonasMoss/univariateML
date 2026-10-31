// Conversion only; shared fit headers must precede Rcpp.h.
#pragma once
#include <Rcpp.h>
inline umvl_binding::Held common_held(const Rcpp::Nullable<double>& value) {
  return value.isNull() ? umvl_binding::Held{} : umvl_binding::Held{true, Rcpp::as<double>(value)};
}
inline Rcpp::List common_result(const umvl_binding::FitResult& fit, bool iterations) {
  if (!fit.error.empty()) Rcpp::stop(fit.error);
  Rcpp::NumericVector estimates(fit.estimates.size());
  Rcpp::CharacterVector names(fit.estimates.size());
  for (std::size_t i=0; i<fit.estimates.size(); ++i) {
    names[i]=fit.estimates[i].first; estimates[i]=fit.estimates[i].second;
  }
  estimates.attr("names")=names;
  auto out=Rcpp::List::create(Rcpp::_["estimates"]=estimates,Rcpp::_["logLik"]=fit.logLik);
  if(iterations) out["iterations"]=fit.iterations;
  out["fixed"]=Rcpp::wrap(fit.fixed);
  return out;
}
