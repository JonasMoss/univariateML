// Conversion only; shared fit headers must precede Rcpp.h.
#pragma once
#include <Rcpp.h>
inline umvl_binding::Held common_held(const Rcpp::Nullable<double>& value) {
  return value.isNull() ? umvl_binding::Held{} : umvl_binding::Held{true, Rcpp::as<double>(value)};
}
inline umvl_binding::Held common_held(const Rcpp::Nullable<Rcpp::NumericVector>& value) {
  if (value.isNull()) return {};
  const auto vector = Rcpp::as<Rcpp::NumericVector>(value);
  return {true, vector.size() == 1 ? vector[0] : NAN, static_cast<std::size_t>(vector.size())};
}
inline umvl_binding::Held common_held(double value) {
  return {true, value};
}
inline Rcpp::List common_result(const umvl_binding::FitResult& fit, bool iterations) {
  for (const auto& message : fit.warnings) Rcpp::warning("%s", message);
  if (!fit.error.empty()) Rcpp::stop(fit.error);
  Rcpp::NumericVector estimates(fit.estimates.size());
  Rcpp::CharacterVector names(fit.estimates.size());
  for (std::size_t i=0; i<fit.estimates.size(); ++i) {
    names[i]=fit.estimates[i].first; estimates[i]=fit.estimates[i].second;
  }
  estimates.attr("names")=names;
  auto out=Rcpp::List::create(Rcpp::_["estimates"]=estimates,Rcpp::_["logLik"]=fit.logLik);
  if (fit.has_restricted) {
    Rcpp::CharacterVector values;
    for (const auto& item : fit.restricted) values.push_back(item.second, item.first);
    out["restricted"] = values;
  }
  for (const auto& item : fit.extra) out[item.first] = item.second;
  for (const auto& item : fit.extra_vectors) out[item.first] = Rcpp::wrap(item.second);
  if(iterations) out["iterations"]=fit.iterations;
  out["fixed"]=Rcpp::wrap(fit.fixed);
  return out;
}

inline Rcpp::List common_result(const umvl_binding::FitResult& fit) {
  auto out = common_result(fit, fit.has_iterations);
  if (!fit.has_fixed) out.erase(out.findName("fixed"));
  return out;
}
