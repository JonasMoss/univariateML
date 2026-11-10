// Conversion only; shared fit headers must precede Rcpp.h.
#pragma once
#include <Rcpp.h>
#include <algorithm>
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
  if (!fit.error.empty()) {
    if(fit.error_kind.empty())Rcpp::stop(fit.error);
    Rcpp::List candidates;
    std::vector<std::string> columns;
    for(const auto& candidate:fit.candidates)for(const auto& item:candidate.estimates)
      if(std::find(columns.begin(),columns.end(),item.first)==columns.end())columns.push_back(item.first);
    const int n=fit.candidates.size();
    Rcpp::CharacterVector models(n);Rcpp::NumericVector lower(n),upper(n);
    for(int i=0;i<n;++i){models[i]=fit.candidates[i].model;lower[i]=fit.candidates[i].loglik_lower;upper[i]=fit.candidates[i].loglik_upper;}
    candidates["model"]=models;
    for(const auto& name:columns) {
      Rcpp::NumericVector values(n,NA_REAL);
      for(int i=0;i<n;++i)for(const auto& item:fit.candidates[i].estimates)if(item.first==name)values[i]=item.second;
      candidates[name]=values;
    }
    candidates["loglik_lower"]=lower;candidates["loglik_upper"]=upper;
    candidates.attr("class")="data.frame";
    candidates.attr("row.names")=Rcpp::IntegerVector::create(NA_INTEGER,-n);
    Rcpp::NumericVector estimates;
    for(const auto& item:fit.limit_estimates)estimates.push_back(item.second,item.first);
    auto condition=Rcpp::List::create(Rcpp::_["message"]=fit.error,Rcpp::_["call"]=R_NilValue,
        Rcpp::_["kind"]=fit.error_kind,Rcpp::_["candidates"]=candidates,
        Rcpp::_["model"]=fit.model,Rcpp::_["estimates"]=estimates,
        Rcpp::_["limit_loglik"]=fit.limit_loglik,Rcpp::_["loglik_lower"]=fit.loglik_lower,
        Rcpp::_["loglik_upper"]=fit.loglik_upper,Rcpp::_["segment_loglik_upper"]=fit.segment_loglik_upper);
    condition.attr("class")=Rcpp::CharacterVector::create("univariateML_"+fit.error_kind,"univariateML_stop","error","condition");
    return condition;
  }
  Rcpp::NumericVector estimates(fit.estimates.size());
  Rcpp::CharacterVector names(fit.estimates.size());
  for (std::size_t i=0; i<fit.estimates.size(); ++i) {
    names[i]=fit.estimates[i].first; estimates[i]=fit.estimates[i].second;
  }
  estimates.attr("names")=names;
  // Keep per-fit certification through the generated wrapper constructor.
  for(const auto& item:fit.extra)if(item.first=="certified")
    estimates.attr("certified")=item.second!=0;
  auto out=Rcpp::List::create(Rcpp::_["estimates"]=estimates,Rcpp::_["logLik"]=fit.logLik);
  if (fit.has_restricted) {
    Rcpp::CharacterVector values;
    for (const auto& item : fit.restricted) values.push_back(item.second, item.first);
    out["restricted"] = values;
  }
  for (const auto& item : fit.extra) {
    if(item.first=="certified")out[item.first]=item.second!=0;
    else out[item.first]=item.second;
  }
  for (const auto& item : fit.extra_vectors) out[item.first] = Rcpp::wrap(item.second);
  if(iterations) out["iterations"]=fit.iterations;
  out["fixed"]=Rcpp::wrap(fit.fixed);
  return out;
}

inline Rcpp::List common_result(const umvl_binding::FitResult& fit) {
  auto out = common_result(fit, fit.has_iterations);
  if (!fit.error.empty()) return out;
  if (!fit.has_fixed) out.erase(out.findName("fixed"));
  return out;
}
