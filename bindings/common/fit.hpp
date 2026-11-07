#pragma once
#include <string>
#include <vector>
#include <utility>
#include <cmath>
#include <sstream>
#include <iomanip>
#include "cppunivariate/core/span.hpp"
#include "cppunivariate/core/stop_detail.hpp"
namespace umvl_binding {
struct NamedCandidate {
  std::vector<std::pair<std::string,double>> estimates;
  double loglik_lower=NAN,loglik_upper=NAN;
  std::string model;
};
struct FitResult {
  std::vector<std::pair<std::string, double>> estimates;
  double logLik = NAN;
  int iterations = 0;
  std::vector<std::string> fixed;
  std::string error;
  std::vector<std::string> warnings;
  std::vector<std::pair<std::string, std::string>> restricted;
  std::vector<std::pair<std::string, double>> extra;
  std::vector<std::pair<std::string, std::vector<double>>> extra_vectors;
  bool has_iterations = false;
  bool has_fixed = false;
  bool has_restricted = false;
  std::string error_kind, model;
  std::vector<NamedCandidate> candidates;
  std::vector<std::pair<std::string,double>> limit_estimates;
  double limit_loglik=NAN,loglik_lower=NAN,loglik_upper=NAN,segment_loglik_upper=NAN;
};
// The adapters use only %s, %g and fixed decimal %f diagnostics. Construct
// strings without host I/O, exceptions, varargs or an R formatting dependency.
template <typename T>
inline void message_argument(std::string& pattern, const T& value) {
  const std::size_t first = pattern.find('%');
  if (first == std::string::npos) return;
  std::size_t last = first + 1;
  int precision = 6;
  if (last < pattern.size() && pattern[last] == '.') {
    precision = 0;
    ++last;
    while (last < pattern.size() && pattern[last] >= '0' && pattern[last] <= '9') {
      precision = precision * 10 + pattern[last] - '0';
      ++last;
    }
  }
  if (last >= pattern.size()) return;
  std::ostringstream stream;
  stream << std::setprecision(precision);
  if (pattern[last] == 'f') stream << std::fixed;
  stream << value;
  pattern.replace(first, last - first + 1, stream.str());
}
template <typename... T>
inline std::string binding_message(std::string pattern, const T&... values) {
  (message_argument(pattern, values), ...);
  return pattern;
}
inline FitResult failure(const std::string& message, const std::vector<std::string>& warnings = {}) {
  FitResult result; result.error = message; result.warnings = warnings; return result;
}
inline std::vector<std::pair<std::string,double>> named_coefficients(
    const std::vector<double>& values,const std::vector<std::string>& names) {
  std::vector<std::pair<std::string,double>> out;
  for(std::size_t i=0;i<values.size()&&i<names.size();++i)out.push_back({names[i],values[i]});
  return out;
}
inline std::string stop_model(cppunivariate::StopModel model) {
  switch(model) {
    case cppunivariate::StopModel::Weibull:return "weibull";
    case cppunivariate::StopModel::Pareto:return "pareto";
    case cppunivariate::StopModel::InvWeibull:return "invweibull";
    case cppunivariate::StopModel::Power:return "power";
    case cppunivariate::StopModel::Exp:return "exp";
    case cppunivariate::StopModel::InvExp:return "invexp";
    case cppunivariate::StopModel::Pois:return "pois";
    case cppunivariate::StopModel::Binom:return "binom";
    case cppunivariate::StopModel::Nbinom:return "nbinom";
    case cppunivariate::StopModel::Geom:return "geom";
    case cppunivariate::StopModel::Tpois:return "tpois";
    case cppunivariate::StopModel::Ztpois:return "ztpois";
    case cppunivariate::StopModel::Zmpois:return "zmpois";
    case cppunivariate::StopModel::Zip:return "zip";
    case cppunivariate::StopModel::Cmp:return "cmp";
    case cppunivariate::StopModel::PointMass:return "unavailable:point-mass";
    case cppunivariate::StopModel::TwoPoint:return "unavailable:two-point";
    case cppunivariate::StopModel::ShiftedNbinom:return "unavailable:translated-negative-binomial";
    case cppunivariate::StopModel::Norm:return "norm";
    case cppunivariate::StopModel::Lst:return "lst";
    case cppunivariate::StopModel::TCollapsePath:return "unavailable:t-scale-collapse";
    case cppunivariate::StopModel::Unif:return "unif";
    case cppunivariate::StopModel::Hnorm:return "hnorm";
    case cppunivariate::StopModel::Laplace:return "laplace";
    case cppunivariate::StopModel::Lnorm:return "lnorm";
    case cppunivariate::StopModel::Sn:return "sn";
    case cppunivariate::StopModel::Ht:return "ht";
    case cppunivariate::StopModel::TranslatedExp:return "unavailable:translated-exponential";
    case cppunivariate::StopModel::ReflectedExp:return "unavailable:reflected-exponential";
    case cppunivariate::StopModel::TruncatedExp:return "unavailable:truncated-exponential";
    case cppunivariate::StopModel::ReflectedHnorm:return "unavailable:reflected-half-normal";
    case cppunivariate::StopModel::ReflectedHt:return "unavailable:reflected-half-t";
    case cppunivariate::StopModel::Trgamma:return "trgamma";
    case cppunivariate::StopModel::InvTrgamma:return "invtrgamma";
    case cppunivariate::StopModel::ShiftedTrgamma:return "unavailable:shifted-trgamma";
    case cppunivariate::StopModel::ShiftedInvTrgamma:return "unavailable:shifted-invtrgamma";
    case cppunivariate::StopModel::UnitGamma:return "unavailable:unit-gamma";
    case cppunivariate::StopModel::HalfGed:return "unavailable:half-ged";
    case cppunivariate::StopModel::ReflectedHalfGed:return "unavailable:reflected-half-ged";
    default:return "";
  }
}
inline std::vector<std::string> stop_names(const std::string& model,const std::vector<std::string>& names) {
  if(model=="weibull"||model=="invweibull")return {"shape","scale"};
  if(model=="pareto")return {"a","b"};
  if(model=="power")return {"alpha","beta"};
  if(model=="exp")return {"rate"};
  if(model=="invexp")return {"scale"};
  if(model=="pois")return {"lambda"};
  if(model=="binom")return {"size","prob"};
  if(model=="nbinom")return {"size","prob"};
  if(model=="geom")return {"prob"};
  if(model=="tpois")return {"lambda","a","b"};
  if(model=="ztpois")return {"lambda"};
  if(model=="zmpois")return {"lambda","p0"};
  if(model=="zip")return {"lambda","pi"};
  if(model=="trgamma"||model=="invtrgamma")return {"shape1","shape2","scale"};
  if(model=="unavailable:shifted-trgamma"||model=="unavailable:shifted-invtrgamma")return {"shape1","shape2","scale","min"};
  if(model=="cmp")return {"lambda","nu"};
  if(model=="unavailable:point-mass")return {"point"};
  if(model=="unavailable:two-point")return {"lower","upper","prob"};
  if(model=="unavailable:translated-negative-binomial")return {"size","prob","shift"};
  if(model=="norm")return {"mean","sd"};
  if(model=="lst")return {"mu","sigma","df"};
  if(model=="unavailable:t-scale-collapse")return {"mu","df"};
  if(model=="unif")return {"min","max"};
  if(model=="hnorm")return {"mu","sigma"};
  if(model=="laplace")return {"mu","sigma"};
  if(model=="lnorm")return {"meanlog","sdlog"};
  if(model=="sn")return {"xi","omega","alpha"};
  if(model=="ht")return {"mu","nu","sigma"};
  if(model=="unavailable:translated-exponential")return {"mu","rate"};
  if(model=="unavailable:reflected-exponential")return {"mu","rate"};
  if(model=="unavailable:truncated-exponential")return {"a","b","rate"};
  if(model=="unavailable:reflected-half-normal")return {"mu","sigma"};
  if(model=="unavailable:reflected-half-t")return {"mu","nu","sigma"};
  if(model=="unavailable:unit-gamma")return {"shape","scale","upper"};
  if(model=="unavailable:half-ged")return {"mu","scale","nu"};
  if(model=="unavailable:reflected-half-ged")return {"mu","scale","nu"};
  return names;
}
// Candidate bounds describe evaluated finite witnesses. An unrepresentable
// face remains an ordinary numerical error; global upper bounds may be infinite.
inline bool finite_stop_payload(const cppunivariate::StopDetail& detail,const std::string& kind) {
  if(kind=="limit") {
    if(detail.limit_coef.empty() || !std::isfinite(detail.limit_loglik))return false;
    for(double v:detail.limit_coef)if(!std::isfinite(v))return false;
  }
  if((kind=="tie" || kind=="budget") && detail.candidates.empty())return false;
  for(const auto& candidate:detail.candidates) {
    if(!cppunivariate::valid_stop_coefficients(candidate.model,candidate.coef) ||
       !std::isfinite(candidate.loglik_lower) || !std::isfinite(candidate.loglik_upper) ||
       candidate.loglik_lower>candidate.loglik_upper)return false;
    for(double v:candidate.coef)if(!std::isfinite(v))return false;
  }
  return true;
}
inline FitResult stop_failure(FitResult out,const cppunivariate::StopDetail& detail,
    const std::string& kind,const std::vector<std::string>& names,const std::string& model="") {
  out.error_kind=kind;out.model=model.empty()?stop_model(detail.limit_model):model;
  out.limit_estimates=named_coefficients(detail.limit_coef,stop_names(out.model,names));
  out.segment_loglik_upper=detail.segment_loglik_upper;
  out.limit_loglik=detail.limit_loglik;out.loglik_lower=detail.loglik_lower;out.loglik_upper=detail.loglik_upper;
  for(const auto& candidate:detail.candidates) {
    const std::string id=stop_model(candidate.model);
    out.candidates.push_back({named_coefficients(candidate.coef,stop_names(id,names)),candidate.loglik_lower,candidate.loglik_upper,id});
  }
  return out;
}
struct Held {
  bool supplied = false;
  double value = NAN;
  std::size_t length = 1;
};
}
