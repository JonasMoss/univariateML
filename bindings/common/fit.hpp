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
    case cppunivariate::StopModel::PointMass:return "point mass (no package family)";
    case cppunivariate::StopModel::TwoPoint:return "two-point law (no package family)";
    case cppunivariate::StopModel::ShiftedNbinom:return "translated negative binomial (no package family)";
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
  if(model=="cmp")return {"lambda","nu"};
  if(model=="point mass (no package family)")return {"point"};
  if(model=="two-point law (no package family)")return {"lower","upper","prob"};
  if(model=="translated negative binomial (no package family)")return {"size","prob","shift"};
  return names;
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
