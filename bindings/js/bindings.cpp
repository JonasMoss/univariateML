#include <emscripten/bind.h>
#include <emscripten/val.h>
#include <string>
#include <vector>
#include <cmath>
#include "cppunivariate/distributions/rng_std.hpp"
#include "common/fit.hpp"
#include "common/modifiers.hpp"
#include "cppunivariate/core/inference_families.hpp"
#include "generated/fit_includes.hpp"
namespace {
std::vector<double> input(emscripten::val x) {
  return emscripten::convertJSArrayToNumberVector<double>(x);
}
emscripten::val output(const std::vector<double>& x) {
  auto a=emscripten::val::global("Float64Array").new_(x.size());
  for(std::size_t i=0;i<x.size();++i) a.set(i,x[i]);
  return a;
}
emscripten::val diagnostic(const std::string& message) {
  auto out=emscripten::val::object(); out.set("error",message); return out;
}


}
#include "generated/dpqr.hpp"
emscripten::val fit_output(const umvl_binding::FitResult& result) {
  if(!result.error.empty()) {
    auto out=diagnostic(result.error), warnings=emscripten::val::array();
    for(const auto& message:result.warnings) warnings.call<void>("push",message);
    out.set("warnings",warnings);
    if(!result.error_kind.empty()) {
      out.set("attainable_tol",result.attainable_tol);out.set("kind",result.error_kind);out.set("model",result.model);
      auto estimates=emscripten::val::object(),candidates=emscripten::val::array();
      for(const auto& item:result.limit_estimates)estimates.set(item.first,item.second);
      for(const auto& candidate:result.candidates) {
        auto row=emscripten::val::object();row.set("model",candidate.model);
        for(const auto& item:candidate.estimates)row.set(item.first,item.second);
        row.set("loglik_lower",candidate.loglik_lower);row.set("loglik_upper",candidate.loglik_upper);
        candidates.call<void>("push",row);
      }
      out.set("estimates",estimates);out.set("candidates",candidates);
      out.set("segment_loglik_upper",result.segment_loglik_upper);
      out.set("limit_loglik",result.limit_loglik);out.set("loglik_lower",result.loglik_lower);out.set("loglik_upper",result.loglik_upper);
    }
    for(const auto& item:result.extra) out.set(item.first,item.second);
    return out;
  }
  auto out=emscripten::val::object(), estimates=emscripten::val::object(), fixed=emscripten::val::array();
  for(const auto& item:result.estimates) estimates.set(item.first,item.second);
  for(const auto& name:result.fixed) fixed.call<void>("push",name);
  auto warnings=emscripten::val::array(), restricted=emscripten::val::object();
  for(const auto& message:result.warnings) warnings.call<void>("push",message);
  for(const auto& item:result.restricted) restricted.set(item.first,item.second);
  for(const auto& item:result.extra) out.set(item.first,item.second);
  for(const auto& item:result.extra_vectors) out.set(item.first,output(item.second));
  out.set("warnings",warnings); out.set("restricted",restricted);
  out.set("estimates",estimates); out.set("fixed",fixed); out.set("logLik",result.logLik);
  out.set("iterations",result.iterations); out.set("status",std::string("Ok")); return out;
}
umvl_binding::Held held_arg(emscripten::val h,const char* name) {
  auto v=h[name]; return v.isUndefined() || v.isNull() ? umvl_binding::Held{} : umvl_binding::Held{true,v.as<double>()};
}
double control(emscripten::val c,const char* name,double value) {
  return c[name].isUndefined() ? value : c[name].as<double>();
}
#include "generated/fits.hpp"
emscripten::val vcov_output(emscripten::val data, const std::string& family,
                             emscripten::val coefficients, unsigned mask, bool sandwich) {
  auto x=input(data), p=input(coefficients);
  if(p.size()>cppunivariate::inference_max_parameters) return diagnostic("invalid inference coefficient dimensions");
  std::array<double,cppunivariate::inference_max_parameters> values{};
  for(std::size_t i=0;i<p.size();++i) values[i]=p[i];
  const auto result=cppunivariate::family_inference(cppunivariate::span{x.data(),x.size()},family,values,mask);
  const auto covariance=cppunivariate::inference_covariance(result,sandwich ?
      cppunivariate::CovarianceKind::Sandwich : cppunivariate::CovarianceKind::Observed);
  auto out=emscripten::val::object(), matrix=emscripten::val::array();
  for(std::size_t i=0;i<covariance.dimension;++i) {
    std::vector<double> row(covariance.dimension);
    for(std::size_t j=0;j<row.size();++j) row[j]=covariance.matrix[i*row.size()+j];
    matrix.call<void>("push",output(row));
  }
  out.set("matrix",matrix); out.set("condition",covariance.condition);
  out.set("status",static_cast<int>(covariance.status)); out.set("validity",result.validity_conditions);
  return out;
}
emscripten::val fit_modifier_output(const std::string& family,emscripten::val lower,emscripten::val upper,
    emscripten::val h,const std::string& zero,bool window,double a,double b,bool censored,double tol,int maxit) {
  auto lo=input(lower),hi=input(upper);
  std::vector<std::pair<std::string,umvl_binding::Held>> holds;
  auto names=emscripten::val::global("Object").call<emscripten::val>("keys",h);
  for(unsigned i=0;i<names["length"].as<unsigned>();++i) {
    const auto name=names[i].as<std::string>();holds.push_back({name,held_arg(h,name.c_str())});
  }
  if(censored)return fit_output(umvl_binding::fit_censored(family,{{lo.data(),lo.size()},{hi.data(),hi.size()}},holds,tol,maxit));
  return fit_output(umvl_binding::fit_modifier(family,{lo.data(),lo.size()},holds,zero,window,a,b,tol,maxit));
}
emscripten::val zeta_window_output(const std::string& kind,emscripten::val values,int N,double s,bool lower,bool log) {
  auto x=input(values);
  for(auto& v:x)v=cppunivariate::zeta_window_eval(kind[0],v,N,s,lower,log);
  return output(x);
}
EMSCRIPTEN_BINDINGS(univariateML) {
  emscripten::function("zetaWindow", &zeta_window_output);
  emscripten::function("vcov", &vcov_output);
  emscripten::function("fitModifier", &fit_modifier_output);
#include "generated/exports.hpp"
#include "generated/fit_exports.hpp"
}
