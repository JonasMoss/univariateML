#include <emscripten/bind.h>
#include <emscripten/val.h>
#include <string>
#include <vector>
#include <cmath>
#include "cppunivariate/distributions/rng_std.hpp"
#include "common/fit.hpp"
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
  if(!result.error.empty()) return diagnostic(result.error);
  auto out=emscripten::val::object(), estimates=emscripten::val::object(), fixed=emscripten::val::array();
  for(const auto& item:result.estimates) estimates.set(item.first,item.second);
  for(const auto& name:result.fixed) fixed.call<void>("push",name);
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
EMSCRIPTEN_BINDINGS(univariateML) {
#include "generated/exports.hpp"
#include "generated/fit_exports.hpp"
}
