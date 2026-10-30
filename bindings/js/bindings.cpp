#include <emscripten/bind.h>
#include <emscripten/val.h>
#include <string>
#include <vector>
#include <cmath>
#include "cppunivariate/distributions/rng_std.hpp"
#include "cppunivariate/distributions/exp/mle.hpp"
#include "cppunivariate/distributions/exp/dpqr.hpp"
#include "cppunivariate/distributions/gamma/mle.hpp"
#include "cppunivariate/distributions/gamma/dpqr.hpp"
#include "cppunivariate/distributions/gb2/mle.hpp"
#include "cppunivariate/distributions/gb2/dpqr.hpp"
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
double held(emscripten::val h,const char* name) {
  auto v=h[name]; return v.isUndefined() || v.isNull() ? NAN : v.as<double>();
}
std::string held_error(emscripten::val h,const char* name,bool gb) {
  auto v=h[name]; if(v.isUndefined() || v.isNull()) return "";
  const double z=v.as<double>(); if(z>0 && std::isfinite(z)) return "";
  return gb ? "`"+std::string(name)+"` must be a positive finite number." : "Held `"+std::string(name)+"` must be positive and finite.";
}
double control(emscripten::val c,const char* name,double value) {
  return c[name].isUndefined() ? value : c[name].as<double>();
}
#ifndef OMIT_EXP
emscripten::val dexp(emscripten::val x,emscripten::val p,bool lower,bool log) {
  auto v=input(x); for(auto &z:v) z=cppunivariate::distributions::dexp(z,p[0].as<double>(),log); return output(v);
}
emscripten::val pexp(emscripten::val x,emscripten::val p,bool lower,bool log) {
  auto v=input(x); for(auto &z:v) z=cppunivariate::distributions::pexp(z,p[0].as<double>(),lower,log); return output(v);
}
emscripten::val qexp(emscripten::val x,emscripten::val p,bool lower,bool log) {
  auto v=input(x); for(auto &z:v) z=cppunivariate::distributions::qexp(z,p[0].as<double>(),lower,log); return output(v);
}
emscripten::val rexp(unsigned n,emscripten::val p,unsigned seed) {
  cppunivariate::distributions::StdRng<> rng(seed); std::vector<double> v(n);
  for(auto &z:v) z=cppunivariate::distributions::rexp(rng,p[0].as<double>()); return output(v);
}
emscripten::val fitexp(emscripten::val data,emscripten::val h,emscripten::val c) {
  auto x=input(data);
  for(double z:x) if(!std::isfinite(z) || z<0) return diagnostic("Data must be nonnegative and finite.");
  if(auto error=held_error(h,"rate",false); !error.empty()) return diagnostic(error);
  double rate=held(h,"rate");
  auto r=cppunivariate::exp_mle(cppunivariate::span{x.data(),x.size()},rate);
  if(!std::isfinite(r.rate)) return diagnostic(std::any_of(x.begin(),x.end(),[](double z){return z>0;}) ? "Exponential MLE rate exceeds double precision." : "The exponential maximum likelihood estimator does not exist when every observation is zero: the likelihood is unbounded as the rate grows.");
  if(!std::isfinite(r.logLik)) return diagnostic("Exponential likelihood exceeds double precision.");
  auto out=emscripten::val::object(), estimates=emscripten::val::object(), fixed=emscripten::val::array();
  estimates.set("rate",r.rate); if(!std::isnan(rate)) fixed.call<void>("push",std::string("rate"));
  out.set("estimates",estimates); out.set("fixed",fixed); out.set("logLik",r.logLik); out.set("iterations",0); out.set("status",std::string("Ok")); return out;
}
#endif
#ifndef OMIT_GAMMA
emscripten::val dgamma(emscripten::val x,emscripten::val p,bool lower,bool log) {
  auto v=input(x); for(auto &z:v) z=cppunivariate::distributions::dgamma(z,p[0].as<double>(),p[1].as<double>(),log); return output(v);
}
emscripten::val pgamma(emscripten::val x,emscripten::val p,bool lower,bool log) {
  auto v=input(x); for(auto &z:v) z=cppunivariate::distributions::pgamma(z,p[0].as<double>(),p[1].as<double>(),lower,log); return output(v);
}
emscripten::val qgamma(emscripten::val x,emscripten::val p,bool lower,bool log) {
  auto v=input(x); for(auto &z:v) z=cppunivariate::distributions::qgamma(z,p[0].as<double>(),p[1].as<double>(),lower,log); return output(v);
}
emscripten::val rgamma(unsigned n,emscripten::val p,unsigned seed) {
  cppunivariate::distributions::StdRng<> rng(seed); std::vector<double> v(n);
  for(auto &z:v) z=cppunivariate::distributions::rgamma(rng,p[0].as<double>(),p[1].as<double>()); return output(v);
}
emscripten::val fitgamma(emscripten::val data,emscripten::val h,emscripten::val c) {
  auto x=input(data);
  if(auto error=held_error(h,"shape",false); !error.empty()) return diagnostic(error);
  double shape=held(h,"shape");
  if(auto error=held_error(h,"scale",false); !error.empty()) return diagnostic(error);
  double scale=held(h,"scale");
  auto r=cppunivariate::gamma_mle(cppunivariate::span{x.data(),x.size()},control(c,"abstol",1.220703e-4),static_cast<int>(control(c,"iterlim",100)),shape,scale);
  if(r.status==cppunivariate::GammaStatus::IterationFailure) return diagnostic("Gamma delegate optimizer IterationFailure; increase iterlim.");
  if(r.status==cppunivariate::GammaStatus::Degenerate) return diagnostic("The gamma maximum likelihood estimator does not exist for constant data: the likelihood is unbounded as the shape grows.");
  if(!std::isfinite(r.shape)||!std::isfinite(r.scale)||!std::isfinite(r.logLik)) return diagnostic("Gamma MLE failed");
  auto out=emscripten::val::object(), estimates=emscripten::val::object(), fixed=emscripten::val::array();
  estimates.set("shape",r.shape); if(!std::isnan(shape)) fixed.call<void>("push",std::string("shape"));
  estimates.set("scale",r.scale); if(!std::isnan(scale)) fixed.call<void>("push",std::string("scale"));
  out.set("estimates",estimates); out.set("fixed",fixed); out.set("logLik",r.logLik); out.set("iterations",r.iterations); out.set("status",std::string("Ok")); return out;
}
#endif
#ifndef OMIT_GB2
emscripten::val dgb2(emscripten::val x,emscripten::val p,bool lower,bool log) {
  auto v=input(x); for(auto &z:v) z=cppunivariate::distributions::dgb2(z,p[0].as<double>(),p[1].as<double>(),p[2].as<double>(),p[3].as<double>(),log); return output(v);
}
emscripten::val pgb2(emscripten::val x,emscripten::val p,bool lower,bool log) {
  auto v=input(x); for(auto &z:v) z=cppunivariate::distributions::pgb2(z,p[0].as<double>(),p[1].as<double>(),p[2].as<double>(),p[3].as<double>(),lower,log); return output(v);
}
emscripten::val qgb2(emscripten::val x,emscripten::val p,bool lower,bool log) {
  auto v=input(x); for(auto &z:v) z=cppunivariate::distributions::qgb2(z,p[0].as<double>(),p[1].as<double>(),p[2].as<double>(),p[3].as<double>(),lower,log); return output(v);
}
emscripten::val rgb2(unsigned n,emscripten::val p,unsigned seed) {
  cppunivariate::distributions::StdRng<> rng(seed); std::vector<double> v(n);
  for(auto &z:v) z=cppunivariate::distributions::rgb2(rng,p[0].as<double>(),p[1].as<double>(),p[2].as<double>(),p[3].as<double>()); return output(v);
}
emscripten::val fitgb2(emscripten::val data,emscripten::val h,emscripten::val c) {
  auto x=input(data);
  if(auto error=held_error(h,"shape1",true); !error.empty()) return diagnostic(error);
  double shape1=held(h,"shape1");
  if(auto error=held_error(h,"shape2",true); !error.empty()) return diagnostic(error);
  double shape2=held(h,"shape2");
  if(auto error=held_error(h,"shape3",true); !error.empty()) return diagnostic(error);
  double shape3=held(h,"shape3");
  if(auto error=held_error(h,"scale",true); !error.empty()) return diagnostic(error);
  double scale=held(h,"scale");
  auto r=cppunivariate::gb2_mle(cppunivariate::span{x.data(),x.size()},control(c,"gradtol",1e-7),static_cast<int>(control(c,"iterlim",300)),shape1,shape2,shape3,scale);
  switch(r.status) {
    case cppunivariate::Gb2Status::Ok: break;
    case cppunivariate::Gb2Status::TrGammaBoundary: return diagnostic("GB2 selects a limiting model. Use `mltrgamma`.");
    case cppunivariate::Gb2Status::InvTrGammaBoundary: return diagnostic("GB2 selects a limiting model. Use `mlinvtrgamma`.");
    case cppunivariate::Gb2Status::LognormalBoundary: return diagnostic("GB2 selects a limiting model. Use `mllnorm`.");
    case cppunivariate::Gb2Status::ParetoBoundary: return diagnostic("GB2 selects a limiting model. Use `mlpareto`.");
    case cppunivariate::Gb2Status::PowerBoundary: return diagnostic("GB2 selects a limiting model. Use `mlpower`.");
    case cppunivariate::Gb2Status::DoubleParetoBoundary: return diagnostic("GB2 selects the double Pareto limit; this package has no double Pareto fitter.");
    case cppunivariate::Gb2Status::InvalidData: return diagnostic("GB2 requires positive finite nonconstant observations; constant data have an unbounded free likelihood.");
    case cppunivariate::Gb2Status::InvalidFixed: return diagnostic("GB2 fixed parameters must be positive and finite.");
    case cppunivariate::Gb2Status::ScaleOutOfRange: return diagnostic("GB2 scale is outside the representable range.");
    default: return diagnostic("GB2 search unresolved: no verified interior maximum; increase iterlim or inspect the limiting models.");
  }
  auto out=emscripten::val::object(), estimates=emscripten::val::object(), fixed=emscripten::val::array();
  estimates.set("shape1",r.shape1); if(!std::isnan(shape1)) fixed.call<void>("push",std::string("shape1"));
  estimates.set("shape2",r.shape2); if(!std::isnan(shape2)) fixed.call<void>("push",std::string("shape2"));
  estimates.set("shape3",r.shape3); if(!std::isnan(shape3)) fixed.call<void>("push",std::string("shape3"));
  estimates.set("scale",r.scale); if(!std::isnan(scale)) fixed.call<void>("push",std::string("scale"));
  out.set("estimates",estimates); out.set("fixed",fixed); out.set("logLik",r.logLik); out.set("iterations",r.iterations); out.set("status",std::string("Ok")); return out;
}
#endif
}
EMSCRIPTEN_BINDINGS(univariateML) {
#ifndef OMIT_EXP
  emscripten::function("dexp",&dexp);
  emscripten::function("pexp",&pexp);
  emscripten::function("qexp",&qexp);
  emscripten::function("rexp",&rexp);
  emscripten::function("fitexp",&fitexp);
#endif
#ifndef OMIT_GAMMA
  emscripten::function("dgamma",&dgamma);
  emscripten::function("pgamma",&pgamma);
  emscripten::function("qgamma",&qgamma);
  emscripten::function("rgamma",&rgamma);
  emscripten::function("fitgamma",&fitgamma);
#endif
#ifndef OMIT_GB2
  emscripten::function("dgb2",&dgb2);
  emscripten::function("pgb2",&pgb2);
  emscripten::function("qgb2",&qgb2);
  emscripten::function("rgb2",&rgb2);
  emscripten::function("fitgb2",&fitgb2);
#endif
}
