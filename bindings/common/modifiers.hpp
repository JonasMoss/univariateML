// Additive modifier seam shared by R and JS. Default fits never enter here.
#pragma once
#include "fit.hpp"
#include "cppunivariate/distributions/pois/zero.hpp"
#include "cppunivariate/distributions/binom/zero.hpp"
#include "cppunivariate/distributions/nbinom/zero.hpp"
#include "cppunivariate/distributions/geom/zero.hpp"
#include "cppunivariate/distributions/norm/truncation.hpp"
#include "distributions/ztpois/fit.hpp"
#include "distributions/ztbinom/fit.hpp"
#include "distributions/ztnbinom/fit.hpp"
#include "distributions/ztgeom/fit.hpp"
#include "distributions/zmpois/fit.hpp"
#include "distributions/zmbinom/fit.hpp"
#include "distributions/zmnbinom/fit.hpp"
#include "distributions/zmgeom/fit.hpp"
#include "distributions/zip/fit.hpp"
#include "distributions/zib/fit.hpp"
#include "distributions/zinb/fit.hpp"
#include "distributions/tnorm/fit.hpp"
#include "cppunivariate/distributions/exp/censored.hpp"
#include "cppunivariate/distributions/weibull/censored.hpp"
#include "cppunivariate/distributions/rayleigh/censored.hpp"
#include "cppunivariate/distributions/pareto/censored.hpp"
#include "cppunivariate/distributions/lomax/censored.hpp"
namespace umvl_binding {
inline FitResult modifier_not_implemented(const std::string& family,const std::string& value) {
  return failure(value+" is not implemented for `ml"+family+"`; see TASK-191 or its successor");
}
template<class Traits> inline FitResult truncate_counts(cppunivariate::ispan x,double a,double b,
    Traits traits,Held held,const std::string& name,double tol,int maxit) {
  const auto r=cppunivariate::truncated<Traits>::fit(x,a,b,traits,held.supplied?held.value:NAN,tol,maxit);
  if(r.status!=cppunivariate::TruncationStatus::Ok) {
    const std::string reason=r.status==cppunivariate::TruncationStatus::LowerLimit?"lower point-mass limit: no finite parent MLE":
      r.status==cppunivariate::TruncationStatus::UpperLimit?"upper boundary limit: no finite parent MLE":
      r.status==cppunivariate::TruncationStatus::NumericalFailure?"truncation solve failed to certify coefficient accuracy":"invalid truncation window, observations or held coefficients";
    return failure(reason);
  }
  FitResult out;out.estimates={{name,r.parameter}};out.logLik=r.logLik;
  out.iterations=r.iterations;out.has_iterations=true;out.has_fixed=true;
  out.extra.push_back({"certified",1});if(held.supplied)out.fixed.push_back(name);return out;
}
inline FitResult fit_modifier(const std::string& family,cppunivariate::span x,
    const std::vector<std::pair<std::string,Held>>& holds,const std::string& zero,
    bool window,double a,double b,double tol=1e-10,int maxit=200) {
  for(const auto& p:holds)if(p.second.supplied && !std::isfinite(p.second.value))
    return failure("held coefficients must be finite scalars");
  const auto h=[&](const std::string& name){for(const auto& p:holds)if(p.first==name)return p.second;return Held{};};
  const auto value=[&](const std::string& name){const auto p=h(name);return p.supplied?p.value:NAN;};
  if(window && zero!="none")return modifier_not_implemented(family,"zero + truncate");
  if(!window && zero=="none")return modifier_not_implemented(family,"modifier");
  if(!(tol>0 && std::isfinite(tol)) || maxit<1)return failure("invalid modifier controls");
  if(family=="norm" && window) {
    auto out=fit_tnorm(x,h("mean"),h("sd"),{!std::isnan(a),a},{!std::isnan(b),b},tol,maxit);
    out.estimates.erase(std::remove_if(out.estimates.begin(),out.estimates.end(),[](const auto& p){return p.first=="a"||p.first=="b";}),out.estimates.end());
    out.fixed.erase(std::remove_if(out.fixed.begin(),out.fixed.end(),[](const auto& p){return p=="a"||p=="b";}),out.fixed.end());return out;
  }
  if(family!="pois" && family!="binom" && family!="nbinom" && family!="geom")
    return modifier_not_implemented(family,window?"`truncate`":"`zero = \""+zero+"\"`");
  std::vector<int> counts;counts.reserve(x.size());
  for(double v:x){if(!std::isfinite(v)||v<0||v>2147483647.||v!=std::floor(v))return failure("requires nonnegative representable integer counts");counts.push_back(static_cast<int>(v));}
  const cppunivariate::ispan ix{counts.data(),counts.size()};
  if(window && a==1 && b==INFINITY){window=false;return fit_modifier(family,x,holds,"truncated",false,NAN,NAN,tol,maxit);}
  if(window) {
    FitResult out;
    if(family=="pois")return truncate_counts(ix,a,b,cppunivariate::PoisTruncationTraits{},h("lambda"),"lambda",tol,maxit);
    if(family=="geom")return truncate_counts(ix,a,b,cppunivariate::GeomTruncationTraits{},h("prob"),"prob",tol,maxit);
    if(!h("size").supplied)return modifier_not_implemented(family,"`truncate` with size free");
    if(family=="binom")out=truncate_counts(ix,a,b,cppunivariate::BinomTruncationTraits{value("size")},h("prob"),"prob",tol,maxit);
    else out=truncate_counts(ix,a,b,cppunivariate::NbinomTruncationTraits{value("size")},h("prob"),"prob",tol,maxit);
    out.estimates.insert(out.estimates.begin(),{"size",value("size")});out.fixed.insert(out.fixed.begin(),"size");return out;
  }
  if(zero=="truncated") {
    if(family=="pois")return fit_ztpois(ix,h("lambda"));
    if(family=="geom")return fit_ztgeom(ix,h("prob"));
    if(family=="binom")return fit_ztbinom(ix,h("size"),h("prob"),tol,maxit);
    return fit_ztnbinom(ix,h("size"),h("prob"),tol,maxit);
  }
  if(zero=="modified") {
    if(family=="pois")return fit_zmpois(ix,h("lambda"),h("p0"));
    if(family=="geom")return fit_zmgeom(ix,h("prob"),h("p0"));
    if(family=="binom")return fit_zmbinom(ix,h("size"),h("prob"),h("p0"),tol,maxit);
    return fit_zmnbinom(ix,h("size"),h("prob"),h("p0"),tol,maxit);
  }
  if(zero=="inflated") {
    if(family=="pois")return fit_zip(ix,h("lambda"),h("pi"),maxit);
    if(family=="binom")return fit_zib(ix,h("size"),h("prob"),h("pi"),tol,maxit);
    auto out=fit_zinb(ix,family=="geom"?Held{true,1}:h("size"),h("prob"),h("pi"),tol,maxit);
    if(family=="geom") {
      out.estimates.erase(std::remove_if(out.estimates.begin(),out.estimates.end(),[](const auto& p){return p.first=="size";}),out.estimates.end());
      out.fixed.erase(std::remove(out.fixed.begin(),out.fixed.end(),"size"),out.fixed.end());
    }
    return out;
  }
  return failure("`zero` must be one of none, truncated, modified, inflated");
}
template<class Result> inline FitResult censored_result(const std::string& family,const Result& r,
    std::vector<std::pair<std::string,double>> estimates,const std::vector<std::pair<std::string,Held>>& holds) {
  const auto status=r.status;
  if(status!=decltype(status)::Ok) {
    if(status==decltype(status)::NotImplemented)return modifier_not_implemented(family,"censoring scheme or hold set");
    return failure(status==decltype(status)::InvalidInput?"invalid censored observations or held coefficients":status==decltype(status)::AllCensored?"AllCensored: no exact observations":status==decltype(status)::Unbounded?"censored likelihood is unbounded":"censored solve failed to certify coefficient accuracy");
  }
  FitResult out;out.estimates=estimates;out.logLik=r.logLik;out.has_fixed=true;
  out.extra.push_back({"certified",1});
  for(const auto& p:holds)if(p.second.supplied)out.fixed.push_back(p.first);
  return out;
}
inline FitResult fit_censored(const std::string& family,cppunivariate::CensoredData x,
    const std::vector<std::pair<std::string,Held>>& holds,double tol=1e-10,int maxit=400) {
  for(const auto& p:holds)if(p.second.supplied && !std::isfinite(p.second.value))
    return failure("held coefficients must be finite scalars");
  const auto v=[&](const std::string& name) -> double {for(const auto& p:holds)if(p.first==name && p.second.supplied)return p.second.value;return NAN;};
  if(family=="exp"){auto r=cppunivariate::exp_censored_mle(x,v("rate"));return censored_result(family,r,{{"rate",r.rate}},holds);}
  if(family=="weibull"){auto r=cppunivariate::weibull_censored_mle(x,v("shape"),v("scale"),tol,maxit);return censored_result(family,r,{{"shape",r.shape},{"scale",r.scale}},holds);}
  if(family=="rayleigh"){auto r=cppunivariate::rayleigh_censored_mle(x,v("sigma"));return censored_result(family,r,{{"sigma",r.sigma}},holds);}
  if(family=="pareto"){auto r=cppunivariate::pareto_censored_mle(x,v("a"),v("b"));return censored_result(family,r,{{"a",r.shape},{"b",r.min}},holds);}
  if(family=="lomax"){auto r=cppunivariate::lomax_censored_mle(x,v("lambda"),v("kappa"));return censored_result(family,r,{{"lambda",r.lambda},{"kappa",r.kappa}},holds);}
  return modifier_not_implemented(family,"censoring");
}
}
