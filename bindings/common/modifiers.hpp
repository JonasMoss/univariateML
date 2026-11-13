// Additive modifier seam shared by R and JS. Default fits never enter here.
#pragma once
#include "fit.hpp"
#include "distributions/binom/tbinom_fit.hpp"
#include "distributions/pois/tpois_fit.hpp"
#include "cppunivariate/distributions/pois/tpois_dpqr.hpp"
#include "cppunivariate/distributions/binom/tbinom_dpqr.hpp"
#include "cppunivariate/distributions/norm/tnorm_dpqr.hpp"
#include "distributions/lgser/zmlogarithmic_fit.hpp"
#include "cppunivariate/distributions/zeta/truncation.hpp"
#include "cppunivariate/distributions/pois/zero.hpp"
#include "cppunivariate/distributions/binom/zero.hpp"
#include "cppunivariate/distributions/nbinom/zero.hpp"
#include "cppunivariate/distributions/geom/zero.hpp"
#include "cppunivariate/distributions/norm/truncation.hpp"
#include "distributions/pois/ztpois_fit.hpp"
#include "distributions/binom/ztbinom_fit.hpp"
#include "distributions/nbinom/ztnbinom_fit.hpp"
#include "distributions/geom/ztgeom_fit.hpp"
#include "distributions/pois/zmpois_fit.hpp"
#include "distributions/binom/zmbinom_fit.hpp"
#include "distributions/nbinom/zmnbinom_fit.hpp"
#include "distributions/geom/zmgeom_fit.hpp"
#include "distributions/pois/zip_fit.hpp"
#include "distributions/binom/zib_fit.hpp"
#include "distributions/nbinom/zinb_fit.hpp"
#include "distributions/norm/tnorm_fit.hpp"
#include "cppunivariate/distributions/exp/censored.hpp"
#include "cppunivariate/distributions/weibull/censored.hpp"
#include "cppunivariate/distributions/rayleigh/censored.hpp"
#include "cppunivariate/distributions/pareto/censored.hpp"
#include "cppunivariate/distributions/lomax/censored.hpp"
namespace umvl_binding {
inline FitResult modifier_not_implemented(const std::string& family,const std::string& value) {
  return invalid_argument(failure(value+" is not implemented for `ml"+family+"`; see TASK-191 or its successor"));
}
template<class Density> inline FitResult held_window(FitResult out,cppunivariate::span x,Density density) {
  if(x.empty())return invalid_argument(failure("requires nonempty data"));
  double sum=0,correction=0;
  for(double v:x) {
    if(!std::isfinite(v))return invalid_argument(failure("requires finite observations"));
    const double term=density(v);
    if(!std::isfinite(term))return invalid_argument(failure("invalid truncation window, observations or held coefficients"));
    const double y=term-correction,z=sum+y;correction=(z-sum)-y;sum=z;
  }
  if(!std::isfinite(sum))return with_outcome(failure("conditional log likelihood exceeds numerical range"),cppunivariate::FitOutcome::failure(cppunivariate::OutcomeReason::Evaluation),-1);
  out.logLik=sum;out.has_fixed=true;return with_outcome(std::move(out),cppunivariate::FitOutcome::estimate(true),-1);
}
template<class Traits> inline FitResult truncate_counts(cppunivariate::ispan x,double a,double b,
    Traits traits,Held held,const std::string& name,double tol,int maxit) {
  const auto r=cppunivariate::truncated<Traits>::fit(x,a,b,traits,held.supplied?held.value:NAN,tol,maxit);
  const auto o=cppunivariate::outcome(r);
  if(r.status!=cppunivariate::TruncationStatus::Ok) {
    const std::string reason=r.status==cppunivariate::TruncationStatus::LowerLimit?"lower point-mass limit: no finite parent MLE":
      r.status==cppunivariate::TruncationStatus::UpperLimit?"upper boundary limit: no finite parent MLE":
      r.status==cppunivariate::TruncationStatus::NumericalFailure?"truncation solve failed to certify coefficient accuracy":"invalid truncation window, observations or held coefficients";
    return with_outcome(failure(reason),o,static_cast<int>(r.status));
  }
  FitResult out;out.estimates={{name,r.parameter}};out.logLik=r.logLik;
  out.iterations=r.iterations;out.has_iterations=true;out.has_fixed=true;
  out.extra.push_back({"certified",1});if(held.supplied)out.fixed.push_back(name);return with_outcome(std::move(out),o,static_cast<int>(r.status));
}
inline FitResult fit_modifier(const std::string& family,cppunivariate::span x,
    const std::vector<std::pair<std::string,Held>>& holds,const std::string& zero,
    bool window,double a,double b,double tol=1e-10,int maxit=200) {
  for(const auto& p:holds)if(p.second.supplied && !std::isfinite(p.second.value))
    return invalid_argument(failure("held coefficients must be finite scalars"));
  const auto h=[&](const std::string& name){for(const auto& p:holds)if(p.first==name)return p.second;return Held{};};
  const auto value=[&](const std::string& name){const auto p=h(name);return p.supplied?p.value:NAN;};
  if(window && zero!="none")return modifier_not_implemented(family,"zero + truncate");
  if(!window && zero=="none")return modifier_not_implemented(family,"modifier");
  if(!(tol>0 && std::isfinite(tol)) || maxit<1)return invalid_argument(failure("invalid modifier controls"));
  if(family=="norm" && window) {
    if(h("mean").supplied && h("sd").supplied && !x.empty()) {
      if(std::isnan(a))a=*std::min_element(x.begin(),x.end());
      if(std::isnan(b))b=*std::max_element(x.begin(),x.end());
      FitResult out;out.estimates={{"mean",value("mean")},{"sd",value("sd")}};out.fixed={"mean","sd"};
      return held_window(out,x,[&](double v){return cppunivariate::distributions::dtnorm(v,value("mean"),value("sd"),a,b,true);});
    }
    auto out=fit_tnorm(x,h("mean"),h("sd"),{!std::isnan(a),a},{!std::isnan(b),b},tol,maxit);
    out.estimates.erase(std::remove_if(out.estimates.begin(),out.estimates.end(),[](const auto& p){return p.first=="a"||p.first=="b";}),out.estimates.end());
    out.fixed.erase(std::remove_if(out.fixed.begin(),out.fixed.end(),[](const auto& p){return p=="a"||p=="b";}),out.fixed.end());return out;
  }
  if(family!="zeta" && family!="lgser" && family!="pois" && family!="binom" && family!="nbinom" && family!="geom")
    return modifier_not_implemented(family,window?"`truncate`":"`zero = \""+zero+"\"`");
  std::vector<int> counts;counts.reserve(x.size());
  for(double v:x){if(!std::isfinite(v)||v<0||v>2147483647.||v!=std::floor(v))return invalid_argument(failure("requires nonnegative representable integer counts"));counts.push_back(static_cast<int>(v));}
  const cppunivariate::ispan ix{counts.data(),counts.size()};
  if(window && !counts.empty()) {
    if(std::isnan(a))a=*std::min_element(counts.begin(),counts.end());
    if(std::isnan(b))b=*std::max_element(counts.begin(),counts.end());
  }
  if(family=="lgser" && !window && zero=="modified") {
    auto out=fit_zmlogarithmic(ix,h("theta"),h("p0"));
    for(auto& p:out.estimates)if(p.first=="prob")p.first="theta";
    for(auto& p:out.fixed)if(p=="prob")p="theta";
    return out;
  }
  if(family=="zeta" && window) {
    const auto r=cppunivariate::zeta_window_mle(ix,a,b,value("s"),tol,maxit);
    const auto o=cppunivariate::outcome(r);
    if(r.status!=cppunivariate::ZetaWindowStatus::Ok) {
      if(r.status==cppunivariate::ZetaWindowStatus::LowerLimit || r.status==cppunivariate::ZetaWindowStatus::UpperLimit) {
        return with_outcome(stop_failure(failure("No finite exponent: point-mass supremum at a window endpoint."),
          cppunivariate::discrete_point(r.status==cppunivariate::ZetaWindowStatus::LowerLimit?1:std::floor(b)),outcome_kind(o),{"s"}),o,static_cast<int>(r.status));
      }
      return with_outcome(failure(r.status==cppunivariate::ZetaWindowStatus::InvalidInput ? "finite zeta window requires [1,N], integer N >= max(x), and finite s" : "finite zeta exponent solve failed to certify coefficient accuracy"),o,static_cast<int>(r.status));
    }
    FitResult out;out.estimates={{"s",r.s}};out.logLik=r.logLik;out.has_fixed=true;
    if(r.unidentified)out.warnings.push_back("All observations are equal to 1. The maximum likelihood estimator is not unique.");
    if(h("s").supplied)out.fixed={"s"};
    return with_outcome(std::move(out),o,static_cast<int>(r.status));
  }
  if(family=="lgser" || family=="zeta")return modifier_not_implemented(family,window?"`truncate`":"`zero`");
  if(window && a==1 && b==INFINITY){window=false;return fit_modifier(family,x,holds,"truncated",false,NAN,NAN,tol,maxit);}
  if(window) {
    FitResult out;
    if(family=="pois" && h("lambda").supplied) {
      out.estimates={{"lambda",value("lambda")}};out.fixed={"lambda"};
      return held_window(out,x,[&](double v){return cppunivariate::distributions::dtpois(v,value("lambda"),std::max(-1.,std::ceil(a)-1),std::floor(b),true);});
    }
    if(family=="binom" && h("size").supplied && h("prob").supplied) {
      out.estimates={{"size",value("size")},{"prob",value("prob")}};out.fixed={"size","prob"};out.extra.push_back({"certified",1});
      return held_window(out,x,[&](double v){return cppunivariate::distributions::dtbinom(v,value("prob"),std::max(-1.,std::ceil(a)-1),std::floor(b),value("size"),true);});
    }
    if(family=="pois") {
      const auto check=cppunivariate::truncated<cppunivariate::PoisTruncationTraits>::fit(ix,a,b,{},value("lambda"),tol,maxit);
      if(check.status==cppunivariate::TruncationStatus::NumericalFailure)
        return with_outcome(failure("truncation solve failed to certify coefficient accuracy"),cppunivariate::outcome(check),static_cast<int>(check.status));
      if(check.status==cppunivariate::TruncationStatus::InvalidInput)
        return with_outcome(failure("invalid truncation window, observations or held coefficients"),cppunivariate::outcome(check),static_cast<int>(check.status));
      auto out=fit_tpois(ix,h("lambda"),{true,std::max(-1.0,std::ceil(a)-1)},{true,std::floor(b)});
      out.estimates.erase(std::remove_if(out.estimates.begin(),out.estimates.end(),[](const auto& p){return p.first=="a"||p.first=="b";}),out.estimates.end());
      out.fixed.erase(std::remove_if(out.fixed.begin(),out.fixed.end(),[](const auto& p){return p=="a"||p=="b";}),out.fixed.end());return out;
    }
    if(family=="geom")return truncate_counts(ix,a,b,cppunivariate::GeomTruncationTraits{},h("prob"),"prob",tol,maxit);
    if(family=="binom") {
      auto out=fit_tbinom_controlled(ix,h("prob"),{true,std::max(-1.0,std::ceil(a)-1)}, {true,std::floor(b)},h("size"),tol,maxit);
      out.estimates.erase(std::remove_if(out.estimates.begin(),out.estimates.end(),[](const auto& p){return p.first=="a"||p.first=="b";}),out.estimates.end());
      std::reverse(out.estimates.begin(),out.estimates.end());
      out.fixed.erase(std::remove_if(out.fixed.begin(),out.fixed.end(),[](const auto& p){return p=="a"||p=="b";}),out.fixed.end());return out;
    }
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
  return invalid_argument(failure("`zero` must be one of none, truncated, modified, inflated"));
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
