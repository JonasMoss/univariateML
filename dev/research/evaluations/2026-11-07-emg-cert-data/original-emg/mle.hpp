#pragma once
#include "../../core/work_budget.hpp"
#include "../../core/stop_detail.hpp"
#include "../../core/face_compare.hpp"
#include "logconcave_profile.hpp"
#include "../../core/precision.hpp"
#include "../../core/multistart.hpp"
#include "dpqr.hpp"
#include "../../core/span.hpp"
#include <vector>
#include <map>
#include <array>

namespace cppunivariate {
enum class EmgStatus { Ok, NormalLimit, TranslatedExponentialLimit, Unbounded, InvalidInput, IterationFailure };
struct EmgResult { double mu,sigma,lambda,logLik; int iterations; EmgStatus status;  StopDetail stop_detail{}; };
namespace emg_detail {
struct Profile {
  const std::vector<double>& x;
  double mu,sigma,lambda;int maxit=500;
  double location(double s,double l) const {
    if(!std::isnan(mu)) return mu;
    auto score=[&](double m) {
      CPPUNIV_EXTENDED_REAL sum=0,deriv=0,a=static_cast<CPPUNIV_EXTENDED_REAL>(s)*l;
      for(double v:x) {
        const CPPUNIV_EXTENDED_REAL z=(static_cast<CPPUNIV_EXTENDED_REAL>(v)-m)/s,t=a-z;
        if(t>=12) {
          CPPUNIV_EXTENDED_REAL r=0; for(int k=40;k>=2;--k) r=k/(t+r);
          const CPPUNIV_EXTENDED_REAL h=1/(t+r);
          sum+=z-h;deriv-=(t+h)*h/s;
        } else {const CPPUNIV_EXTENDED_REAL r=std::exp(-logmills(t));sum+=a-r;deriv-=r*(r-t)/s;}
      }
      return std::array<CPPUNIV_EXTENDED_REAL,2>{{sum,deriv}};
    };
    double step=std::max(s,1/l),lo=x.front()-step,hi=x.back()+step;
    for(int k=0;k<maxit && score(lo)[0]<0;++k) {step*=2;lo-=step;}
    for(int k=0;k<maxit && score(hi)[0]>0;++k) {step*=2;hi+=step;}
    double m=lo/2+hi/2;
    for(int k=0;k<maxit;++k) {
      const auto g=score(m);
      if(g[0]>0)lo=m;else hi=m;
      double next=m-static_cast<double>(g[0]/g[1]);
      if(std::abs(next-m)<1e-12*s)return next;
      if(!std::isfinite(next)||next<=lo||next>=hi)next=lo/2+hi/2;
      m=next;
    }
    return NAN;
  }
  // Reuse only identical held-profile coordinates, including across starts.
  // Clearing the bounded cache changes cost only, never search decisions.
  mutable std::map<std::vector<double>,EmgResult> evaluations{};
  EmgResult evaluate(const std::vector<double>& t) const {
    if(std::isnan(mu) && std::isnan(sigma) && std::isnan(lambda)) return evaluate_uncached(t);
    for(double v:t) if(std::isnan(v) || v==0) return evaluate_uncached(t);
    const auto found=evaluations.find(t);
    if(found!=evaluations.end()) return found->second;
    const auto value=evaluate_uncached(t);
    if(evaluations.size()>=4096) evaluations.clear();
    evaluations.emplace(t,value);
    return value;
  }
  EmgResult evaluate_uncached(const std::vector<double>& t) const {
    int k=0;
    const double s=std::isnan(sigma)?std::exp(t[k++]):sigma;
    const double l=std::isnan(lambda)?std::exp(-t[k++]):lambda;
    if(!(s>0 && l>0 && std::isfinite(s) && std::isfinite(l))) return {0,s,l,special::kNegInf,0,EmgStatus::IterationFailure};
    const double m=location(s,l);
    double ll=0;for(double v:x) ll+=distributions::demg(v,m,s,l,true);
    return {m,s,l,ll,0,EmgStatus::Ok};
  }
};
// Nelder-Mead on the one/two remaining log-scale coordinates. Location is
// profiled exactly (strict log concavity of the normal-exponential convolution).
inline EmgResult search(const Profile& p,std::vector<double> start,double tol,int maxit) {
  maxit = static_cast<int>(solver_budget(maxit));
  const int d=static_cast<int>(start.size());
  if(!d) return p.evaluate(start);
  std::vector<std::vector<double>> v(d+1,start);
  for(int i=0;i<d;++i) v[i+1][i]+=.4;
  std::vector<EmgResult> f;for(const auto& t:v)f.push_back(p.evaluate(t));
  for(int it=0;it<maxit;++it) {
    for(int i=0;i<=d;++i)for(int j=i+1;j<=d;++j)if(f[j].logLik>f[i].logLik){std::swap(f[i],f[j]);std::swap(v[i],v[j]);}
    double diameter=0;for(int i=1;i<=d;++i)for(int j=0;j<d;++j)diameter=std::max(diameter,std::abs(v[i][j]-v[0][j]));
    if(diameter<tol) {f[0].iterations=it;return f[0];}
    std::vector<double> c(d,0),r(d),e(d);
    for(int j=0;j<d;++j){for(int i=0;i<d;++i)c[j]+=v[i][j]/d;r[j]=2*c[j]-v[d][j];}
    auto fr=p.evaluate(r);
    if(fr.logLik>f[0].logLik){for(int j=0;j<d;++j)e[j]=c[j]+2*(r[j]-c[j]);auto fe=p.evaluate(e);if(fe.logLik>fr.logLik){v[d]=e;f[d]=fe;}else{v[d]=r;f[d]=fr;}}
    else if(fr.logLik>f[d-1].logLik){v[d]=r;f[d]=fr;}
    else {
      const bool outside=fr.logLik>f[d].logLik;
      for(int j=0;j<d;++j)e[j]=c[j]+.5*((outside?r[j]:v[d][j])-c[j]);
      auto fe=p.evaluate(e);
      if(fe.logLik>(outside?fr.logLik:f[d].logLik)){v[d]=e;f[d]=fe;}
      else for(int i=1;i<=d;++i){for(int j=0;j<d;++j)v[i][j]=(v[i][j]+v[0][j])/2;f[i]=p.evaluate(v[i]);}
    }
  }
  auto best=*std::max_element(f.begin(),f.end(),[](const EmgResult& a,const EmgResult& b){return a.logLik<b.logLik;});
  best.iterations=maxit;best.status=EmgStatus::IterationFailure;
  std::vector<double> theta;
  if(std::isnan(p.mu))theta.push_back(best.mu);
  if(std::isnan(p.sigma))theta.push_back(std::log(best.sigma));
  if(std::isnan(p.lambda))theta.push_back(-std::log(best.lambda));
  const auto likelihood=[&](const std::vector<double>& t) {
    int k=0;
    const double m=std::isnan(p.mu)?t[k++]:p.mu;
    const double s=std::isnan(p.sigma)?std::exp(t[k++]):p.sigma;
    const double b=std::isnan(p.lambda)?std::exp(-t[k++]):p.lambda;
    double ll=0;for(double x:p.x)ll+=distributions::demg(x,m,s,b,true);
    return ll;
  };
  if(multistart_score_acceptable(theta,likelihood)) {
    best.logLik=likelihood(theta);best.status=EmgStatus::Ok;
  }
  if(best.status==EmgStatus::NormalLimit) best.stop_detail=limit_detail({best.mu,best.sigma},best.logLik,StopModel::Norm);
  if(best.status==EmgStatus::TranslatedExponentialLimit) best.stop_detail=limit_detail({best.mu,best.lambda},best.logLik,StopModel::TranslatedExp);
  return best;
}
}
inline EmgResult emg_mle(span x,double tol=1e-8,int maxit=500,
    double mu=special::kNaN,double sigma=special::kNaN,double lambda=special::kNaN) {
  maxit = static_cast<int>(solver_budget(maxit));
  EmgResult bad={special::kNaN,special::kNaN,special::kNaN,special::kNaN,0,EmgStatus::InvalidInput};
  if(x.empty() || !(tol>0 && std::isfinite(tol)) || maxit<1 || (!std::isnan(mu)&&!std::isfinite(mu)) || (!std::isnan(sigma)&&!(sigma>0&&std::isfinite(sigma))) || (!std::isnan(lambda)&&!(lambda>0&&std::isfinite(lambda)))) return bad;
  double mean=0;for(double v:x){if(!std::isfinite(v))return bad;mean+=v/x.size();}
  double var=0;for(double v:x)var+=(v-mean)*(v-mean)/x.size();
  const double minimum=*std::min_element(x.begin(),x.end());
  if(var==0 && std::isnan(sigma) && std::isnan(lambda) && (std::isnan(mu)||mu==mean)){bad.status=EmgStatus::Unbounded;return bad;}
  double scale=std::sqrt(var);
  if(!(scale>0)) scale=!std::isnan(sigma)?sigma:(!std::isnan(lambda)?1/lambda:1);
  std::vector<double> y;for(double v:x)y.push_back((v-mean)/scale);std::sort(y.begin(),y.end());
  emg_detail::Profile p{y,std::isnan(mu)?mu:(mu-mean)/scale,std::isnan(sigma)?sigma:sigma/scale,std::isnan(lambda)?lambda:lambda*scale,maxit};
  EmgResult best={0,0,0,special::kNegInf,0,EmgStatus::IterationFailure};
  auto attempted=best;
  bool faces_only=false;
  auto fallback_reason=logconcave_detail::FallbackReason::InnerFailure;
  logconcave_detail::reset_fallback();
  // A grid of starts covers both faces and the interior. It is a numerical
  // global search, not a certificate that every stationary mode is isolated.
  const std::vector<double> starts={-6,-3,-1,0,1};
  if(std::isnan(mu)&&std::isnan(sigma)&&std::isnan(lambda)) {
    const auto candidate=logconcave_detail::search(y,[](CPPUNIV_EXTENDED_REAL z,CPPUNIV_EXTENDED_REAL k) {
      const CPPUNIV_EXTENDED_REAL t=static_cast<CPPUNIV_EXTENDED_REAL>(k)-z;
      CPPUNIV_EXTENDED_REAL g,h;
      if(t>=12) { CPPUNIV_EXTENDED_REAL r=0;for(int j=40;j>=2;--j)r=j/(t+r);const CPPUNIV_EXTENDED_REAL v=1/(t+r);g=-z+v;h=-(t+v)*v; }
      else {const CPPUNIV_EXTENDED_REAL r=std::exp(-emg_detail::logmills(t));g=r-k;h=-r*(r-t);}
      return std::array<CPPUNIV_EXTENDED_REAL,3>{{std::log(k)+emg_detail::logterm(z,k),g,h}};
    },maxit);
    if(candidate.ok && candidate.faces_only) {
      faces_only=true;best=attempted={0,0,0,candidate.ll,0,EmgStatus::IterationFailure};
    } else if(candidate.ok) {
      const double sig=1/candidate.b,lam=candidate.k*candidate.b;
      const auto polished=emg_detail::search(p,{std::log(sig),-std::log(lam)},tol,maxit);
      if(polished.status==EmgStatus::Ok) {
        best=attempted=polished;
        // Compare one existing start to preserve established fit precision.
        const auto central=emg_detail::search(p,{-6,0},tol,maxit);
        if(central.status==EmgStatus::Ok && central.logLik>=best.logLik)best=attempted=central;
      } else fallback_reason=logconcave_detail::FallbackReason::PolishFailure;
    }
  }
  auto run_multistart=[&]() {
  for(double a:starts)for(double b:starts) {
    if(!std::isnan(sigma)&&a!=starts.front())continue;
    if(!std::isnan(lambda)&&b!=starts.front())continue;
    std::vector<double> t;if(std::isnan(sigma))t.push_back(a);if(std::isnan(lambda))t.push_back(b);
    const auto fit=emg_detail::search(p,t,tol,maxit);
    if(std::isfinite(fit.logLik) && fit.logLik>attempted.logLik)attempted=fit;
    if(fit.status==EmgStatus::Ok && std::isfinite(fit.logLik) && fit.logLik>best.logLik)best=fit;
  }
  };
  if(!std::isfinite(best.logLik)) {
    if(std::isnan(mu)&&std::isnan(sigma)&&std::isnan(lambda))logconcave_detail::record_fallback(fallback_reason);
    run_multistart();
  }
  if(!std::isfinite(best.logLik))best=attempted;
  const double n=static_cast<double>(x.size()),tie=1e-8*n;
  const auto tied_face=[&](std::vector<double> coef,double ll,StopModel model) {
    const double shift=n*std::log(scale),finite=attempted.logLik-shift;
    StopDetail d;d.comparison_tie=true;
    d.candidates.push_back({coef,ll-shift,ll-shift,model});
    d.loglik_lower=ll-shift;d.loglik_upper=INFINITY;
    if(best.status==EmgStatus::NormalLimit) {
      const double face=best.logLik-shift;
      d.candidates.push_back({{mean+scale*best.mu,scale*best.sigma},face,face,StopModel::Norm});
      d.loglik_lower=std::max(d.loglik_lower,face);
    }
    if(attempted.sigma>0 && attempted.lambda>0 && std::isfinite(attempted.sigma) && std::isfinite(attempted.lambda)) {
      d.candidates.push_back({{mean+scale*attempted.mu,scale*attempted.sigma,attempted.lambda/scale},finite,finite,StopModel::None});
      d.loglik_lower=std::max(d.loglik_lower,finite);
    }
    best.status=EmgStatus::IterationFailure;best.stop_detail=d;
  };
  auto compare_faces=[&]() {
  double comparison=attempted.logLik;
  // Conditional normal face: respect every held parameter.
  if(std::isnan(lambda)) {
    const double m=std::isnan(p.mu)?0:p.mu;
    double v=0;for(double z:y)v+=(z-m)*(z-m)/n;
    const double s=std::isnan(p.sigma)?std::sqrt(v):p.sigma;
    double ll=0;for(double z:y)ll+=-std::log(s)-.91893853320467274178-(z-m)*(z-m)/(2*s*s);
    if(face_numerical_tie(ll,comparison,tie)) {tied_face({mean+scale*m,scale*s},ll,StopModel::Norm);return;}
    if(ll>=comparison-tie){best={m,s,special::kPosInf,ll,0,EmgStatus::NormalLimit};comparison=ll;}
  }
  if(std::isnan(sigma) && (std::isnan(mu)||mu<=minimum)) {
    const double m=std::isnan(p.mu)?y.front():p.mu;
    double gap=0;int ties=0;for(double z:y){gap+=z-m;if(z==m)++ties;}
    const double l=std::isnan(p.lambda)?n/gap:p.lambda;
    double ll=n*std::log(l)-l*gap;
    // A held endpoint cannot move below the minimum to recover full density.
    if(!std::isnan(mu))ll-=ties*std::log(2.0);
    if(face_numerical_tie(ll,comparison,((best.status==EmgStatus::NormalLimit)?0:tie))) {tied_face({mean+scale*m,l/scale},ll,StopModel::TranslatedExp);return;}
    if(ll>=comparison-((best.status==EmgStatus::NormalLimit)?0:tie))best={m,0,l,ll,0,EmgStatus::TranslatedExponentialLimit};
  }
  };
  if(maxit==1 && !faces_only && best.status!=EmgStatus::Ok)return {NAN,NAN,NAN,NAN,maxit,EmgStatus::IterationFailure};
  compare_faces();
  if(faces_only && best.status==EmgStatus::IterationFailure) {
    logconcave_detail::record_fallback(logconcave_detail::FallbackReason::UnresolvedFaces);
    best={0,0,0,special::kNegInf,0,EmgStatus::IterationFailure};attempted=best;
    run_multistart();
    if(!std::isfinite(best.logLik))best=attempted;
    compare_faces();
  }
  best.mu=std::isnan(mu)?mean+scale*best.mu:mu;
  best.sigma=std::isnan(sigma)?best.sigma*scale:sigma;
  best.lambda=std::isnan(lambda)?best.lambda/scale:lambda;
  best.logLik-=n*std::log(scale);
  if(best.status==EmgStatus::NormalLimit) best.stop_detail=limit_detail({best.mu,best.sigma},best.logLik,StopModel::Norm);
  if(best.status==EmgStatus::TranslatedExponentialLimit) best.stop_detail=limit_detail({best.mu,best.lambda},best.logLik,StopModel::TranslatedExp);
  return best;
}
}
