#pragma once
#include "../../core/face_compare.hpp"
#include "../../core/precision.hpp"
#include "../gb2/mle.hpp"
#include "../gamma/mle.hpp"
#include <array>

namespace cppunivariate {
enum class GenbetaStatus { Ok, UnitGammaBoundary, PowerBoundary, InvalidData,
  InvalidFixed, SearchUnresolved };
struct GenbetaResult {
  double shape1, shape2, shape3, scale, logLik;
  int iterations;
  GenbetaStatus status;
};
namespace genbeta_detail {
struct Point { double a=1,b=1,t=1,ll=-std::numeric_limits<double>::infinity(),score=0; bool ok=false; };
// Concave beta exponential-family problem from sufficient statistics, without
// rounding exp(-t*y) to a support endpoint. Held shapes restrict its tangent.
inline Point profile(const std::vector<double>& y,double t,double aa,double bb,int maxit) {
  const double n=static_cast<double>(y.size());
  CPPUNIV_EXTENDED_REAL ss=0,rr=0,dd=0;
  for(double v:y) {
    const double z=t*v;
    ss+=z;
    rr-=z<.6931471805599453?std::log(-std::expm1(-z)):std::log1p(-std::exp(-z));
    dd+=z>700?0:z/std::expm1(z);
  }
  const double s=static_cast<double>(ss/n),r=static_cast<double>(rr/n);
  const bool fa=std::isnan(aa),fb=std::isnan(bb);
  if(!(s>0)||!std::isfinite(s)||!std::isfinite(r)||(fb&&!(r>0)))return {};
  double a=fa?std::max(.01,.5/s):aa,b=fb?std::max(.01,.5/r):bb;
  auto ll=[&](double av,double bv) {return -av*s-(bv-1)*r-gb2_detail::logbeta(av,bv);};
  if(a==1&&!fa) {
    if(fb)b=1/r;
    const double value=n*(std::log(t)+ll(a,b));
    return {a,b,t,value,1-a*s+(b-1)*static_cast<double>(dd/n),std::isfinite(value)&&std::isfinite(b)};
  }
  bool ok=false;
  for(int it=0;it<maxit;++it) {
    const double u=trgamma_detail::log_minus_digamma(a+b);
    const double ga=fa?a*(-s+std::log1p(b/a)+trgamma_detail::log_minus_digamma(a)-u):0;
    const double gb=fb?b*(-r+std::log1p(a/b)+trgamma_detail::log_minus_digamma(b)-u):0;
    if(std::hypot(ga,gb)<1e-9){ok=true;break;}
    const double tri=special::trigamma(a+b);
    const double h1=a*a*(special::trigamma(a)-tri),h2=b*b*(special::trigamma(b)-tri),h12=-a*b*tri;
    double da=0,db=0;
    if(fa&&fb) {
      const double det=h1*h2-h12*h12;
      if(!(det>0))break;
      da=(h2*ga-h12*gb)/det;db=(h1*gb-h12*ga)/det;
    } else if(fa)da=ga/h1;
    else if(fb)db=gb/h2;
    else {ok=true;break;}
    const double norm=std::max(std::abs(da),std::abs(db));
    if(norm>2){da*=2/norm;db*=2/norm;}
    bool accepted=false;
    for(double step=1;step>1e-12;step*=.5) {
      const double an=a*std::exp(step*da),bn=b*std::exp(step*db);
      if(std::isfinite(an+bn)&&ll(an,bn)>=ll(a,b)-1e-13) {a=an;b=bn;accepted=true;break;}
    }
    if(!accepted)break;
  }
  const double value=n*(std::log(t)+ll(a,b));
  return {a,b,t,value,1-a*s+(b-1)*static_cast<double>(dd/n),ok&&std::isfinite(value)&&std::isfinite(a)&&std::isfinite(b)};
}
} // namespace genbeta_detail

inline GenbetaResult genbeta_mle(span x,double scale,double tol=1e-7,int maxit=300,
    double a=std::numeric_limits<double>::quiet_NaN(),
    double b=std::numeric_limits<double>::quiet_NaN(),
    double t=std::numeric_limits<double>::quiet_NaN()) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  GenbetaResult bad{a,b,t,scale,nan,0,GenbetaStatus::InvalidData};
  if(!(scale>0)||!std::isfinite(scale)){bad.status=GenbetaStatus::InvalidFixed;return bad;}
  for(double v:{a,b,t})if(!std::isnan(v)&&(!(v>0)||!std::isfinite(v))){bad.status=GenbetaStatus::InvalidFixed;return bad;}
  if(x.size()==0||!(tol>0)||!std::isfinite(tol)||maxit<1)return bad;
  std::vector<double> y;CPPUNIV_EXTENDED_REAL jac=0,sy=0;double ymin=std::numeric_limits<double>::infinity();
  for(double v:x) {
    if(!(v>0&&v<scale)||!std::isfinite(v))return bad;
    const double z=v>scale*.5?-std::log1p((v-scale)/scale):std::log(scale)-std::log(v);
    y.push_back(z);jac-=std::log(v);sy+=z;ymin=std::min(ymin,z);
  }
  const double n=static_cast<double>(x.size()),mean=static_cast<double>(sy/n);
  bool constant=true;for(double v:y)constant=constant&&v==y[0];
  // Free concentration can collapse on constant samples. With both beta
  // shapes held, the one-dimensional scale-in-y problem remains meaningful.
  if(constant&&std::isnan(b)&&(std::isnan(a)||std::isnan(t)))return bad;
  double face=-std::numeric_limits<double>::infinity();
  GenbetaStatus fs=GenbetaStatus::SearchUnresolved;
  auto consider=[&](double ll,GenbetaStatus s){if(std::isfinite(ll)&&ll>face){face=ll;fs=s;}};
  if(std::isnan(t)&&std::isnan(a)) {
    if(std::isnan(b)) {
      const auto g=gamma_mle({y.data(),y.size()},1e-10,300);
      if(g.status!=GammaStatus::Ok){bad.status=GenbetaStatus::SearchUnresolved;return bad;}
      consider(g.logLik,GenbetaStatus::UnitGammaBoundary);
      CPPUNIV_EXTENDED_REAL total_gap=0;for(double v:y)total_gap+=v-ymin;
      const double gap=static_cast<double>(total_gap/n);
      if(gap>0)consider(n*(-std::log(gap)-1),GenbetaStatus::PowerBoundary);
    } else {
      CPPUNIV_EXTENDED_REAL sl=0;for(double v:y)sl+=std::log(v);
      const double logmean=static_cast<double>(sl/n);
      const double contrast=gamma_detail::robust_s({y.data(),y.size()},mean);
      const double base=b>=100?.5*std::log(b)-.5*std::log(6.2831853071795864769)-gamma_detail::stirlerr(b):b*std::log(b)-b-special::lgamma(b);
      consider(n*(base-b*contrast-logmean),GenbetaStatus::UnitGammaBoundary);
      if(b!=1)consider(n*(-std::log(mean)-1),GenbetaStatus::PowerBoundary);
    }
  }
  // beta=1 is an attained ridge: only alpha*tau is identified.
  if(b==1) {
    const double rate=1/mean;
    if(std::isnan(a)&&std::isnan(t)){a=rate;t=1;}
    else if(std::isnan(a))a=rate/t;
    else if(std::isnan(t))t=rate/a;
    const double value=n*(std::log(a*t)-a*t*mean)+static_cast<double>(jac);
    return {a,b,t,scale,value,0,std::isfinite(value)&&std::isfinite(a)&&std::isfinite(t)?GenbetaStatus::Ok:GenbetaStatus::SearchUnresolved};
  }
  int evals=0;
  auto at=[&](double z){++evals;return genbeta_detail::profile(y,std::exp(z),a,b,maxit);};
  genbeta_detail::Point best;
  bool edge=false;
  if(!std::isnan(t)){++evals;best=genbeta_detail::profile(y,t,a,b,maxit); }
  else {
    // Logarithmic mesh nominates every visible local maximum. Refine each
    // sign change of the analytic envelope score by bisection.
    const double lo=-12-std::log(mean);
    double hi=std::log(100/ymin);
    if(!std::isnan(a))hi=std::max(hi,std::log(100.0)-std::log(a)-std::log(mean));
    if(!std::isnan(b))hi=std::max(hi,std::log(std::max(100.0,std::log(b)+100))-std::log(ymin));
    const int count=240;
    auto prev=at(lo); if(prev.ok)best=prev;
    for(int i=1;i<=count;++i) {
      const double z=lo+(hi-lo)*i/count;
      auto cur=at(z);
      if(cur.ok&&cur.ll>best.ll){best=cur;edge=i==count;}
      if(prev.ok&&cur.ok&&prev.score>0&&cur.score<0) {
        double l=z-(hi-lo)/count,h=z;
        for(int j=0;j<std::min(maxit,80);++j) {
          const double m=(l+h)/2;auto p=at(m);
          if(!p.ok)break;
          if(p.ll>best.ll||(p.ll>=best.ll-1e-12*n&&std::abs(p.score)<std::abs(best.score))){best=p;edge=false;}
          if(std::abs(p.score)<std::min(tol,1e-9)||h-l<1e-11)break;
          if(p.score>0)l=m;else h=m;
        }
      }
      prev=cur;
    }
  }
  if(best.ok && face_numerical_tie(face,best.ll,1e-8*n)) return {nan,nan,nan,scale,face,evals,GenbetaStatus::SearchUnresolved};
  if(best.ok&&face>=best.ll-1e-8*n)return {nan,nan,nan,scale,face+static_cast<double>(jac),evals,fs};
  const bool ok=best.ok&&!edge&&(!std::isnan(t)||std::abs(best.score)<tol);
  return {best.a,best.b,best.t,scale,best.ll+static_cast<double>(jac),evals,
    ok?GenbetaStatus::Ok:GenbetaStatus::SearchUnresolved};
}
} // namespace cppunivariate
