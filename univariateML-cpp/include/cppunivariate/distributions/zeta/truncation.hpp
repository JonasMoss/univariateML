// Finite [1,N] zeta exponential family, with the full real exponent domain.
#pragma once
#include "../../core/outcome.hpp"
#include "../../special/special.hpp"
#include "../../special/tails.hpp"
#include "finite_harmonic.hpp"
#include "zipf_dpqr.hpp"
namespace cppunivariate {
enum class ZetaWindowStatus { Ok, InvalidInput, LowerLimit, UpperLimit, NumericalFailure };
struct ZetaWindowResult { double s=NAN, logLik=NAN; ZetaWindowStatus status=ZetaWindowStatus::InvalidInput; bool unidentified=false; };
namespace zeta_window_detail {
struct Moments { double h=0,l=0,log_h=NAN; };
// Center at the range's upper endpoint. The first moment is in log(X/b).
inline double log_ratio(double x,double b) {
  return x>b/2 ? std::log1p((x-b)/b) : std::log(x/b);
}
inline Moments negative_range(int a,int b,double r,std::int64_t& remaining) {
  Moments out;
  const auto add=[&](double w,double t,double& ch,double& cl) {
    const double yh=w-ch,zh=out.h+yh;ch=(zh-out.h)-yh;out.h=zh;
    const double yl=w*t-cl,zl=out.l+yl;cl=(zl-out.l)-yl;out.l=zl;
  };
  double ch=0,cl=0,other=0,co=0;
  if(b-a<2048 || r/b>.01) {
    for(std::int64_t k=b;k>=a;--k) {
      if(--remaining<0)return {};
      const double t=log_ratio(double(k),b),w=std::exp(r*t);
      add(w,t,ch,cl);
      if(k<b) {const double y=w-co,z=other+y;co=(z-other)-y;other=z;}
      if(k>a) {
        const double ratio=std::exp(r*std::log1p(-1.0/k));
        const double tail=ratio<1?w*ratio/(1-ratio):INFINITY;
        if(tail<=out.h*1e-18 && tail*std::max(1.0,std::log(double(b)))<=std::abs(out.l)*1e-18)break;
      }
    }
    out.log_h=std::log1p(other);
    return out;
  }
  // Euler--Maclaurin through B8. At r/b <= .01 the eighth-derivative
  // remainder is small. Sum an initial prefix to keep the lower endpoint
  // away from the noninteger-power singularity at zero.
  remaining-=2048;
  if(remaining<0)return {};
  const int K=std::min(b,std::max(a,1024));
  for(int k=a;k<=K;++k) {
    const double t=log_ratio(k,b);add(std::exp(r*t),t,ch,cl);
  }
  const double t=log_ratio(K,b),w=std::exp(r*t),q=r+1,z=q*t,ez=std::exp(z);
  out.h+=b*(-std::expm1(z))/q+.5*(1-w);
  out.l+=b*(std::expm1(z)-z*ez)/(q*q)-.5*w*t;
  const auto polynomial=[&](int m,double x) {
    std::pair<double,double> p{1,0};
    for(int j=0;j<m;++j) {p.second=p.second*((r-j)/x)+p.first/x;p.first*=(r-j)/x;}
    return p;
  };
  for(int m:{1,3,5,7}) {
    const double c=m==1?1./12:m==3?-1./720:m==5?1./30240:-1./1209600;
    const auto upper=polynomial(m,b),lower=polynomial(m,K);
    out.h+=c*(upper.first-w*lower.first);
    out.l+=c*(upper.second-w*(lower.second+lower.first*t));
  }
  double bh,bl;
  if(r>8) {
    bh=std::abs(polynomial(7,b).first)/1209600;
    double factor=1/(r-7);
    for(int j=0;j<8;++j)factor+=1/(r-j);
    bl=bh*factor;
  } else {
    // On the finite interval these derivative bounds remain finite at r=7.
    // For r<=8, x^(r-8) and |log(x/b)| are largest at the lower endpoint.
    const auto p=polynomial(8,K);
    bh=(b-K)*w*std::abs(p.first)/1209600;
    bl=(b-K)*w*(std::abs(p.second)+std::abs(p.first*t))/1209600;
  }
  if(!(out.h>0) || bh>out.h*1e-18 || bl>std::abs(out.l)*1e-18)return {NAN,NAN,NAN};
  out.log_h=std::log(out.h);
  return out;
}
inline Moments moments(int N,double s,std::int64_t& remaining) {
  if(s<0)return negative_range(1,N,-s,remaining);
  remaining-=std::min(N,2048);
  if(remaining<0)return {};
  if(s>32) {
    // Direct summation avoids overflowing the endpoint polynomials at huge
    // finite exponents. The omitted integral and its log moment bound the
    // decreasing positive tail. Keep log(H) accurate when H rounds to one.
    double other=0,l=0,ch=0,cl=0;
    const int K=std::min(N,1024);
    for(int k=2;k<=K;++k) {
      const double t=std::log(double(k)),w=std::exp(-s*t);
      const double y=w-ch,z=other+y;ch=(z-other)-y;other=z;
      const double yl=w*t-cl,zl=l+yl;cl=(zl-l)-yl;l=zl;
    }
    if(N>K) {
      const double tail=std::exp((1-s)*std::log(double(K)))/(s-1);
      if(tail>other*1e-18 || tail*(std::log(double(K))+1/(s-1))>l*1e-18)return {NAN,NAN,NAN};
    }
    return {1+other,l,std::log1p(other)};
  }
  const auto p=zipf_detail::moments(N,s);
  return {p.h,p.l,std::log(p.h)};
}
inline double log_range(int a,int b,int N,double s) {
  if(a>b)return -INFINITY;
  if(s>=0)return distributions::zipf_dpqr_detail::log_range(a,b,s);
  std::int64_t remaining=std::numeric_limits<std::int64_t>::max();
  const auto m=negative_range(a,b,-s,remaining);
  return -s*log_ratio(b,N)+m.log_h;
}
}
inline ZetaWindowResult zeta_window_mle(ispan x,double a,double b,double held=NAN,double tol=1e-10,int maxit=200) {
  ZetaWindowResult out;
  a=std::max(1.0,std::ceil(a));b=std::floor(b);
  if(x.empty() || a!=1 || !std::isfinite(b) || b<1 || b!=std::floor(b) || b>2147483647. || !(tol>0) || !std::isfinite(tol) || maxit<1 || (!std::isnan(held)&&!std::isfinite(held)))return out;
  const double requested_tol=tol;
  tol=std::min(tol,1e-12);
  const int N=static_cast<int>(b);
  double target=0,centered_target=0,target_c=0,centered_c=0;
  bool lower=true,upper=true;
  for(int v:x) {
    if(v<1 || v>N)return out;
    const double y=std::log(double(v))-target_c,t=target+y;
    target_c=(t-target)-y;target=t;
    const double cy=zeta_window_detail::log_ratio(v,N)-centered_c,ct=centered_target+cy;
    centered_c=(ct-centered_target)-cy;centered_target=ct;
    lower=lower&&v==1;upper=upper&&v==N;
  }
  target/=x.size();centered_target/=x.size();
  if(N==1){out.unidentified=std::isnan(held);out.s=std::isnan(held)?1:held;out.logLik=0;out.status=ZetaWindowStatus::Ok;return out;}
  if(std::isnan(held) && (lower||upper)) {out.status=lower?ZetaWindowStatus::LowerLimit:ZetaWindowStatus::UpperLimit;return out;}
  std::int64_t cells=enumeration_budget(maxit);
  const auto evaluate=[&](double s,zeta_window_detail::Moments& m) {
    m=zeta_window_detail::moments(N,s,cells);
    if(cells<0)return false;
    return m.h>0 && std::isfinite(m.h) && std::isfinite(m.l) && std::isfinite(m.log_h);
  };
  double s=held;
  if(std::isnan(s)) {
    double lo=-1,hi=1;
    zeta_window_detail::Moments m;
    bool bracket=false;
    for(std::int64_t i=0;i<solver_budget(maxit);++i) {
      if(!evaluate(lo,m))break;
      if(m.l/m.h>=centered_target) {bracket=true;break;} lo*=2;
    }
    if(!bracket){out.status=ZetaWindowStatus::NumericalFailure;return out;}
    bracket=false;
    for(std::int64_t i=0;i<solver_budget(maxit);++i) {
      if(!evaluate(hi,m))break;
      if(m.l/m.h<=target){bracket=true;break;} hi*=2;
    }
    if(!bracket){out.status=ZetaWindowStatus::NumericalFailure;return out;}
    bool closed=false;
    for(std::int64_t i=0;i<solver_budget(maxit);++i) {
      s=lo+(hi-lo)/2;
      if(hi-lo<=tol*std::max(1.0,std::abs(s))) {closed=true;break;}
      if(!evaluate(s,m))break;
      if(m.l/m.h>(s<0?centered_target:target))lo=s;else hi=s;
    }
    if(!closed){out.status=ZetaWindowStatus::NumericalFailure;return out;}
  }
  zeta_window_detail::Moments m;
  if(!evaluate(s,m)){out.status=ZetaWindowStatus::NumericalFailure;return out;}
  out.s=s;
  out.logLik=x.size()*(-s*(s<0?centered_target:target)-m.log_h);
  out.status=std::isfinite(out.logLik)?ZetaWindowStatus::Ok:ZetaWindowStatus::NumericalFailure;
  if(out.status==ZetaWindowStatus::Ok && std::isnan(held)) {
    // A narrow bracket of rounded scores is insufficient near zero. Confirm
    // the requested coefficient neighbourhood with padded score signs.
    const double relative=std::min(requested_tol,1.0);
    const double radius=relative/(1+relative)*std::max(1.0,std::abs(s));
    const auto confirmed=[&](double coefficient,bool left) {
      zeta_window_detail::Moments checked;
      if(!evaluate(coefficient,checked))return false;
      const double observed=coefficient<0?centered_target:target;
      const double expected=checked.l/checked.h;
      const double margin=128*std::numeric_limits<double>::epsilon()*(std::abs(expected)+std::abs(observed));
      const double score=expected-observed;
      return left?score>margin:score < -margin;
    };
    if(!confirmed(s-radius,true) || !confirmed(s+radius,false))out.status=ZetaWindowStatus::NumericalFailure;
  }
  return out;
}
// Conditional evaluation is separate from zeta's improper s<=1 parent.
inline double zeta_window_eval(char kind,double v,int N,double s,bool lower=true,bool log=false,bool fuzz=true) {
  if(kind=='r')return zeta_window_eval('q',v,N,s,lower,log,false);
  if(N<1 || !std::isfinite(s) || std::isnan(v))return NAN;
  if(s>=0 && kind=='d')return distributions::dzipf(v,N,s,log);
  if(!fuzz && s>=0 && kind!='d' && kind!='p')return distributions::zipf_quantile(v,N,s,lower,log,false);
  const double den=kind=='d'||kind=='p'?zeta_window_detail::log_range(1,N,N,s):0;
  if(kind=='d') {
    const double lp=v<1||v>N||v!=std::floor(v)?-INFINITY:-s*zeta_window_detail::log_ratio(v,N)-den;
    return log?lp:std::exp(lp);
  }
  if(kind=='p') {
    if(v<1)return log?(lower?-INFINITY:0):(lower?0:1);
    if(v>=N)return log?(lower?0:-INFINITY):(lower?1:0);
    const int k=static_cast<int>(std::floor(v));
    const double below=zeta_window_detail::log_range(1,k,N,s)-den;
    const double above=zeta_window_detail::log_range(k+1,N,N,s)-den;
    const double lp=lower?(below<above?below:special::log1mexp(std::min(0.0,above))):(above<below?above:special::log1mexp(std::min(0.0,below)));
    return log?lp:std::exp(lp);
  }
  if(log?v>0:(v<0||v>1))return NAN;
  if(v==(log?-INFINITY:0))return lower?1:N;
  if(v==(log?0:1))return lower?N:1;
  return special::discrete_search(1,N,[&](double k) {
    return special::discrete_reached(zeta_window_eval('p',k,N,s,lower,log),v,lower,log,fuzz);
  });
}
}

namespace cppunivariate {
inline FitOutcome outcome(const ZetaWindowResult& r) {
  switch(r.status) {
    case ZetaWindowStatus::Ok: return FitOutcome::estimate(true, !r.unidentified);
    case ZetaWindowStatus::InvalidInput: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case ZetaWindowStatus::LowerLimit: return FitOutcome::limit(true);
    case ZetaWindowStatus::UpperLimit: return FitOutcome::limit(true);
    case ZetaWindowStatus::NumericalFailure: return FitOutcome::failure(OutcomeReason::NoConvergence);
  }
  return {};
}
} // namespace cppunivariate
