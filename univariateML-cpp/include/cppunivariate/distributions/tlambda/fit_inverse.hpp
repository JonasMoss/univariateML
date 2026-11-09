#pragma once
#include "dpqr.hpp"
#include "../../core/work_budget.hpp"
#include <array>
namespace cppunivariate {
namespace tlambda_fit_detail {
struct Root { double logp=NAN,logd=NAN; bool ok=false; };
struct Equation {double f,slope,curvature,logd;};
// All derivatives are with respect to s=log(p). Scaled ratios avoid powers
// of subnormal p and overflow of D. The centre uses h itself, not log(h).
inline Equation equation(double s,double l,double az,double ld) {
  const double p=std::exp(s),qlog=std::log1p(-p);
  if(s>-340 && std::abs(l*s)<600 && std::abs(l)>1e-7) {
    const double q=1-p,P=std::exp(l*s),R=std::exp(l*qlog);
    const double pd=P+p*R/q;
    const double ppd=(l-1)*(P-p*p*R/(q*q));
    const double dlog=std::log(pd)-s;
    if(l>0 && ld<-.6931471805599453) {
      const double deficit=P-std::expm1(l*qlog),g=l*pd/deficit;
      return {std::log(deficit)-ld,g,l*(pd+ppd)/deficit-g*g,dlog};
    }
    const double h=P*std::expm1(l*(qlog-s))/l;
    if(az>1){const double g=-pd/h;return {std::log(h)-std::log(az),g,(-pd-ppd)/h-g*g,dlog};}
    return {h-az,-pd,-pd-ppd,dlog};
  }
  const double d=tlambda_detail::logderiv_from_logs(s,qlog,l);
  const double ratio=(l-1)*(std::exp((l-1)*s-d)-std::exp(s+(l-2)*qlog-d));
  const bool deficit=l>0 && ld<-.6931471805599453;
  double f,g;
  if(deficit) {
    const double v=tlambda_detail::logdeficit(s,l);
    f=v-ld;g=std::exp(std::log(l)+s+d-v);
    return {f,g,g*(1+ratio-g),d};
  }
  const double hlog=tlambda_detail::logmag_from_logs(s,qlog,l);
  if(az>1) {
    f=hlog-std::log(az);g=-std::exp(s+d-hlog);
    return {f,g,g*(1+ratio-g),d};
  }
  f=std::exp(hlog)-az;g=-std::exp(s+d);
  return {f,g,g*(1+ratio),d};
}
inline Root solve(double az,double l,double ld,double seed,int maxit) {
  const double log2=std::log(2.0),eps=std::numeric_limits<double>::epsilon();
  if(az==0)return {-log2,(2-l)*log2,true};
  if(!(az>0) || !std::isfinite(az) || !std::isfinite(l))return {};
  if(l==0) {const double s=-az-std::log1p(std::exp(-az));return {s,tlambda_detail::logderiv(s,l),true};}
  if(l==-1) {
    const double t=std::log(az);
    const double s=log2-tlambda_detail::logadd(tlambda_detail::logadd(t,log2),.5*tlambda_detail::logadd(2*t,std::log(4.0)));
    return {s,tlambda_detail::logderiv(s,l),true};
  }
  if(l>0 && (!(ld<=0) || (ld==0 && l*az>.5)))return {};
  if(l==1 || l==2) {const double s=ld-log2;return {s,tlambda_detail::logderiv(s,l),true};}
  double hi=-log2,lo;
  if(l<0)lo=-tlambda_detail::logadd(std::log(-l)+std::log(az),-l*log2)/(-l);
  else lo=-(l<1?(log2-ld)/l:std::log1p(l)-ld);
  if(!std::isfinite(lo))return {};
  lo-=64*eps*(1+std::abs(lo));
  double s=std::max(lo,std::min(hi,seed));
  const bool increasing=l>0 && ld<-log2;
  for(std::int64_t k=0;k<solver_budget(maxit);++k) {
    const auto e=equation(s,l,az,ld);
    if(!std::isfinite(e.f) || !std::isfinite(e.slope) || e.slope==0) {
      s=lo+(hi-lo)/2;continue;
    }
    // A residual/derivative check verifies the new iterate, rather than
    // accepting an earlier small Halley step. Conditional on rounded
    // comparisons, the practical root margin is 32 epsilon in log(p).
    const double error=std::abs(e.f/e.slope);
    if(error<=32*eps*(1+std::abs(s))) {
      const double radius=64*eps*(1+std::abs(s));
      const double left=std::max(lo,s-radius),right=std::min(hi,s+radius);
      const auto el=equation(left,l,az,ld),er=equation(right,l,az,ld);
      if(increasing ? (el.f<=0 && er.f>=0) : (el.f>=0 && er.f<=0))return {s,e.logd,true};
    }
    if((e.f>0)==increasing)hi=s;else lo=s;
    double next=s-2*e.f*e.slope/(2*e.slope*e.slope-e.f*e.curvature);
    if(!(next>lo && next<hi) || !std::isfinite(next))next=lo+(hi-lo)/2;
    if(next==s)return {};
    s=next;
  }
  return {};
}
struct SeedTable {
  std::array<double,33*65> r{};
  SeedTable() {
    for(int i=0;i<33;++i)for(int j=0;j<65;++j){
      const double l=-1+i/16.,t=j/64.;
      if(j==64){r[i*65+j]=l==1?-std::log(2.0):0;continue;}
      const double eta=-4*std::log1p(-t);
      const double az=l==0?eta:-std::expm1(-l*eta)/l;
      const auto root=solve(az,l,l>0?-l*eta:NAN,-eta-std::log(2.0)*std::exp(-eta),100000);
      r[i*65+j]=root.ok?root.logp+eta:-std::log(2.0)*std::exp(-eta);
    }
  }
};
inline std::array<double,4> weights(double x,int& k,int n) {
  k=std::max(0,std::min(n-4,static_cast<int>(std::floor(x))-1));
  const double u=x-k;
  return {-(u-1)*(u-2)*(u-3)/6,u*(u-2)*(u-3)/2,-u*(u-1)*(u-3)/2,u*(u-1)*(u-2)/6};
}
inline Root inverse(double az,double l,double logslack=NAN,int maxit=1000) {
  if(l>0 && std::isnan(logslack))logslack=std::log(std::fma(-l,az,1.0));
  if(az==0 || l==0 || l==-1 || l==1 || l==2)return solve(az,l,logslack,NAN,maxit);
  const double eta=l==0?az:l>0?-logslack/l:tlambda_detail::logadd(0,std::log(-l)+std::log(az))/(-l);
  double seed=-eta-std::log(2.0)*std::exp(-eta);
  if(l>=-1 && l<=1 && std::isfinite(eta) && l!=0 && l!=-1) {
    static const SeedTable table;
    int a,b;const auto wa=weights((l+1)*16,a,33),wb=weights(-std::expm1(-eta/4)*64,b,65);
    double r=0;
    for(int i=0;i<4;++i)for(int j=0;j<4;++j)r+=wa[i]*wb[j]*table.r[(a+i)*65+b+j];
    seed=-eta+r;
  }
  return solve(az,l,logslack,seed,maxit);
}
inline double logdensity(double x,double mu,double sigma,double l,int maxit=1000) {
  if(!tlambda_detail::valid(mu,sigma,l))return NAN;
  const double gap=l>0?tlambda_detail::slack(x,mu,sigma,l):1;
  if(l>0 && gap<=0)return gap<0 || l<1?-INFINITY:-std::log(sigma)-(l==1?std::log(2.0):0);
  const auto r=inverse(std::exp(tlambda_detail::logstandard(x,mu,sigma)),l,l>0?std::log(gap)-std::log(sigma):NAN,maxit);
  return r.ok?-std::log(sigma)-r.logd:NAN;
}
}
}
