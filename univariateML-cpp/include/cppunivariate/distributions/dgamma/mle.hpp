#pragma once
#include "../../core/span.hpp"
#include "../dnorm/mle.hpp"
#include "dpqr.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>
namespace cppunivariate {
enum class DgammaStatus { Ok, InvalidData, InvalidControl, PointMass, TwoPointLimit, IterationFailure };
struct DgammaResult { double shape, rate, logLik; int iterations; DgammaStatus status; };
namespace dgamma_detail {
// With shape held, log-rate is a location coordinate for log(Y).
// Its grouped likelihood is strictly concave (log-gamma density is
// log-concave). Solve that exact profile score, not a plug-in moment rate.
inline double rate_score(const dnorm_detail::Cells &p,double shape,double logr,double &derivative) {
  const double rate=std::exp(logr); double g=0; derivative=0;
  for (std::size_t j=0;j<p.x.size();++j) {
    const double a=p.x[j]*rate,b=(p.x[j]+1)*rate;
    const double lp=distributions::ddgamma(p.x[j],shape,rate,true);
    if (!std::isfinite(lp)) return std::numeric_limits<double>::quiet_NaN();
    const double u=std::exp(shape*std::log(b)-b-special::lgamma(shape)-lp);
    const double v=a==0 ? 0:std::exp(shape*std::log(a)-a-special::lgamma(shape)-lp);
    const double z=u-v;
    g+=p.w[j]*z/p.n;
    derivative+=p.w[j]*((shape-b)*u-(shape-a)*v-z*z)/p.n;
  }
  return g;
}
inline bool profile_rate(const dnorm_detail::Cells &p,double shape,double &rate,int maxit=100) {
  double u=std::log(shape)-std::log(p.centre),derivative=0;
  double g=rate_score(p,shape,u,derivative);
  if (!std::isfinite(g)) return false;
  double lo=u,hi=u; const bool up=g>0;
  for (double step=1;step<=512;step*=2) {
    const double v=u+(up ? step:-step);
    double dg=0; const double gv=rate_score(p,shape,v,dg);
    if (!std::isfinite(gv)) return false;
    if (up) hi=v; else lo=v;
    if ((gv>0)!=up || gv==0) break;
    if (step==512) return false;
  }
  u=(lo+hi)/2;
  for (int it=0;it<maxit;++it) {
    g=rate_score(p,shape,u,derivative);
    if (!std::isfinite(g)) return false;
    if (std::abs(g)<1e-11*std::min(1.0,shape) || hi-lo<1e-12*(1+std::abs(u))) { rate=std::exp(u); return true; }
    if (g>0) lo=u; else hi=u;
    const double next=u-g/derivative;
    u=derivative<0 && next>lo && next<hi ? next:(lo+hi)/2;
  }
  return false;
}
struct Cost {
  const dnorm_detail::Cells &p;
  bool hr; double r; int maxit;
  double operator()(const std::vector<double> &t) const {
    const double shape=std::exp(t[0]); double rate=r;
    if (!(shape>0) || !std::isfinite(shape) || (!hr && !profile_rate(p,shape,rate,maxit)))
      return std::numeric_limits<double>::infinity();
    double f=0;
    for (std::size_t j=0;j<p.x.size();++j) f-=p.w[j]*distributions::ddgamma(p.x[j],shape,rate,true)/p.n;
    return f;
  }
};
// Deterministic Nelder--Mead with vector storage, avoiding pointer interfaces.
// The outer logarithmic grid supplies separated starts; no unimodality is assumed.
inline bool search(const Cost &cost,std::vector<double> &t,double tol,int maxit,int &iterations) {
  const int d=static_cast<int>(t.size());
  std::vector<std::vector<double>> v(d+1,t); std::vector<double> f(d+1);
  for (int j=1;j<=d;++j) v[j][j-1]+=0.4;
  for (int j=0;j<=d;++j) f[j]=cost(v[j]);
  for (int it=0;it<maxit;++it) {
    ++iterations;
    for (int i=0;i<=d;++i) for (int j=i+1;j<=d;++j) if (f[j]<f[i]) { std::swap(f[i],f[j]); std::swap(v[i],v[j]); }
    double width=0; for (int j=1;j<=d;++j) for (int k=0;k<d;++k) width=std::max(width,std::abs(v[j][k]-v[0][k]));
    if (width<tol) { t=v[0]; return std::isfinite(f[0]); }
    std::vector<double> c(d,0),z(d),e(d);
    for (int j=0;j<d;++j) for (int k=0;k<d;++k) c[k]+=v[j][k]/d;
    for (int k=0;k<d;++k) z[k]=2*c[k]-v[d][k];
    double fz=cost(z);
    if (fz<f[0]) {
      for (int k=0;k<d;++k) e[k]=c[k]+2*(z[k]-c[k]);
      const double fe=cost(e); v[d]=fe<fz ? e:z; f[d]=std::min(fe,fz);
    } else if (fz<f[d-1]) { v[d]=z; f[d]=fz; }
    else {
      const bool outside=fz<f[d];
      for (int k=0;k<d;++k) e[k]=c[k]+0.5*((outside ? z[k]:v[d][k])-c[k]);
      const double fe=cost(e);
      if (fe<(outside ? fz:f[d])) { v[d]=e; f[d]=fe; }
      else for (int j=1;j<=d;++j) { for (int k=0;k<d;++k) v[j][k]=(v[j][k]+v[0][k])/2; f[j]=cost(v[j]); }
    }
  }
  t=v[0]; return false;
}

// Bounds here are conditional on the computed floating-point comparisons.
// The derivation and the likelihood allowance are in the family dossier.
inline bool enclosed_rate(const dnorm_detail::Cells &p,double a,double &r,
                          double &upper,int maxit) {
  const double mean=p.centre-0.5;
  if (!(mean>0)) return false;
  double lo=std::log(a)-std::log(mean+1),hi=std::log(a)-std::log(mean);
  double u=(lo+hi)/2,du=0,g=0;
  for(int i=0;i<maxit;++i) {
    g=rate_score(p,a,u,du);
    if(!std::isfinite(g)||!std::isfinite(du)) return false;
    if(g>0) lo=u; else hi=u;
    upper=std::max(g*(lo-u),g*(hi-u));
    if(upper<1e-11) {r=std::exp(u); return std::isfinite(r)&&r>0;}
    const double next=u-g/du;
    u=du<0 && next>lo && next<hi ? next:(lo+hi)/2;
  }
  return false;
}

inline double shape_integral(const dnorm_detail::Cells &p,double a,double r,double &magnitude) {
  const double lg=special::lgamma(a),lr=a*std::log(r);
  double v=lg-lr;
  magnitude=std::abs(lg)+std::abs(lr);
  for(std::size_t j=0;j<p.x.size();++j) {
    const double term=p.w[j]*distributions::ddgamma(p.x[j],a,r,true)/p.n;
    v+=term; magnitude+=std::abs(term);
  }
  return v;
}

inline bool shape_score_bounds(const dnorm_detail::Cells &p,double A,double B,
                               bool held,double held_r,double &lower,double &upper) {
  const double mean=p.centre-0.5;
  double rlo=held_r,rhi=held_r;
  if(!held) {
    if(!(mean>0)) return false;
    double lo=std::log(A)-std::log(mean+1),hi=std::log(B)-std::log(mean);
    for(int i=0;i<40;++i) {
      const double mid=(lo+hi)/2; double d1=0,d2=0;
      const double ga=rate_score(p,A,mid,d1),gb=rate_score(p,B,mid,d2);
      if(!std::isfinite(ga)||!std::isfinite(gb)) return false;
      if(gb-(B-A)>0) lo=mid;
      else if(ga+(B-A)<0) hi=mid;
      else break;
    }
    rlo=std::exp(lo);rhi=std::exp(hi);
  }
  if(!(rlo>0 && rhi>=rlo) || !std::isfinite(rhi)) return false;
  const double h=1e-4*A;
  if (!(h>0 && A-h>0 && A-h<A && B+h>B)) return false;
  double lm1=0,lm0=0,um1=0,um0=0;
  const double l1=shape_integral(p,A,rhi,lm1),l0=shape_integral(p,A-h,rhi,lm0);
  const double u1=shape_integral(p,B+h,rlo,um1),u0=shape_integral(p,B,rlo,um0);
  const double roundoff=64*std::numeric_limits<double>::epsilon()/h;
  const double sl=std::log(rlo)-special::digamma(B)+(l1-l0)/h-roundoff*(1+lm1+lm0);
  const double su=std::log(rhi)-special::digamma(A)+(u1-u0)/h+roundoff*(1+um1+um0);
  if(!std::isfinite(sl)||!std::isfinite(su)||sl>su) return false;
  lower=(sl<0 ? B:A)*sl;
  upper=(su>0 ? B:A)*su;
  return std::isfinite(lower)&&std::isfinite(upper);
}

inline bool certify(const dnorm_detail::Cells &p,bool held,double held_r,
                    double &best,double &best_t,int maxit) {
  const double L=-p.n*best;
  double low=0;
  for(std::size_t j=0;j<p.x.size();++j) if(p.x[j]>0) {
    const double a=std::exp(L/p.w[j])/(2*std::exp(1.0)*std::log1p(1/p.x[j]));
    low=std::max(low,std::min(1.0,a));
  }
  double high=std::numeric_limits<double>::infinity();
  if(held) high=std::max(2*held_r*(p.x.back()+1),-L/(p.n*(std::log(2.0)-0.5)));
  else for(std::size_t i=0;i<p.x.size();++i) for(std::size_t j=i+1;j<p.x.size();++j) {
    if(!(p.x[j]>p.x[i]+1)) continue;
    const double logq=0.5*std::log(p.x[j]/(p.x[i]+1));
    const double c=std::min(std::expm1(logq)-logq,std::expm1(-logq)+logq);
    if(c>0) high=std::min(high,-L/(c*std::min(p.w[i],p.w[j])));
  }
  if(!(low>0 && high>low) || !std::isfinite(high)) return false;
  const double tlo=std::log(low),thi=std::log(high);
  if(!(best_t>=tlo && best_t<=thi)) return false;
  struct Interval {double lo,hi;};
  std::vector<Interval> pending;
  const int pieces=static_cast<int>(std::ceil((thi-tlo)/0.5));
  if(pieces<1 || pieces>10000) return false;
  for(int i=0;i<pieces;++i) pending.push_back({tlo+(thi-tlo)*i/pieces,tlo+(thi-tlo)*(i+1)/pieces});
  long long count=0; const long long budget=1000LL*maxit;
  while(!pending.empty()) {
    if(++count>budget) return false;
    const Interval q=pending.back();pending.pop_back();
    const double t=(q.lo+q.hi)/2,A=std::exp(q.lo),B=std::exp(q.hi),a=std::exp(t);
    double r=held_r,error=0;
    if(!held && !enclosed_rate(p,a,r,error,maxit)) return false;
    double ll=0;
    for(std::size_t j=0;j<p.x.size();++j)
      ll+=p.w[j]*distributions::ddgamma(p.x[j],a,r,true)/p.n;
    if(!std::isfinite(ll)) return false;
    if(-ll<best) {best=-ll;best_t=t;}
    double lower=0,upper=0;
    if(!shape_score_bounds(p,A,B,held,held_r,lower,upper)) return false;
    const double bound=ll+error+std::max(0.0,std::max(-lower*(t-q.lo),upper*(q.hi-t)));
    if(!std::isfinite(bound)) return false;
    if(bound>-best+5e-9) {
      if(!(t>q.lo && t<q.hi)) return false;
      pending.push_back({q.lo,t});pending.push_back({t,q.hi});
    }
  }
  return true;
}
}
inline DgammaResult dgamma_mle(span x,double tol=1e-8,int maxit=400,
    double shape=std::numeric_limits<double>::quiet_NaN(),double rate=std::numeric_limits<double>::quiet_NaN()) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  DgammaResult out{nan,nan,nan,0,DgammaStatus::InvalidData};
  const bool ha=!std::isnan(shape),hr=!std::isnan(rate);
  if (!(tol>0) || !std::isfinite(tol) || maxit<1 || (ha && (!(shape>0) || !std::isfinite(shape))) || (hr && (!(rate>0) || !std::isfinite(rate)))) { out.status=DgammaStatus::InvalidControl; return out; }
  dnorm_detail::Cells p; if (!p.init(x) || p.x.front()<0) return out;
  if (!ha && !hr && p.x.back()-p.x.front()<2) { out.status=p.x.size()==1 ? DgammaStatus::PointMass:DgammaStatus::TwoPointLimit; return out; }
  if ((!ha || !hr) && p.x.back()==0) { out.status=DgammaStatus::PointMass; return out; }
  if (ha && hr) {
    double ll=0; for (std::size_t j=0;j<p.x.size();++j) ll+=p.w[j]*distributions::ddgamma(p.x[j],shape,rate,true);
    return {shape,rate,ll,0,DgammaStatus::Ok};
  }
  if (ha) {
    double r=0;
    if (!dgamma_detail::profile_rate(p,shape,r,maxit)) { out.status=DgammaStatus::IterationFailure; return out; }
    out.shape=shape; out.rate=r; out.logLik=0;
    for (std::size_t j=0;j<p.x.size();++j) out.logLik+=p.w[j]*distributions::ddgamma(p.x[j],shape,r,true);
    out.iterations=1; out.status=DgammaStatus::Ok; return out;
  }
  const double mean=p.centre,var=std::max(0.1,p.variance);
  const double a0=hr ? mean*rate:mean*mean/var;
  dgamma_detail::Cost cost{p,hr,rate,maxit};
  // Global scan of the one-dimensional exact profile, followed by local
  // refinement of every grid-local maximum. Extend either improving edge.
  std::vector<double> grid,values;
  for (int i=-20;i<=20;++i) {
    const double t=std::log(a0)+i;
    grid.push_back(t); values.push_back(cost(std::vector<double>{t}));
  }
  for (int i=0;i<32 && values.front()<values[1];++i) {
    grid.insert(grid.begin(),grid.front()-1);
    values.insert(values.begin(),cost(std::vector<double>{grid.front()}));
  }
  for (int i=0;i<32 && values.back()<values[values.size()-2];++i) {
    grid.push_back(grid.back()+1); values.push_back(cost(std::vector<double>{grid.back()}));
  }
  double best=std::numeric_limits<double>::infinity(); std::vector<double> best_t;
  for (std::size_t j=1;j+1<grid.size();++j) {
    if (!(values[j]<=values[j-1] && values[j]<=values[j+1]) || !std::isfinite(values[j])) continue;
    std::vector<double> t{grid[j]};
    const bool ok=dgamma_detail::search(cost,t,tol,maxit,out.iterations);
    const double f=cost(t);
    if (ok && f<best) { best=f; best_t=t; }
  }
  if (best_t.empty()) { out.status=DgammaStatus::IterationFailure; return out; }
  if (!dgamma_detail::certify(p,hr,rate,best,best_t[0],maxit)) { out.status=DgammaStatus::IterationFailure; return out; }
  out.shape=std::exp(best_t[0]); out.rate=rate;
  if (!hr && !dgamma_detail::profile_rate(p,out.shape,out.rate,maxit)) { out.status=DgammaStatus::IterationFailure; return out; }
  out.logLik=0;
  for (std::size_t j=0;j<p.x.size();++j)
    out.logLik+=p.w[j]*distributions::ddgamma(p.x[j],out.shape,out.rate,true);
  if (!std::isfinite(out.logLik) || out.logLik/p.n < -best-5e-9) {
    out.status=DgammaStatus::IterationFailure; return out;
  }
  out.status=DgammaStatus::Ok; return out;
}
}
