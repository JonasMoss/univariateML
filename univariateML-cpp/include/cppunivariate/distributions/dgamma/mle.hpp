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
  out.shape=std::exp(best_t[0]); out.rate=rate;
  if (!hr && !dgamma_detail::profile_rate(p,out.shape,out.rate,maxit)) { out.status=DgammaStatus::IterationFailure; return out; }
  out.logLik=-p.n*best; out.status=DgammaStatus::Ok; return out;
}
}
