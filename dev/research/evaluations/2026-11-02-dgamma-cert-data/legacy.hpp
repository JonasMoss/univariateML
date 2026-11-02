#pragma once
#include "cppunivariate/distributions/dgamma/mle.hpp"
namespace cppunivariate {
inline DgammaResult dgamma_legacy_mle(span x,double tol=1e-8,int maxit=400,
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
