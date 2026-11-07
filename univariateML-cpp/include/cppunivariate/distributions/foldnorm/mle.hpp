#pragma once
#include "../../core/work_budget.hpp"
#include "../../core/discrete_stop.hpp"
#include "../../core/span.hpp"
#include "dpqr.hpp"
#include <vector>
#include <limits>
namespace cppunivariate {
enum class FoldnormStatus { Ok, InvalidInput, Unbounded, IterationFailure };
struct FoldnormResult { double mu,sigma,logLik; int iterations; FoldnormStatus status; StopDetail stop_detail{}; };
namespace foldnorm_detail {
// Search every sampled local maximum, including both endpoints. Log spacing
// resolves the weakly identified half-normal face as well as narrow normal fits.
template <typename Function>
inline double maximize(Function f,double lo,double hi,double tol,int maxit,int &iterations) {
  maxit = static_cast<int>(solver_budget(maxit));
  const int grids=512;
  std::vector<double> u(grids+1),v(grids+1);
  double best=lo,bestv=f(lo);
  for (int i=0;i<=grids;++i) {
    u[i]=lo+(hi-lo)*i/grids; v[i]=f(u[i]);
    if (v[i]>bestv) { best=u[i]; bestv=v[i]; }
  }
  for (int i=1;i<grids;++i) if (v[i]>=v[i-1] && v[i]>=v[i+1]) {
    double a=u[i-1],b=u[i+1],c=b-(b-a)*.6180339887498949,d=a+(b-a)*.6180339887498949;
    double fc=f(c),fd=f(d);
    int k=0;
    for (;k<maxit && b-a>tol;++k) {
      if (fc>fd) { b=d; d=c; fd=fc; c=b-(b-a)*.6180339887498949; fc=f(c); }
      else { a=c; c=d; fc=fd; d=a+(b-a)*.6180339887498949; fd=f(d); }
    }
    iterations=std::max(iterations,k);
    if (fc>bestv) {best=c;bestv=fc;} if (fd>bestv) {best=d;bestv=fd;}
  }
  return best;
}
}
inline FoldnormResult foldnorm_mle(span x,double tol=1e-10,int maxit=200,
    double fixed_mu=std::numeric_limits<double>::quiet_NaN(),
    double fixed_sigma=std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  FoldnormResult out{special::kNaN,special::kNaN,special::kNaN,0,FoldnormStatus::InvalidInput};
  const bool fm=!std::isnan(fixed_mu),fs=!std::isnan(fixed_sigma);
  if (x.empty() || !(tol>0) || !std::isfinite(tol) || maxit<=0 ||
      (fm && (!std::isfinite(fixed_mu) || fixed_mu<0)) ||
      (fs && (!std::isfinite(fixed_sigma) || fixed_sigma<=0))) return out;
  double scale=0;bool constant=true;
  for (double z:x) {
    if (!std::isfinite(z) || z<0) return out;
    scale=std::max(scale,z);constant=constant && z==x[0];
  }
  if (!fs && constant && (!fm || x[0]==fixed_mu)) {out.status=FoldnormStatus::Unbounded;return out;}
  scale=std::max(scale,fm ? fixed_mu : 0.0);
  scale=std::max(scale,fs ? fixed_sigma : 0.0);
  if (scale==0) {out.status=FoldnormStatus::Unbounded;return out;}
  std::vector<double> y; double m=0,m2=0;
  for (double z:x) { y.push_back(z/scale); m+=y.back()/x.size(); m2+=y.back()*y.back()/x.size(); }
  double variance=0;
  for (double z:y) variance+=(z-m)*(z-m)/y.size();
  const double mu0=fm ? fixed_mu/scale : 0, s0=fs ? fixed_sigma/scale : 0;
  double residual=0;
  for (double z:y) residual+=(z-mu0)*(z-mu0)/y.size();
  if (!fs && (fm ? residual==0 : variance==0)) {out.status=FoldnormStatus::Unbounded;return out;}
  const auto ll=[&](double mu,double s) { double value=0; for (double z:y) value+=distributions::dfoldnorm(z,mu,s,true); return value; };
  double mu=mu0,s=s0;
  if (fs && !fm) {
    // tanh(c*mu)/mu strictly decreases: at most one positive score root.
    if (m2>s*s) {
      double lo=0,hi=m;
      for (int k=0;k<maxit;++k) {
        const double mid=(lo+hi)*.5; double score=-mid;
        for (double z:y) score+=z*std::tanh(mid*z/(s*s))/y.size();
        if (score>0) lo=mid; else hi=mid;
        out.iterations=k+1;
        if (hi-lo<tol*std::max(hi,1e-10)) break;
      }
      mu=(lo+hi)*.5;
    }
  } else if (!fs && fm) {
    if (mu==0) s=std::sqrt(m2);
    else {
      const double lo=.5*std::log(residual),hi=.5*std::log(m2+mu*mu);
      s=std::exp(foldnorm_detail::maximize([&](double t){return ll(mu,std::exp(t));},lo,hi,tol,maxit,out.iterations));
      // At fixed positive mu, a=mu/sigma^2 has a concave log(a) term
      // plus the same convex correction, so chord bounds apply here too.
      const auto correction=[&](double a) {double value=0;for (double z:y) value+=std::log1p(std::exp(-2*a*z));return value;};
      struct Interval {double lo,hi;};
      std::vector<Interval> pending{{mu/(m2+mu*mu),mu/residual}};
      double best=ll(mu,s);int visited=0;
      while (!pending.empty() && visited<certificate_budget(maxit)) {
        const Interval iv=pending.back();pending.pop_back();++visited;
        if (iv.lo==iv.hi) continue;
        const double al=correction(iv.lo),ar=correction(iv.hi);
        const double slope=(ar-al)/(iv.hi-iv.lo);
        const double candidate=std::max(iv.lo,std::min(iv.hi,y.size()/(y.size()*residual/mu-2*slope)));
        const double candidate_s=std::sqrt(mu/candidate),value=ll(mu,candidate_s);
        if (value>best) {best=value;s=candidate_s;}
        const double weight=(candidate-iv.lo)/(iv.hi-iv.lo);
        const double upper=value+(1-weight)*al+weight*ar-correction(candidate);
        if (upper<=best+tol*y.size()) continue;
        const double mid=std::sqrt(iv.lo)*std::sqrt(iv.hi);
        if (mid<=iv.lo || mid>=iv.hi) continue;
        pending.push_back({iv.lo,mid});pending.push_back({mid,iv.hi});
      }
      if (!pending.empty()) {
        out.status=FoldnormStatus::IterationFailure;
        out.stop_detail=discrete_budget(out.stop_detail);
        const double witness_ll=ll(mu,s)-x.size()*std::log(scale);
        out.stop_detail.candidates.push_back({{mu*scale,s*scale},witness_ll,witness_ll});
        out.stop_detail.loglik_lower=witness_ll;return out;
      }
    }
  } else if (!fs) {
    s=std::sqrt(m2); mu=0;
    // a=mu/sigma^2. Exact maximizer in sigma at fixed a.
    const double amax=m/variance;
    const auto sigma2=[&](double a){return 2*m2/(1+std::hypot(1.0,2*std::sqrt(m2)*a));};
    const auto profile=[&](double t) { const double a=std::expm1(t),v=sigma2(a);return ll(a*v,std::sqrt(v)); };
    double t=foldnorm_detail::maximize(profile,0,std::log1p(amax),tol,maxit,out.iterations);
    double best=profile(t);
    // Certify the grid incumbent to a per-observation likelihood tolerance.
    // A(a)=sum log(1+exp(-2*a*x)) is convex. Replace A by its chord:
    // the remaining profile is strictly concave, with derivative n*(mean-mu(a)).
    // Its maximum under the chord occurs at mu=mean+chord_slope/n.
    const auto correction=[&](double a) {
      double value=0;for (double z:y) value+=std::log1p(std::exp(-2*a*z));return value;
    };
    struct Interval { double lo,hi; int depth; };
    std::vector<Interval> pending{{0,amax,0}};
    int visited=0;
    while (!pending.empty() && visited<certificate_budget(maxit)) {
      const Interval iv=pending.back();pending.pop_back();++visited;
      const double al=correction(iv.lo),ar=correction(iv.hi);
      const double target=std::max(0.0,std::min(m,m+(ar-al)/((iv.hi-iv.lo)*y.size())));
      const double denominator=(m2-target*target);
      const double candidate=std::max(iv.lo,std::min(iv.hi,target/denominator));
      const double value=profile(std::log1p(candidate));
      if (value>best) {best=value;t=std::log1p(candidate);}
      const double weight=(candidate-iv.lo)/(iv.hi-iv.lo);
      const double upper=value+(1-weight)*al+weight*ar-correction(candidate);
      if (upper<=best+tol*y.size()) continue;
      const double mid=std::expm1((std::log1p(iv.lo)+std::log1p(iv.hi))*.5);
      if (mid<=iv.lo || mid>=iv.hi) continue;
      pending.push_back({iv.lo,mid,iv.depth+1});pending.push_back({mid,iv.hi,iv.depth+1});
    }
    if (!pending.empty()) {
        out.status=FoldnormStatus::IterationFailure;
        out.stop_detail=discrete_budget(out.stop_detail);
        const double witness_ll=ll(mu,s)-x.size()*std::log(scale);
        out.stop_detail.candidates.push_back({{mu*scale,s*scale},witness_ll,witness_ll});
        out.stop_detail.loglik_lower=witness_ll;return out;
      }
    const double a=std::expm1(t),v=sigma2(a);
    if (best>ll(0,s)+1e-11*y.size()) { mu=a*v;s=std::sqrt(v); }
  }
  out.mu=fm ? fixed_mu : mu*scale;out.sigma=fs ? fixed_sigma : s*scale;
  out.logLik=ll(mu,s)-x.size()*std::log(scale);
  out.status=out.iterations>=maxit ? FoldnormStatus::IterationFailure : FoldnormStatus::Ok;
  return out;
}
}
