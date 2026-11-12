#pragma once
#include "../../core/work_budget.hpp"
#include "../../core/precision.hpp"
#include "../../core/stop_detail.hpp"
#include "dpqr.hpp"
#include "../../core/span.hpp"
#include <vector>
namespace cppunivariate {
enum class SkellamStatus { Ok, InvalidInput, Impossible, IterationFailure };
struct SkellamResult { double mu1,mu2,logLik; int iterations; SkellamStatus status; StopDetail stop_detail{}; };
inline SkellamResult skellam_accuracy_search(span x,double tol=1e-9,int maxit=150,
    double mu1=special::kNaN,double mu2=special::kNaN) {
  maxit = static_cast<int>(solver_budget(maxit));
  const bool f1=!std::isnan(mu1),f2=!std::isnan(mu2);
  const auto fail=[](SkellamStatus s) {return SkellamResult{special::kNaN,special::kNaN,special::kNaN,0,s};};
  if(x.size()==0 || !(tol>0) || !std::isfinite(tol) || maxit<1 ||
     (f1 && !(std::isfinite(mu1) && mu1>=0)) || (f2 && !(std::isfinite(mu2) && mu2>=0))) return fail(SkellamStatus::InvalidInput);
  CPPUNIV_EXTENDED_REAL sum=0,correction=0;double xmin=INFINITY,xmax=-INFINITY;
  for(double v:x) {if(!std::isfinite(v) || v!=std::floor(v)) return fail(SkellamStatus::InvalidInput);const CPPUNIV_EXTENDED_REAL y=static_cast<CPPUNIV_EXTENDED_REAL>(v)-correction,t=sum+y;correction=(t-sum)-y;sum=t;xmin=std::min(xmin,v);xmax=std::max(xmax,v);}
  const double m=static_cast<double>(sum/x.size());
  const auto ll=[&](double a,double b) {double out=0;for(double v:x) out+=skellam_detail::logmass(v,a,b);return out;};
  if(f1 && f2) {const double l=ll(mu1,mu2);if(!std::isfinite(l)) return fail(SkellamStatus::Impossible);return {mu1,mu2,l,0,SkellamStatus::Ok};}
  if((f1 && mu1==0) || (f2 && mu2==0)) {
    if((f1 && xmax>0) || (f2 && xmin<0)) return fail(SkellamStatus::Impossible);
    const double a=f1?0:m,b=f2?0:-m;
    return {a,b,ll(a,b),0,SkellamStatus::Ok};
  }
  if(!f1 && !f2 && xmin==xmax) {
    const double a=std::max(m,0.0),b=std::max(-m,0.0);
    return {a,b,ll(a,b),0,SkellamStatus::Ok};
  }
  if(f1 || f2) {
    const auto value=[&](double t) {return f1 ? ll(mu1,t) : ll(t,mu2);};
    const auto score=[&](double t) {
      const double a=f1?mu1:t,b=f2?mu2:t,shift=f1?1.0:-1.0;
      double out=0;
      for(double v:x) out+=std::expm1(skellam_detail::logmass(v+shift,a,b)-skellam_detail::logmass(v,a,b));
      return out;
    };
    const double face=value(0);
    // Conditional J has (j+1)*p(j+1)/p(j)=a*b/(j+|k|+1), decreasing.
    // Hence Var(J)<E(J) and d2 log p(k)/d a2=(Var(Y)-E(Y))/a2<0.
    // The likelihood is strictly concave in either single free rate.
    double face_score=0;
    if(std::isfinite(face)) {
      for(double v:x) face_score+=(f1 ? mu1/(v+1) : mu2/(1-v))-1;
      if(face_score<=0) return {f1?mu1:0,f2?mu2:0,face,0,SkellamStatus::Ok};
    }
    double lo=0,hi=std::max(1.0,(f1?mu1:mu2)+std::abs(m)+1);
    int iterations=0;
    while(score(hi)>0 && iterations<maxit) {hi*=2;++iterations;}
    if(!std::isfinite(hi) || iterations==maxit) return fail(SkellamStatus::IterationFailure);
    while(hi-lo>std::min(tol,1e-9)*(1+lo) && iterations<maxit) {
      const double mid=lo+(hi-lo)/2;
      const double g=score(mid);
      if(std::isnan(g)) return fail(SkellamStatus::IterationFailure);
      if(g>0) lo=mid;else hi=mid;
      ++iterations;
    }
    if(iterations==maxit) return fail(SkellamStatus::IterationFailure);
    const double t=lo+(hi-lo)/2,l=value(t);
    if(!std::isfinite(l)) return fail(SkellamStatus::IterationFailure);
    if(std::isfinite(face) && l<=face+1e-8*x.size())
      return {f1?mu1:0,f2?mu2:0,face,iterations,SkellamStatus::Ok};
    return {f1?mu1:t,f2?mu2:t,l,iterations,SkellamStatus::Ok};
  }
  // The theta score at fixed product gives mu1-mu2=sample mean.
  // In the joint fit, t is the smaller rate.
  const auto rates=[&](double t) {return std::vector<double>{f1?mu1:(f2?t:t+std::max(m,0.0)),f2?mu2:(f1?t:t+std::max(-m,0.0))};};
  const auto value=[&](double t) {const auto r=rates(t);return ll(r[0],r[1]);};
  double best=value(0),best_t=0;
  std::int64_t checked=0;
  const auto budget_stop=[&]() {
      auto out=fail(SkellamStatus::IterationFailure);
      out.stop_detail.budget_exhausted=true;
      out.stop_detail.loglik_upper=INFINITY;
      if(std::isfinite(best)) {
        const auto r=rates(best_t);
        const double pad=64*std::numeric_limits<double>::epsilon()*x.size()*(1+std::abs(best));
        out.stop_detail.loglik_lower=best-pad;
        out.stop_detail.candidates.push_back({r,best-pad,best+pad,StopModel::None});
      }
      return out;
  };
  // Search all nominated local maxima on a logarithmic grid, comparing the
  // attained zero-rate face exactly. No uniqueness assumption is used.
  constexpr double upper=709;
  std::vector<double> grid,values;
  bool certified=false;
  for(double u=-32;u<=upper;u+=0.5) {
    if(++checked>certificate_budget(maxit))return budget_stop();
    const double t=std::exp(u),v=value(t);
    grid.push_back(u);values.push_back(v);
    if(v>best) {best=v;best_t=t;}
    // A convolution mass cannot exceed the largest mass of its Poisson
    // factor of rate >= t. Stirling bounds that mode by
    // 1/sqrt(2*pi*floor(t)). This certifies exclusion of the far tail.
    const double candidate=std::max(best,*std::max_element(values.begin(),values.end()));
    if(t>=2 && -0.5*x.size()*(1.8378770664093454836+std::log(std::floor(t)))<candidate-1e-8*x.size()) {certified=true;break;}
  }
  if(!certified) return fail(SkellamStatus::IterationFailure);
  int iterations=0;
  for(std::size_t i=1;i+1<grid.size();++i) {
    if(!(values[i]>=values[i-1] && values[i]>=values[i+1])) continue;
    double lo=grid[i-1],hi=grid[i+1];
    constexpr double c=0.6180339887498948482;
    double a=hi-c*(hi-lo),b=lo+c*(hi-lo),fa=value(std::exp(a)),fb=value(std::exp(b));
    int it=0;
    for(;it<maxit && hi-lo>std::min(tol,1e-8);++it) {
      if(fa>fb) {hi=b;b=a;fb=fa;a=hi-c*(hi-lo);fa=value(std::exp(a));}
      else {lo=a;a=b;fa=fb;b=lo+c*(hi-lo);fb=value(std::exp(b));}
    }
    iterations=std::max(iterations,it);
    if(it==maxit) return fail(SkellamStatus::IterationFailure);
    const double t=std::exp(fa>fb?a:b),l=std::max(fa,fb);
    if(l>best) {best=l;best_t=t;}
  }
  // Certify the entire finite interval, not just the grid nominations.
  // The profile's second derivative is at most 2 sum 1/(|x_i|+1).
  // See the dossier for the conditional-count argument and error margin.
  double curvature=0;
  for(double v:x) curvature+=2/(std::abs(v)+1);
  // Half the total budget is reserved for a possible face tie.
  const double margin=0.5e-8*x.size();
  const double first=std::exp(grid.front()),last=std::exp(grid.back());
  const double low=value(first)+2*x.size()*first;
  if(!std::isfinite(low) || low>best+margin) return fail(SkellamStatus::IterationFailure);
  struct Interval { double lo,hi; };
  std::vector<Interval> pending{{first,last}};
  while(!pending.empty()) {
    if(++checked>certificate_budget(maxit))return budget_stop();
    const Interval interval=pending.back();pending.pop_back();
    const double mid=interval.lo+(interval.hi-interval.lo)/2;
    if(!(mid>interval.lo && mid<interval.hi)) return fail(SkellamStatus::IterationFailure);
    const auto r=rates(mid);
    double l=0,g=0;
    for(double v:x) {
      if(v-1==v || v+1==v) return fail(SkellamStatus::IterationFailure);
      const double p=skellam_detail::logmass(v,r[0],r[1]);
      const double pminus=skellam_detail::logmass(v-1,r[0],r[1]);
      const double pplus=skellam_detail::logmass(v+1,r[0],r[1]);
      l+=p;g+=std::expm1(pminus-p)+std::expm1(pplus-p);
    }
    if(!std::isfinite(l) || !std::isfinite(g)) return fail(SkellamStatus::IterationFailure);
    if(l>best) {best=l;best_t=mid;}
    const double h=(interval.hi-interval.lo)/2;
    const double bound=l+std::abs(g)*h+0.5*curvature*h*h;
    if(!std::isfinite(bound)) return fail(SkellamStatus::IterationFailure);
    if(bound>best+margin) {
      pending.push_back({interval.lo,mid});
      pending.push_back({mid,interval.hi});
    }
  }
  const double face=value(0);
  if(std::isfinite(face) && best<=face+margin) {best=face;best_t=0;}
  if(!std::isfinite(best)) return fail(SkellamStatus::Impossible);
  const auto r=rates(best_t);
  return {r[0],r[1],best,iterations,SkellamStatus::Ok};
}
}

#include "accuracy.hpp"
namespace cppunivariate {
inline SkellamResult skellam_mle(span x,double tol=1e-10,int maxit=150,double mu1=NAN,double mu2=NAN) {
  auto r=skellam_accuracy_search(x,tol,maxit,mu1,mu2);
  if(r.status!=SkellamStatus::Ok||r.mu1==0||r.mu2==0)return r;
  std::array<double,3> q{{std::log(r.mu1),std::log(r.mu2),0}};
  const std::array<bool,3> free{{std::isnan(mu1),std::isnan(mu2),false}};
  const auto objective=[&](const std::array<accuracy_detail::Jet,3>& z) {
    const auto a=accuracy_detail::exp(z[0]),b=accuracy_detail::exp(z[1]);accuracy_detail::JetSum f;
    for(double v:x)f.add(skellam_detail::differentiable_mass(v,a,b,maxit));
    return f.value;
  };
  if(!accuracy_detail::polish(q,free,tol,maxit,objective,r.stop_detail)){r.status=SkellamStatus::IterationFailure;return r;}
  if(free[0])r.mu1=std::exp(q[0]);
  if(free[1])r.mu2=std::exp(q[1]);
  r.logLik=0;for(double v:x)r.logLik+=skellam_detail::logmass(v,r.mu1,r.mu2);
  return r;
}
}
