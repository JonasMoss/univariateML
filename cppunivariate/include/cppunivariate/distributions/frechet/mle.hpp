#pragma once
#include "../gev/mle.hpp"
#include "../invweibull/mle.hpp"
namespace cppunivariate {
inline GevResult frechet_gev_mle(span x, double tol=1e-10, int maxit=100,
                         double xi=std::numeric_limits<double>::quiet_NaN()) {
  GevResult bad{0,0,xi,0,0,GevStatus::InvalidData};
  if(x.size()<3 || !(tol>0) || !std::isfinite(tol) || maxit<1) return bad;
  if(!std::isnan(xi) && (!std::isfinite(xi) || xi<-.5 || xi>1)) {
    bad.status=GevStatus::InvalidShape;return bad;
  }
  std::vector<double> y(x.begin(),x.end());
  for(double v:y) if(!std::isfinite(v)) return bad;
  std::sort(y.begin(),y.end());
  std::size_t run=1;
  for(std::size_t i=1;i<y.size();++i) {
    run=y[i]==y[i-1] ? run+1 : 1;
    if(2*run>=y.size()) return bad;
  }
  // Standardization prevents scale and translation from setting tolerances.
  const double origin=y[0],range=y.back()-origin;
  if(!(range>0) || !std::isfinite(range)) return bad;
  for(double &v:y) v=(v-origin)/range;
  const span data{y.data(),y.size()};
  auto restore=[&](GevResult r) {
    r.mu=origin+range*r.mu;r.sigma*=range;
    r.logLik-=y.size()*std::log(range);return r;
  };
  if(!std::isnan(xi)) return restore(gev_profile(data,xi,tol,maxit).fit);
  auto prev=gev_profile(data,0,tol,maxit);
  const auto gumbel=prev.fit;
  auto best=prev.fit;
  bool failed=best.status!=GevStatus::Ok;
  auto retain=[&](const GevProfile &p) {
    failed=failed || p.fit.status!=GevStatus::Ok;
    if(p.fit.logLik>best.logLik) best=p.fit;
  };
  auto root=[&](GevProfile left,GevProfile right) {
    double l=left.fit.xi,h=right.fit.xi;
    double mid=(l*right.score-h*left.score)/(right.score-left.score);
    bool done=false;
    for(int k=0;k<maxit;++k) {
      if(!std::isfinite(mid) || mid<=l || mid>=h) mid=(l+h)/2;
      const auto candidate=gev_profile(data,mid,tol,maxit);
      retain(candidate);
      if(std::abs(candidate.score)<tol*y.size() || h-l<tol) {
        // Prefer the score-converged point over rounding ties in logLik.
        if(candidate.fit.logLik>=best.logLik-1e-12*y.size()) best=candidate.fit;
        done=true;break;
      }
      if(candidate.score>0) l=mid; else h=mid;
      mid-=candidate.score/candidate.curvature;
    }
    failed=failed || !done;
  };
  // Enumerate profile-score sign changes, retaining both attained bounds.
  // Curvature turns also expose pairs of roots within a single mesh cell.
  // This is root discovery, not an assumption of global concavity.
  const int cells=32;
  for(int j=1;j<=cells;++j) {
    const auto next=gev_profile(data,1.0*j/cells,tol,maxit);
    retain(next);
    if(prev.score>0 && next.score<0) root(prev,next);
    else if((prev.score>0)==(next.score>0) &&
            ((prev.score>0 && prev.curvature<0 && next.curvature>0) ||
             (prev.score<0 && prev.curvature>0 && next.curvature<0))) {
      auto l=prev,h=next;
      for(int k=0;k<maxit;++k) {
        const auto mid=gev_profile(data,(l.fit.xi+h.fit.xi)/2,tol,maxit);
        retain(mid);
        if((mid.score>0)!=(prev.score>0)) {
          if(prev.score>0) root(prev,mid); else root(mid,next);
          break;
        }
        if(h.fit.xi-l.fit.xi<tol) break;
        if((mid.curvature>0)==(l.curvature>0)) l=mid; else h=mid;
      }
    }
    prev=next;
  }
  if(best.logLik <= gumbel.logLik+1e-8*y.size()) best=gumbel;
  if(failed) best.status=GevStatus::IterationFailure;
  return restore(best);
}

enum class FrechetStatus { Ok, InvalidData, GumbelLimit, IterationFailure };
struct FrechetResult { double lambda, mu, sigma, logLik; int iterations; FrechetStatus status; };
inline FrechetResult frechet_mle(span x, double tol=1e-10, int maxit=100,
 double mu=std::numeric_limits<double>::quiet_NaN()) {
 const double nan=std::numeric_limits<double>::quiet_NaN();
 FrechetResult bad{nan,mu,nan,nan,0,FrechetStatus::InvalidData};
 if(x.size()<2 || !(tol>0) || !std::isfinite(tol) || maxit<1) return bad;
 if(!std::isnan(mu)) {
   if(!std::isfinite(mu)) return bad;
   std::vector<double> y(x.size());
   for(std::size_t i=0;i<x.size();++i) {
     y[i]=x[i]-mu;
     if(!(y[i]>0) || !std::isfinite(y[i])) return bad;
   }
   const auto r=invweibull_mle(span{y.data(),y.size()},tol,maxit);
   if(!(r.shape>0) || !(r.rate>0) || !std::isfinite(r.shape) || !std::isfinite(1/r.rate) || !std::isfinite(r.logLik)) return bad;
   return {r.shape,mu,1/r.rate,r.logLik,r.iterations,r.iterations>maxit ? FrechetStatus::IterationFailure : FrechetStatus::Ok};
 }
 const auto r=frechet_gev_mle(x,tol,maxit);
 if(r.status==GevStatus::InvalidData) return bad;
 if(r.status!=GevStatus::Ok) {bad.status=FrechetStatus::IterationFailure;return bad;}
 if(r.xi==0) {bad.status=FrechetStatus::GumbelLimit;return bad;}
 return {1/r.xi,r.mu-r.sigma/r.xi,r.sigma/r.xi,r.logLik,r.iterations,FrechetStatus::Ok};
}
}
