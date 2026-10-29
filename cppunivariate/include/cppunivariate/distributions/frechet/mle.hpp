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
  std::size_t minimum_count=1;
  while(minimum_count<y.size() && y[minimum_count]==y[0]) ++minimum_count;
  // At positive xi the lower-endpoint collapse is controlled only by
  // the multiplicity of the minimum. Equality is an unattained face.
  if(std::isnan(xi) ? 2*minimum_count>=y.size() :
      (xi>0 && xi*static_cast<double>(minimum_count)>=y.size()-minimum_count)) return bad;
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
inline double frechet_loglik(span x,double lambda,double mu,double sigma) {
 if(!(lambda>0) || !(sigma>0)) return -std::numeric_limits<double>::infinity();
 double ll=x.size()*(std::log(lambda)-std::log(sigma));
 for(double v:x) {
   if(!(v>mu)) return -std::numeric_limits<double>::infinity();
   const double z=std::log(v-mu)-std::log(sigma);
   ll-=(lambda+1)*z+std::exp(-lambda*z);
 }
 return ll;
}
// Strictly concave shape likelihood with held location and scale.
inline FrechetResult frechet_shape(span x,double mu,double sigma,double floor,
                                   double tol,int maxit) {
 bool constant_at_scale=true;
 for(double v:x) constant_at_scale=constant_at_scale && v-mu==sigma;
 if(constant_at_scale) return {0,mu,sigma,0,0,FrechetStatus::InvalidData};
 auto score=[&](double a) {
   double v=static_cast<double>(x.size())/a;
   for(double y:x) {
     const double z=std::log(y-mu)-std::log(sigma);
     if(z!=0) v+=z*std::expm1(-a*z);
   }
   return v;
 };
 double lo=floor,hi=std::max(1.,floor*2);
 if(floor>0 && score(floor)<=0)
   return {floor,mu,sigma,frechet_loglik(x,floor,mu,sigma),0,FrechetStatus::Ok};
 while(score(hi)>0 && hi<1e16) hi*=2;
 if(score(hi)>0) return {hi,mu,sigma,0,0,FrechetStatus::IterationFailure};
 int it=0;
 for(;it<maxit;++it) {
   const double mid=(lo+hi)/2;
   if(score(mid)>0) lo=mid; else hi=mid;
   if(hi-lo<tol*(lo+hi)) break;
 }
 const double a=(lo+hi)/2;
 return {a,mu,sigma,frechet_loglik(x,a,mu,sigma),it+1,
         it<maxit ? FrechetStatus::Ok : FrechetStatus::IterationFailure};
}
inline double frechet_scale(span x,double lambda,double mu) {
 double m=std::numeric_limits<double>::infinity();
 for(double v:x) m=std::min(m,std::log(v-mu));
 double se=0,delta=0,spread=0;
 for(double v:x) {
   const double z=lambda*(std::log(v-mu)-m);
   se+=std::exp(-z);delta+=std::expm1(-z);spread=std::max(spread,z);
 }
 // log(n/sum(exp(-lambda*log(y)))) must retain its O(lambda)
 // term when a held lambda is tiny; otherwise the scale becomes min(y).
 const double correction=spread<.5 ? -std::log1p(delta/x.size()) : std::log(x.size()/se);
 return std::exp(m+correction/lambda);
}
inline FrechetResult frechet_held_mle(span x,double tol,int maxit,double mu,
                                     double lambda,double sigma) {
 const double nan=std::numeric_limits<double>::quiet_NaN();
 FrechetResult bad{lambda,mu,sigma,nan,0,FrechetStatus::InvalidData};
 const bool hm=!std::isnan(mu),hl=!std::isnan(lambda),hs=!std::isnan(sigma);
 if(x.size()==0 || !(tol>0) || !std::isfinite(tol) || maxit<1 ||
   (hm && !std::isfinite(mu)) || (hl && (!(lambda>0) || !std::isfinite(lambda))) ||
   (hs && (!(sigma>0) || !std::isfinite(sigma)))) return bad;
 double m=std::numeric_limits<double>::infinity(),M=-m;
 for(double v:x) {
   if(!std::isfinite(v) || (hm && !(v>mu))) return bad;
   m=std::min(m,v);M=std::max(M,v);
 }
 if(!hm && !hl && m==M) return bad; // shape spike with held scale
 if(hm) {
   if(hl) {
     const double sf=hs ? sigma : frechet_scale(x,lambda,mu);
     const double ll=frechet_loglik(x,lambda,mu,sf);
     return {lambda,mu,sf,ll,0,std::isfinite(ll) ? FrechetStatus::Ok : FrechetStatus::IterationFailure};
   }
   return frechet_shape(x,mu,sigma,0,tol,maxit);
 }
 if(hl && !hs) {
   std::size_t k=0;for(double v:x) if(v==m) ++k;
   if(lambda*(x.size()-k)<=k) return bad;
 }
 // Work in units of range or held scale, keeping log-gap tolerances
 // invariant under location-scale transformations.
 const double unit=hs ? sigma : M-m;
 if(!(unit>0) || !std::isfinite(unit)) return bad;
 std::vector<double> y;for(double v:x)y.push_back((v-m)/unit);
 const span data{y.data(),y.size()};
 auto candidate=[&](double u) {
   const double location=-std::exp(u);
   if(!hl) return frechet_shape(data,location,1,1,tol,maxit);
   const double sf=hs ? 1 : frechet_scale(data,lambda,location);
   return FrechetResult{lambda,location,sf,frechet_loglik(data,lambda,location,sf),0,FrechetStatus::Ok};
 };
 const auto opt=extreme_detail::maximize([&](double u){return candidate(u).logLik;},-32,32,tol,maxit,false);
 auto fit=candidate(opt.x);
 fit.mu=m+unit*fit.mu;fit.sigma=hs ? sigma : unit*fit.sigma;
 fit.logLik=frechet_loglik(x,fit.lambda,fit.mu,fit.sigma);
 if(!opt.ok || fit.status!=FrechetStatus::Ok || !std::isfinite(fit.logLik)) fit.status=FrechetStatus::IterationFailure;
 return fit;
}
inline FrechetResult frechet_mle(span x, double tol=1e-10, int maxit=100,
 double mu=std::numeric_limits<double>::quiet_NaN(),
 double lambda=std::numeric_limits<double>::quiet_NaN(),
 double sigma=std::numeric_limits<double>::quiet_NaN()) {
 if(!std::isnan(lambda) || !std::isnan(sigma)) return frechet_held_mle(x,tol,maxit,mu,lambda,sigma);
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
