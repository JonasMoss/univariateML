#pragma once
#include "../../core/work_budget.hpp"
#include "../gev/mle.hpp"
#include "../invweibull/mle.hpp"
namespace cppunivariate {
inline constexpr double kFrechetLambdaLower = 1.0;

inline GevResult frechet_gev_mle(span x, double tol=1e-10, int maxit=100,
                         double xi=std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
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
  // At positive xi (lambda=1/xi) the lower-endpoint collapse is
  // controlled only by the multiplicity of the minimum. A strict
  // majority (k>n/2) diverges regardless of xi; exact equality
  // (k==n/2) is data-dependent when xi is also free, since xi->1
  // (lambda->1) is then a finite but unattained collapse face that an
  // interior maximum elsewhere can still beat -- see gev/mle.hpp's
  // identical fix and notes/evaluations/2026-10-29-p0-extreme.md.
  const bool tie_half = std::isnan(xi) && 2*minimum_count==y.size();
  if(std::isnan(xi) ? 2*minimum_count>y.size() :
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
  //
  // At the k==n/2 tie, xi=1 (lambda=1) is itself the unattained face
  // (gev/mle.hpp's identical reasoning); nudge the scan's upper end
  // strictly below it rather than evaluate there directly.
  const double xi_hi = tie_half ? kGevXiUpper-1e-6 : kGevXiUpper;
  const int cells=32;
  for(int j=1;j<=cells;++j) {
    const auto next=gev_profile(data,xi_hi*j/cells,tol,maxit);
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
  GevResult result=restore(best);
  if(result.status==GevStatus::Ok && tie_half) {
    // A_free (GEV xi->1 collapse face, free mu and sigma) applies
    // verbatim: Frechet's own log-likelihood at a given (mu,sigma,lambda)
    // is the GEV log-likelihood at (mu-sigma/xi, sigma/xi, xi=1/lambda)
    // (frechet_mle_impl's own reparametrization below), so the same
    // closed-form supremum bounds the same face here. See gev/mle.hpp.
    double sum_log_d=0;
    for(double v:x) if(v>origin) sum_log_d+=std::log(v-origin);
    const double n=static_cast<double>(y.size());
    const double a_free=n*std::log(2.0)-n-2.0*sum_log_d;
    if(!(result.logLik>a_free+tol*n)) result.status=GevStatus::NoMLE;
  }
  return result;
}

enum class FrechetStatus { Ok, InvalidData, GumbelLimit, IterationFailure, NoMLE };
struct FrechetResult { double lambda, mu, sigma, logLik; int iterations; FrechetStatus status; unsigned restricted = 0; };
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
  maxit = static_cast<int>(solver_budget(maxit));
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
 for(std::int64_t bracket_pass=0; bracket_pass<solver_budget(maxit) && (score(hi)>0 && hi<1e16); ++bracket_pass) hi*=2;
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
  maxit = static_cast<int>(solver_budget(maxit));
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
   std::vector<double> excess;for(double v:x)excess.push_back(v-mu);
   const auto r=invweibull_mle(span{excess.data(),excess.size()},tol,maxit,NAN,lambda,sigma);
   return {r.shape,mu,r.scale,r.logLik,r.iterations,
     r.status==InvWeibullStatus::Ok?FrechetStatus::Ok:
     r.status==InvWeibullStatus::IterationFailure?FrechetStatus::IterationFailure:FrechetStatus::InvalidData};
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
inline FrechetResult frechet_mle_impl(span x, double tol=1e-10, int maxit=100,
 double mu=std::numeric_limits<double>::quiet_NaN(),
 double lambda=std::numeric_limits<double>::quiet_NaN(),
 double sigma=std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
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
   if(!(r.shape>0) || !(r.scale>0) || !std::isfinite(r.shape) || !std::isfinite(r.scale) || !std::isfinite(r.logLik)) return bad;
   return {r.shape,mu,r.scale,r.logLik,r.iterations,r.status==InvWeibullStatus::Ok ? FrechetStatus::Ok : FrechetStatus::IterationFailure};
 }
 const auto r=frechet_gev_mle(x,tol,maxit);
 if(r.status==GevStatus::InvalidData) return bad;
 if(r.status==GevStatus::NoMLE) {bad.status=FrechetStatus::NoMLE;return bad;}
 if(r.status!=GevStatus::Ok) {bad.status=FrechetStatus::IterationFailure;return bad;}
 if(r.xi==0) {bad.status=FrechetStatus::GumbelLimit;return bad;}
 return {1/r.xi,r.mu-r.sigma/r.xi,r.sigma/r.xi,r.logLik,r.iterations,FrechetStatus::Ok};
}

inline FrechetResult frechet_mle(span x,double tol=1e-10,int maxit=100,double mu=std::numeric_limits<double>::quiet_NaN(),double lambda=std::numeric_limits<double>::quiet_NaN(),double sigma=std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  auto r=frechet_mle_impl(x,tol,maxit,mu,lambda,sigma);
  if(r.status==FrechetStatus::Ok && (std::isnan(lambda) && std::isnan(mu)) && r.lambda==kFrechetLambdaLower) r.restricted|=1u;
  return r;
}
}
