#pragma once
#include "../../core/work_budget.hpp"
#include "../../core/stop_detail.hpp"
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
  // The exact half-minimum case has a finite unattained global supremum
  // by the Fréchet pair bound; strict majorities diverge.
  const bool tie_half = std::isnan(xi) && 2*minimum_count==y.size();
  if(std::isnan(xi) ? 2*minimum_count>y.size() :
      (xi>0 && xi*static_cast<double>(minimum_count)>=y.size()-minimum_count)) return bad;
  if(tie_half) {
    double face=y.size()*(std::log(2.)-1);
    for(double v:y)if(v>y[0]) {
      const double gap=v-y[0];
      face-=2*(std::isfinite(gap)?std::log(gap):std::log(v/2-y[0]/2)+std::log(2.));
    }
    bad.status=GevStatus::NoMLE;
    bad.stop_detail.loglik_lower=face;bad.stop_detail.loglik_upper=face;
    return bad;
  }
  // Standardization prevents scale and translation from setting tolerances.
  const double origin=y[0],range=y.back()-origin;
  if(!(range>0) || !std::isfinite(range)) return bad;
  for(double &v:y) v=(v-origin)/range;
  const span data{y.data(),y.size()};
  auto restore=[&](GevResult r) {
    r.mu=origin+range*r.mu;r.sigma*=range;
    const double jac=y.size()*std::log(range);
    r.logLik-=jac;
    if(!std::isnan(r.stop_detail.loglik_lower))r.stop_detail.loglik_lower-=jac;
    if(!std::isnan(r.stop_detail.loglik_upper))r.stop_detail.loglik_upper-=jac;
    if(!std::isnan(r.stop_detail.segment_loglik_upper))r.stop_detail.segment_loglik_upper-=jac;
    for(auto& c:r.stop_detail.candidates) {
      c.coef[0]=origin+range*c.coef[0];c.coef[1]*=range;
      c.loglik_lower-=jac;c.loglik_upper-=jac;
    }
    gev_verify_contradiction(x,r);
    return r;
  };
  if(!std::isnan(xi)) return restore(gev_profile(data,xi,tol,maxit).fit);
  return restore(gev_joint_certificate(data,tol,maxit,true));
}

enum class FrechetStatus { Ok, InvalidData, GumbelLimit, IterationFailure, NoMLE, NumericalFailure, SearchContradicted };
struct FrechetResult { double lambda, mu, sigma, logLik; int iterations; FrechetStatus status; unsigned restricted = 0; StopDetail stop_detail{}; bool certified=true; };
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
} // namespace cppunivariate
#include "certificate.hpp"
namespace cppunivariate {
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
 if(!hm && !hl && hs && m<M && x.size()>=3) {
   std::size_t k=0;bool two_point=true;
   for(double v:x) {if(v==m)++k;else if(v!=M)two_point=false;}
   if(two_point && 2*k<x.size()) {
     auto member=frechet_two_point_certificate(x,m,M,k,tol,maxit);
     if(member.sigma==sigma &&
        extreme_detail::exact_count_scale(m,M,x.size()-2*k,x.size(),sigma)) {
       if(member.status==FrechetStatus::Ok)return member;
       if(member.status!=FrechetStatus::GumbelLimit) {
         // An unconstrained face is not a feasible lower witness under this
         // hold. Its global upper bound still bounds the constrained fit.
         member.stop_detail.loglik_lower=NAN;
         for(const auto& c:member.stop_detail.candidates)
           member.stop_detail.loglik_lower=std::isnan(member.stop_detail.loglik_lower) ?
             c.loglik_lower : std::max(member.stop_detail.loglik_lower,c.loglik_lower);
         member.stop_detail.comparison_tie=false;
         return member;
       }
     }
   }
 }

 if(hm) {
   std::vector<double> excess;for(double v:x)excess.push_back(v-mu);
   const auto r=invweibull_mle(span{excess.data(),excess.size()},tol,maxit,NAN,lambda,sigma);
   FrechetResult out{r.shape,mu,r.scale,r.logLik,r.iterations,
     r.status==InvWeibullStatus::Ok?FrechetStatus::Ok:
     r.status==InvWeibullStatus::IterationFailure?FrechetStatus::IterationFailure:FrechetStatus::InvalidData};
   out.certified=r.certified;return out;
 }
 if(hl && !hs) {
   std::size_t k=0;for(double v:x) if(v==m) ++k;
   const double n=static_cast<double>(x.size());
   const double E=std::fma(lambda,static_cast<double>(x.size()-k),-static_cast<double>(k));
   if(E<=0) {
     double C=n*std::log(n*lambda)-n-n*std::log(static_cast<double>(k));
     for(double v:x)if(v>m) {
       const double gap=v-m;
       const double log_gap=std::isfinite(gap) ? std::log(gap) : std::log(v/2-m/2)+std::log(2.);
       C-=(lambda+1)*log_gap;
     }
     // Retain the historical InvalidData status for this rejected hold;
     // the explicit supremum bound distinguishes the statistical stop.
     bad.stop_detail.loglik_upper=E<0 ? INFINITY : C;
     bad.stop_detail.loglik_lower=bad.stop_detail.loglik_upper;
     return bad;
   }
 }
 // Work in units of range or held scale, keeping log-gap tolerances
 // invariant under location-scale transformations.
 const double unit=hs ? sigma : M-m;
 if(!(unit>0) || !std::isfinite(unit)) {
   if(hl)bad.status=FrechetStatus::NumericalFailure;
   return bad;
 }
 std::vector<double> y;for(double v:x)y.push_back((v-m)/unit);
 const span data{y.data(),y.size()};
 if(hl) {
   auto fit=frechet_fixed_shape_certificate(data,lambda,hs,tol,maxit);
   fit.mu=m+unit*fit.mu;fit.sigma=hs ? sigma : unit*fit.sigma;
   const double jac=x.size()*std::log(unit);
   fit.logLik-=jac;
   auto& detail=fit.stop_detail;
   std::vector<FitCandidate> restored;
   bool lost_witness=false;
   for(auto c:detail.candidates) {
     c.coef[1]=m+unit*c.coef[1];c.coef[2]*=unit;
     c.loglik_lower-=jac;c.loglik_upper-=jac;
     const double evaluated=frechet_loglik(x,c.coef[0],c.coef[1],c.coef[2]);
     if(!std::isfinite(c.coef[1]) || !std::isfinite(c.coef[2]) || !std::isfinite(evaluated)) {
       lost_witness=true;continue;
     }
     c.loglik_lower=std::min(c.loglik_lower,evaluated);
     c.loglik_upper=std::max(c.loglik_upper,evaluated);
     restored.push_back(c);
   }
   detail.candidates=std::move(restored);
   detail.loglik_lower-=jac;detail.loglik_upper-=jac;
   if(!detail.candidates.empty()) {
     detail.loglik_lower=-INFINITY;
     for(const auto& c:detail.candidates)detail.loglik_lower=std::max(detail.loglik_lower,c.loglik_lower);
   } else if(lost_witness)detail.loglik_lower=NAN;
   if(lost_witness)fit.status=FrechetStatus::NumericalFailure;
   if(fit.status==FrechetStatus::Ok) {
     const double evaluated=frechet_loglik(x,fit.lambda,fit.mu,fit.sigma);
     if(!std::isfinite(evaluated) || std::abs(evaluated-fit.logLik)>1e-7*x.size())fit.status=FrechetStatus::NumericalFailure;
     else fit.logLik=evaluated;
   }
   return fit;
 }
 auto candidate=[&](double u) {
   const double location=-std::exp(u);
   if(!hl) return frechet_shape(data,location,1,1,tol,maxit);
   const double sf=hs ? 1 : frechet_scale(data,lambda,location);
   return FrechetResult{lambda,location,sf,frechet_loglik(data,lambda,location,sf),0,FrechetStatus::Ok};
 };
 const auto opt=extreme_detail::maximize([&](double u){return candidate(u).logLik;},-32,32,tol,maxit,false);
 auto fit=candidate(opt.x);
 if(!opt.ok)fit.status=FrechetStatus::IterationFailure;
 const auto standardized_search=fit;
 fit.mu=m+unit*fit.mu;fit.sigma=hs ? sigma : unit*fit.sigma;
 fit.logLik=frechet_loglik(x,fit.lambda,fit.mu,fit.sigma);
 if(!opt.ok || fit.status!=FrechetStatus::Ok || !std::isfinite(fit.logLik)) fit.status=FrechetStatus::IterationFailure;
 if(hs && !hl && fit.status==FrechetStatus::Ok) {
   // The search result is uncertified unless the held-scale cover closes.
   fit.certified=false;
   auto fallback=standardized_search;fallback.certified=false;
   auto candidate=frechet_held_scale_certificate(data,tol,maxit,fallback);
   if(candidate.status==FrechetStatus::SearchContradicted) {
     candidate.mu=m+unit*candidate.mu;candidate.sigma=sigma;
     candidate.logLik=frechet_loglik(x,candidate.lambda,candidate.mu,candidate.sigma);
     const double error=4096*std::numeric_limits<double>::epsilon()*
       (x.size()+std::abs(candidate.logLik)+std::abs(fit.logLik));
     if(std::isfinite(candidate.logLik)&&candidate.logLik>fit.logLik+2*error) {
       candidate.stop_detail={};candidate.stop_detail.loglik_lower=candidate.logLik-error;
       candidate.stop_detail.loglik_upper=INFINITY;
       candidate.stop_detail.candidates.push_back({{fit.lambda,fit.mu,fit.sigma},fit.logLik-error,fit.logLik+error});
       candidate.stop_detail.candidates.push_back({{candidate.lambda,candidate.mu,candidate.sigma},candidate.logLik-error,candidate.logLik+error});
       return candidate;
     }
   }
   if(candidate.certified) {
     const double certificate_tol=std::min(tol,.01);
     const double expected=candidate.logLik-x.size()*std::log(unit);
     const double error=256*std::numeric_limits<double>::epsilon()*(std::abs(m)+unit*std::abs(candidate.mu));
     candidate.mu=m+unit*candidate.mu;candidate.sigma=sigma;
     candidate.logLik=frechet_loglik(x,candidate.lambda,candidate.mu,candidate.sigma);
     if(std::isnormal(std::max(std::abs(candidate.mu),sigma))&&
        error<=certificate_tol*.25*std::max(std::abs(candidate.mu),sigma)&&std::isfinite(candidate.logLik)&&
        std::abs(candidate.logLik-expected)<=1e-7*x.size())fit=candidate;
   }
 }
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
   if(r.status==InvWeibullStatus::IterationFailure) {
     bad.status=FrechetStatus::IterationFailure;return bad;
   }
   if(!(r.shape>0) || !(r.scale>0) || !std::isfinite(r.shape) || !std::isfinite(r.scale) || !std::isfinite(r.logLik)) return bad;
   FrechetResult out{r.shape,mu,r.scale,r.logLik,r.iterations,r.status==InvWeibullStatus::Ok ? FrechetStatus::Ok : FrechetStatus::IterationFailure};
   out.certified=r.certified;return out;
 }
 // The two-point reduction applies only with every public coefficient free.
 if(x.size()>=3) {
   double minimum=INFINITY,maximum=-INFINITY;
   bool finite=true;
   for(double v:x) {finite=finite && std::isfinite(v);minimum=std::min(minimum,v);maximum=std::max(maximum,v);}
   std::size_t k=0;bool two_point=minimum<maximum;
   for(double v:x) {if(v==minimum)++k;else if(v!=maximum)two_point=false;}
   if(finite && two_point && 2*k<x.size())
     return frechet_two_point_certificate(x,minimum,maximum,k,tol,maxit);
 }
 const auto r=frechet_gev_mle(x,tol,maxit);
 if(r.status==GevStatus::InvalidData) return bad;
 if(r.status==GevStatus::NoMLE) {bad.status=FrechetStatus::NoMLE;bad.stop_detail=r.stop_detail;return bad;}
 auto detail=r.stop_detail;
 std::vector<FitCandidate> mapped;
 for(const auto& c:detail.candidates) {
   if(c.coef.size()!=3)continue;
   const double shape=c.coef[2];
   if(shape==0 && c.coef[1]>0) {
     const double ll=gev_loglik(x,c.coef[0],c.coef[1],0);
     if(std::isfinite(ll)) {detail.limit_coef={c.coef[0],c.coef[1]};detail.limit_loglik=ll;}
   } else if(shape>0) {
     const double scale=c.coef[1]/shape,location=c.coef[0]-scale,lambda=1/shape;
     const double ll=frechet_loglik(x,lambda,location,scale);
     if(std::isfinite(lambda)&&std::isfinite(location)&&scale>0&&std::isfinite(scale)&&std::isfinite(ll)) {
       const double margin=4096*std::numeric_limits<double>::epsilon()*(x.size()+std::abs(ll));
       mapped.push_back({{lambda,location,scale},ll,std::max(ll,c.loglik_upper)+margin});
     }
   }
 }
 detail.candidates=std::move(mapped);
 bad.iterations=r.iterations;bad.stop_detail=detail;
 if(r.status!=GevStatus::Ok) {
   bad.status=r.status==GevStatus::SearchContradicted?FrechetStatus::SearchContradicted:
     (r.status==GevStatus::NumericalFailure?FrechetStatus::NumericalFailure:FrechetStatus::IterationFailure);
   return bad;
 }
 if(r.xi==0) {
   const double ll=gev_loglik(x,r.mu,r.sigma,0);
   if(!(r.sigma>0)||!std::isfinite(r.mu)||!std::isfinite(ll)||std::abs(ll-r.logLik)>1e-7*x.size()) {
     bad.status=FrechetStatus::NumericalFailure;return bad;
   }
   bad.status=FrechetStatus::GumbelLimit;
   bad.logLik=ll;
   bad.stop_detail=limit_detail({r.mu,r.sigma},ll);
   bad.stop_detail.loglik_lower=r.stop_detail.loglik_lower;
   bad.stop_detail.loglik_upper=r.stop_detail.loglik_upper;
   return bad;
 }
 const double scale=r.sigma/r.xi,location=r.mu-scale,shape=1/r.xi;
 const double ll=frechet_loglik(x,shape,location,scale);
 const double eps=256*std::numeric_limits<double>::epsilon();
 if(!std::isnormal(scale)||!std::isfinite(shape)||!std::isfinite(location)||!std::isfinite(ll)||
     eps>tol*.25 || eps*(std::abs(r.mu)+scale)>tol*.25*std::max(std::abs(location),scale)||
     std::abs(ll-r.logLik)>1e-7*x.size()) {
   bad.status=FrechetStatus::NumericalFailure;
   if(std::isfinite(shape)&&std::isfinite(location)&&scale>0&&std::isfinite(scale)&&std::isfinite(ll))
     bad.stop_detail.candidates.push_back({{shape,location,scale},ll,ll});
   return bad;
 }
 FrechetResult answer{shape,location,scale,ll,r.iterations,FrechetStatus::Ok};
 answer.certified=r.certified;return answer;
}

inline FrechetResult frechet_mle(span x,double tol=1e-10,int maxit=100,double mu=std::numeric_limits<double>::quiet_NaN(),double lambda=std::numeric_limits<double>::quiet_NaN(),double sigma=std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  auto r=frechet_mle_impl(x,tol,maxit,mu,lambda,sigma);
  if(r.status==FrechetStatus::Ok && (std::isnan(lambda) && std::isnan(mu)) && r.lambda==kFrechetLambdaLower) r.restricted|=1u;
  return r;
}
}
