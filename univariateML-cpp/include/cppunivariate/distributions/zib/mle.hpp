#pragma once
#include "../ztbinom/certificate.hpp"
#include "../../core/work_budget.hpp"
#include "../../core/discrete_stop.hpp"
#include "../ztbinom/mle.hpp"
#include "../zip/mle.hpp"
namespace cppunivariate {
enum class ZibStatus { Ok, InvalidInput, InvalidSize, ZipLimit, IterationFailure };
struct ZibResult {double size, prob, pi, logLik;int iterations;ZibStatus status;  StopDetail stop_detail{};
};
namespace zib_fit_detail {
inline ZibResult zib_mle_proposal(ispan x,double tol=1e-10,int maxit=200,
    double fixed_size=std::numeric_limits<double>::quiet_NaN(),
    double fixed_prob=std::numeric_limits<double>::quiet_NaN(),double fixed_pi=std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  auto stopped = [&]() -> ZibResult {
  const bool fixed=!std::isnan(fixed_size);
  if(fixed && (!(fixed_size>=0)||!std::isfinite(fixed_size)||fixed_size!=std::floor(fixed_size)))
    return {0,0,0,0,0,ZibStatus::InvalidSize};
  if(x.size()==0||!(tol>0)||!std::isfinite(tol)||maxit<1)return {0,0,0,0,0,ZibStatus::InvalidInput};
  double sum=0,zeros=0;int mx=0;
  for(int v:x) {if(v<0)return {0,0,0,0,0,ZibStatus::InvalidInput};sum+=v;zeros+=v==0;mx=std::max(mx,v);}
  if(fixed && fixed_size<mx)return {0,0,0,0,0,ZibStatus::InvalidSize};
  if(sum==0 && std::isnan(fixed_prob) && std::isnan(fixed_pi))return {fixed?fixed_size:1,0,0,0,0,ZibStatus::Ok};
  const double n=x.size(), z=zeros/n, m=sum/n, pm=sum==0?0:sum/(n-zeros);
  const bool fp=!std::isnan(fixed_prob),fi=!std::isnan(fixed_pi);
  if((fp && (!std::isfinite(fixed_prob)||fixed_prob<0||fixed_prob>1)) ||
     (fi && (!std::isfinite(fixed_pi)||fixed_pi<0||fixed_pi>1)))return {0,0,0,0,0,ZibStatus::InvalidInput};
  if(fi && fixed_pi==1 && sum>0)return {0,0,0,0,0,ZibStatus::InvalidInput};
  if(fp || fi) {
    const auto evaluate=[&](double r,double p) {
      const double f0=(r==0 || p==0)?1:(p==1?0:std::exp(r*std::log1p(-p)));
      const double inflation=fi?fixed_pi:(f0<z?(z-f0)/(1-f0):0);
      double ll=0;
      for(int v:x) {
        if(v==0)ll+=std::log(inflation+(1-inflation)*f0);
        else ll+=std::log1p(-inflation)+zero_detail::bin_logpmf(v,r,p);
      }
      return ZibResult{r,p,inflation,ll,0,std::isfinite(ll)?ZibStatus::Ok:ZibStatus::InvalidInput};
    };
    const auto held_profile=[&](double r) {
      if(fp)return evaluate(r,fixed_prob);
      if(sum==0)return evaluate(r,0);
      if(fixed_pi==0 || zeros==0)return evaluate(r,m/r);
      if(fixed_pi==1)return evaluate(r,.5);
      // At any stationary point m/r <= p <= positive_mean/r.
      // Turning points of (1-p)^r*(n*r*p-S)/(S-k*r*p)
      // are the roots of a quadratic. Partition there to isolate every root.
      const double k=n-zeros, lower=m/r, upper=std::min(1.,sum/(k*r));
      std::vector<double> points{lower,upper};
      // Use u=r*p: recovering p coefficients by differencing values at
      // 0, 1/2, 1 loses the linear coefficient for a large held size.
      const double a=n*k,b=-sum*(n+k)-sum*zeros/r,c=sum*sum+sum*zeros;
      const double disc=b*b-4*a*c;
      const double arithmetic=4096*std::numeric_limits<double>::epsilon()*(1+b*b+4*a*c);
      if(!std::isfinite(disc) || std::abs(disc)<=arithmetic) {
        auto out=evaluate(r,lower);out.status=ZibStatus::IterationFailure;return out;
      }
      if(disc>0) {
        const double q=-.5*(b+std::copysign(std::sqrt(disc),b));
        for(double u : {q/a,c/q}) {
          const double p=u/r;
          if(p>lower && p<upper)points.push_back(p);
        }
      }
      std::sort(points.begin(),points.end());
      auto best=evaluate(r,lower);
      auto candidate=evaluate(r,upper);if(candidate.logLik>best.logLik)best=candidate;
      const auto score=[&](double p) {
        const double q=std::exp(r*std::log1p(-p));
        return fixed_pi*(sum-k*r*p)+(1-fixed_pi)*q*(sum-n*r*p);
      };
      for(std::size_t j=1;j<points.size();++j) {
        double lo=points[j-1],hi=points[j];
        if(score(lo)*score(hi)>0)continue;
        const bool positive=score(lo)>0;
        for(int it=0;it<maxit;++it) {
          const double mid=lo+(hi-lo)/2;
          if((score(mid)>0)==positive)lo=mid;else hi=mid;
        }
        if(hi-lo>tol*std::abs(lo)) {
          best.status=ZibStatus::IterationFailure;return best;
        }
        candidate=evaluate(r,lo+(hi-lo)/2);if(candidate.logLik>best.logLik)best=candidate;
      }
      return best;
    };
    if(fixed)return held_profile(fixed_size);
    // Size zero makes the zero mass one for every valid held subset.
    // In particular, size one is suboptimal when p>0 and pi<1 are held.
    if(sum==0)return held_profile(0);
    if(fp) {
      auto best=held_profile(mx);
      if(fixed_prob==0 || fixed_prob==1)return best;
      std::int64_t work=x.size();
      const std::int64_t cap=enumeration_budget(maxit);
      for(std::int64_t j=0;;++j) {
        if(static_cast<std::int64_t>(x.size())>cap-work){
          best.status=ZibStatus::IterationFailure;
          best.stop_detail=discrete_budget(best.stop_detail);
          best.stop_detail.candidates.push_back({{best.size,best.prob,best.pi},best.logLik,best.logLik});
          best.stop_detail.loglik_lower=best.logLik;return best;
        }
        work+=x.size();
        const double r=double(mx)+j;
        auto candidate=held_profile(r);if(candidate.logLik>best.logLik)best=candidate;
        // Each positive mass is bounded by the mass at mx beyond mx/p.
        if(r>=mx/fixed_prob && (n-zeros)*zero_detail::bin_logpmf(mx,r,fixed_prob)<best.logLik)return best;
      }
      best.status=ZibStatus::IterationFailure;return best;
    }
    auto best=held_profile(mx);
    // Exponential grid + golden-section refinement, exactly as
    // ztbinom's free-size search; not a certified unimodality result
    // (see that file's comment and notes/evaluations/2026-10-29-tail85-report.md).
    // The grid always covers the old depth (t<24) -- a strict superset,
    // no existing case regresses -- then keeps growing, governed only
    // by the iteration budget `maxit` and `patience` stable cells, not
    // a hardcoded magnitude.
    int since_improvement=0, t=0;
    const int grid_floor=24, patience=10, cap=static_cast<int>(solver_budget(maxit));
    for(; t<=cap; ++t) {
      bool improved=false;
      auto candidate=held_profile(std::floor(mx+std::exp(t)));
      if(candidate.logLik>best.logLik) {best=candidate;improved=true;}
      bool done=false;
      const double opt=ztbinom_detail::maximize([&](double u){return held_profile(mx+std::exp(u)).logLik;},t-1,t+1,tol,maxit,done);
      if(!done) {best.status=ZibStatus::IterationFailure;return best;}
      for(double r:{std::floor(mx+std::exp(opt)),std::ceil(mx+std::exp(opt))}) {
        candidate=held_profile(r);if(candidate.logLik>best.logLik) {best=candidate;improved=true;}
      }
      if(improved) since_improvement=0; else ++since_improvement;
      if(t>=grid_floor && since_improvement>patience) break;
    }
    // J1 isolates all held-inflation ZIP branches, including ambiguity.
    const auto face=zip_mle(x,NAN,fixed_pi,maxit);
    if(face.status==ZipStatus::Ok && face.logLik>=best.logLik-1e-8*n) {
      best.status=ZibStatus::ZipLimit;
      best.stop_detail=discrete_face(StopModel::Zip,{face.lambda,fixed_pi},face.logLik);
    }
    // Ran out of the iteration budget while still improving, rather
    // than stabilizing: the budget, not a size, is exhausted.
    if(best.status==ZibStatus::Ok && t>cap && since_improvement<=patience)best.status=ZibStatus::IterationFailure;
    return best;
  }
  auto profile=[&](double r) {
    double p=zeros==0?m/r:zero_detail::bin_prob(r,pm,maxit);
    // Certify the positive-mean solve before using it to select the constrained
    // inflation branch; an inaccurate root can otherwise select the wrong side.
    const bool root_done=zeros==0 || pm==1 || (p>0 && p<=1 &&
      std::abs((n-zeros)*(r*p/(-std::expm1(r*std::log1p(-p)))-pm))<1e-7);
    // If the unconstrained mixture needs negative inflation, pi=0.
    if(std::exp(r*std::log1p(-p))>z)p=m/r;
    const double logf0=r*std::log1p(-p), f0=std::exp(logf0);
    const double pi=f0<z ? (z-f0)/(-std::expm1(logf0)) : 0;
    double ll=0;
    for(int v:x) {
      if(v==0)ll+=std::log(pi+(1-pi)*f0);
      else ll+=std::log1p(-pi)+zero_detail::bin_logpmf(v,r,p);
    }
    return ZibResult{r,p,pi,ll,0,std::isfinite(ll)&&root_done?ZibStatus::Ok:ZibStatus::IterationFailure};
  };
  if(fixed)return profile(fixed_size);
  auto best=profile(mx);
  // Same exponential-grid search, same caveats as the two loops above:
  // not certified, grid depth extended via the iteration budget and
  // stabilization rather than a hardcoded magnitude.
  int since_improvement=0, t=0;
  const int grid_floor=24, patience=10, cap=static_cast<int>(solver_budget(maxit));
  for(; t<=cap; ++t) {
    bool improved=false;
    const double r=mx+std::exp(t),ll=profile(r).logLik;
    auto candidate=profile(std::floor(r));
    if(candidate.logLik>best.logLik) {best=candidate;improved=true;}
    if(!(ll<profile(mx+std::exp(t-1)).logLik||ll<profile(mx+std::exp(t+1)).logLik)) {
      bool done=false;
      const double opt=ztbinom_detail::maximize([&](double s){return profile(mx+std::exp(s)).logLik;},t-1,t+1,tol,maxit,done);
      if(!done) {best.status=ZibStatus::IterationFailure;return best;}
      const double s=mx+std::exp(opt);
      for(double k:{std::floor(s),std::ceil(s)}) {candidate=profile(k);if(candidate.logLik>best.logLik) {best=candidate;improved=true;}}
    }
    if(improved) since_improvement=0; else ++since_improvement;
    if(t>=grid_floor && since_improvement>patience) break;
  }
  const auto zip=zip_mle(x);
  if(zip.status==ZipStatus::Ok && zip.logLik>=best.logLik-1e-8*n) {best.status=ZibStatus::ZipLimit;
    best.stop_detail=discrete_face(StopModel::Zip,{zip.lambda,zip.pi},zip.logLik);}
  if(best.status==ZibStatus::Ok && t>cap && since_improvement<=patience)best.status=ZibStatus::IterationFailure;
  return best;

  }();

  return stopped;
}
} // namespace zib_fit_detail
inline ZibResult zib_mle(ispan x,double tol=1e-10,int maxit=200,
    double fixed_size=NAN,double fixed_prob=NAN, double fixed_pi=NAN) {
  // The support {0,1} is saturated at size one, even with inflation held.
  if((std::isnan(fixed_size)||fixed_size==1) && std::isnan(fixed_prob) && x.size()>0 && maxit>1 && tol>0 && std::isfinite(tol)) {
    bool binary=true;double sum=0;for(int v:x){binary=binary && v>=0 && v<=1;sum+=v;}
    if(binary && sum>0 && (std::isnan(fixed_pi)||(fixed_pi>=0 && fixed_pi<1))) {
      const double pi=std::isnan(fixed_pi)?0:fixed_pi;
      const double p=std::min(1.,sum/(x.size()*(1-pi)));
      double ll=sum*std::log((1-pi)*p);
      if(sum<x.size())ll+=(x.size()-sum)*std::log(1-(1-pi)*p);
      return {1,p,pi,ll,0,ZibStatus::Ok};
    }
  }
  auto best=zib_fit_detail::zib_mle_proposal(x,tol,maxit,fixed_size,fixed_prob,fixed_pi);
  if(!std::isnan(fixed_size)||!std::isnan(fixed_prob)||x.size()==0) {
    if(best.status==ZibStatus::IterationFailure)
      binomial_certificate_detail::witness(best.stop_detail,{best.size,best.prob,best.pi},best.logLik,false);
    return best;
  }
  int mx=0;double sum=0;for(int v:x){mx=std::max(mx,v);sum+=v;}
  if(sum==0 || mx==1)return best;
  if(best.status==ZibStatus::InvalidInput || best.status==ZibStatus::InvalidSize)return best;
  if(best.status==ZibStatus::IterationFailure) {
    binomial_certificate_detail::witness(best.stop_detail,{best.size,best.prob,best.pi},best.logLik,false);return best;
  }
  // A face is a witness, never a J7a proof that no finite size wins.
  if(best.status!=ZibStatus::Ok) {
    binomial_certificate_detail::witness(best.stop_detail,{best.size,best.prob,best.pi},best.logLik,false,true);return best;
  }
  if(best.logLik==0)return best;
  binomial_certificate_detail::Work work{0,0,maxit};
  const double end=binomial_certificate_detail::cutoff(x,0,INFINITY,true,fixed_pi,best.logLik,work);
  if(std::isfinite(end) && best.size>=end)work.numerical=true;
  if(!std::isfinite(end) || work.numerical) {
    best.status=ZibStatus::IterationFailure;
    binomial_certificate_detail::witness(best.stop_detail,{best.size,best.prob,best.pi},best.logLik,!work.numerical);return best;
  }
  double incumbent=best.logLik;
  const bool complete=binomial_certificate_detail::cover(x,mx,end,incumbent,work,[&](double N) {
    const auto f=zib_fit_detail::zib_mle_proposal(x,tol,maxit,N,NAN,fixed_pi);
    if(f.status!=ZibStatus::Ok)return double(NAN);
    if(f.logLik>best.logLik)best=f;
    return f.logLik;
  });
  if(!complete) {
    best.status=ZibStatus::IterationFailure;
    binomial_certificate_detail::witness(best.stop_detail,{best.size,best.prob,best.pi},best.logLik,!work.numerical);
  }
  return best;
}

}
