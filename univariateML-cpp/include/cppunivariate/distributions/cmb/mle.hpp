// Full finite exponential family in (logit(prob), nu).
#pragma once
#include "../../core/work_budget.hpp"
#include "../../core/discrete_stop.hpp"
#include "dpqr.hpp"
#include "acceptance.hpp"
#include "../../core/span.hpp"
#include "../../special/cmp.hpp"
#include <limits>
#include <utility>
#include <map>
namespace cppunivariate {
enum class CmbStatus { Ok, InvalidInput, PointMass, AdjacentLimit, EndpointLimit,
                       Unidentified, IterationFailure, FaceUnresolved, FaceTie,
                       NotImplemented };
struct CmbResult { double prob,nu,logLik; int iterations; CmbStatus status;   StopDetail stop_detail{};
};
namespace cmb_detail {
inline double probability(double eta) {
  return eta>=0 ? 1/(1+std::exp(-eta)) : std::exp(eta)/(1+std::exp(eta));
}
// One cell is one supported count's normalizer/moment contribution. The
// finite number of table passes is O(1) work per cell. All delegated tables
// share this ledger; record the best feasible represented probability too.
struct TableWork {
  std::uint64_t& used;
  std::uint64_t cap;
  const std::map<int,std::uint64_t>& counts;
  double fixed_prob=NAN;
  bool exhausted=false;
  double best_prob=NAN,best_nu=NAN,best_loglik=-INFINITY,best_error=0;
  Table table(int N,double eta,double nu) {
    const auto cost=static_cast<std::uint64_t>(N)+1;
    if(exhausted || cost>cap-used) {
      exhausted=true;
      Table out(0,0,0);out.mean=out.mh=out.vx=out.vh=out.cov=NAN;return out;
    }
    used+=cost;
    Table out(N,eta,nu);
    double ll=0,n=0;for(const auto& kv:counts){ll+=kv.second*out.lp[kv.first];n+=kv.second;}
    const double prob=std::isnan(fixed_prob)?probability(eta):fixed_prob;
    if(prob>0 && prob<1 && std::isfinite(nu) && std::isfinite(ll)) {
      const double reported=std::log(prob)-std::log1p(-prob);
      // |d ell / d eta| <= n*N: bound the coordinate-rounding change
      // without spending another normalizer after exhaustion.
      const double error=n*N*std::abs(reported-eta)+64*std::numeric_limits<double>::epsilon()*n*
        (1+std::abs(ll)+N*(1+std::abs(eta)+std::abs(reported)));
      if(std::isfinite(error) && ll-error>best_loglik-best_error) {
        best_prob=prob;best_nu=nu;best_loglik=ll;best_error=error;
      }
    }
    return out;
  }
};
// Mean matching has derivative Var(X). Safeguarded Newton within a bracket.
inline double match_mean(int N,double nu,double m,int maxit,TableWork& work) {
  maxit = static_cast<int>(solver_budget(maxit));
  double lo=-1,hi=1;
  for (int j=0;j<maxit && work.table(N,lo,nu).mean>m;++j) lo*=2;
  for (int j=0;j<maxit && work.table(N,hi,nu).mean<m;++j) hi*=2;
  double eta=0;
  for (int j=0;j<maxit;++j) {
    const Table tab=work.table(N,eta,nu);
    if(!std::isfinite(tab.mean))return NAN;
    const double score=tab.mean-m;
    if (std::abs(score)<2e-14*(1+m)) return eta;
    if (score<0) lo=eta; else hi=eta;
    const double next=eta-score/tab.vx;
    eta=std::isfinite(next) && next>lo && next<hi ? next : lo+(hi-lo)/2;
  }
  return NAN;
}
}
inline CmbResult cmb_mle_work(ispan x,int N,double tol,int maxit,
                        double prob,double nu,std::uint64_t& used,std::uint64_t budget) {
  std::map<int,std::uint64_t> counts;
  cmb_detail::TableWork work{used,budget,counts,prob};
  maxit = static_cast<int>(solver_budget(maxit));
  auto stopped = [&]() -> CmbResult {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const bool fp=!std::isnan(prob),fn=!std::isnan(nu);
  const auto fail=[&](CmbStatus status) { return CmbResult{nan,nan,nan,0,status}; };
  if (x.size()==0 || N<0 || N>10000000 || !(tol>0) || !std::isfinite(tol) || maxit<1 ||
      (fp && !(prob>=0 && prob<=1)) || (fn && !std::isfinite(nu))) return fail(CmbStatus::InvalidInput);
  int xmin=N,xmax=0; double sx=0,sh=0;
  bool endpoints=true,centres=true;
  for (int v:x) {
    if (v<0 || v>N) return fail(CmbStatus::InvalidInput);
    xmin=std::min(xmin,v); xmax=std::max(xmax,v); sx+=v; sh+=cmb_detail::logchoose(N,v);++counts[v];
    endpoints=endpoints && (v==0 || v==N);
    centres=centres && (v==N/2 || v==(N+1)/2);
  }
  const double n=static_cast<double>(x.size()),m=sx/n,t=sh/n;
  if (N==0) return {fp?prob:.5,fn?nu:1,0,0,CmbStatus::Ok};
  if (fp && (prob==0 || prob==1))
    return (prob==0?xmax==0:xmin==N) ? CmbResult{prob,fn?nu:1,0,0,CmbStatus::Ok} : fail(CmbStatus::InvalidInput);
  if (!fp && (m==0 || m==N)) return {m==0?0.0:1.0,fn?nu:1,0,0,CmbStatus::Ok};
  if (!fp && !fn && N>1) {
    if (xmin==xmax) return fail(CmbStatus::PointMass);
    if (xmax-xmin==1) return fail(CmbStatus::AdjacentLimit);
    if (endpoints) return fail(CmbStatus::EndpointLimit);
  }
  if (fp && !fn && N>1) {
    if (endpoints) return fail(CmbStatus::EndpointLimit);
    if (centres) return fail(N%2==0 ? CmbStatus::PointMass : CmbStatus::AdjacentLimit);
  }
  const double fixed_eta=fp ? std::log(prob)-std::log1p(-prob) : 0;
  int iterations=0;
  if (!fn && N>1) {
    const auto score=[&](double v) {
      const double eta=fp ? fixed_eta : cmb_detail::match_mean(N,v,m,maxit,work);
      return work.table(N,eta,v).mh-t;
    };
    // Profile derivative Var(h)-Cov(X,h)^2/Var(X) is positive.
    double lo=-1,hi=1;
    for (int j=0;j<maxit && score(lo)>0;++j) lo*=2;
    for (int j=0;j<maxit && score(hi)<0;++j) hi*=2;
    if (!(score(lo)<=0 && score(hi)>=0)) return fail(CmbStatus::IterationFailure);
    for (;iterations<maxit;++iterations) {
      nu=lo+(hi-lo)/2;
      const double g=score(nu);
      if (std::abs(g)<tol*(1+std::abs(t))) break;
      if (g<0) lo=nu; else hi=nu;
    }
    if (iterations==maxit) return fail(CmbStatus::IterationFailure);
  } else if (!fn) nu=1;
  const double eta=fp ? fixed_eta : cmb_detail::match_mean(N,nu,m,maxit,work);
  const double fitted_prob=fp ? prob : cmb_detail::probability(eta);
  if (!(fitted_prob>0 && fitted_prob<1)) return fail(CmbStatus::IterationFailure);
  // Evaluate the coordinates actually returned to the caller, including
  // the rounding incurred by converting canonical log odds to probability.
  const double reported_eta=std::log(fitted_prob)-std::log1p(-fitted_prob);
  const auto tab=work.table(N,reported_eta,nu);
  if(work.exhausted)return fail(CmbStatus::IterationFailure);
  if ((!fp && std::abs(tab.mean-m)>1e-10*(1+m)) ||
      (!fn && N>1 && std::abs(tab.mh-t)>tol*(1+std::abs(t))))
    return fail(CmbStatus::IterationFailure);
  double ll=0; for (int v:x) ll+=tab.lp[v];
  return {fitted_prob,nu,ll,iterations,CmbStatus::Ok};

  }();
  if(stopped.status==CmbStatus::PointMass) stopped.stop_detail=discrete_point(x[0]);
  if(stopped.status==CmbStatus::AdjacentLimit || stopped.status==CmbStatus::EndpointLimit) {
    const double lo=stopped.status==CmbStatus::EndpointLimit?0:*std::min_element(x.begin(),x.end());
    const double hi=stopped.status==CmbStatus::EndpointLimit?N:lo+1;
    double p=NAN,eta=NAN;
    if(!std::isnan(prob)) {
      eta=(hi-lo)*(std::log(prob)-std::log1p(-prob));
      p=cmb_detail::probability(eta);
    }
    stopped.stop_detail=discrete_two_point(x,lo,hi,p,eta);
  }
  if(work.exhausted) {
    stopped.status=CmbStatus::IterationFailure;
    stopped.stop_detail=discrete_budget(stopped.stop_detail);
    if(std::isfinite(work.best_loglik)) {
      const double margin=work.best_error;
      stopped.stop_detail.candidates.push_back({{double(N),work.best_prob,work.best_nu},work.best_loglik-margin,work.best_loglik+margin});
      stopped.stop_detail.loglik_lower=work.best_loglik-margin;
    }
  }
  return stopped;
}
inline CmbResult cmb_accept_work(ispan x,int N,CmbResult out,int maxit,
                                 double prob,double nu,std::uint64_t& work) {
  if(out.status==CmbStatus::Ok && !(std::isfinite(out.prob) && std::isfinite(out.nu) && std::isfinite(out.logLik))) {
    out.status=CmbStatus::IterationFailure;out.stop_detail.loglik_upper=INFINITY;return out;
  }
  if(out.status!=CmbStatus::Ok || N<=1 || out.prob==0 || out.prob==1 ||
     (!std::isnan(prob) && !std::isnan(nu)))return out;
  std::map<int,std::uint64_t> counts;for(int k:x)++counts[k];
  cmb_detail::TableWork ledger{work,static_cast<std::uint64_t>(enumeration_budget(maxit)),counts,prob};
  const double eta=std::log(out.prob)-std::log1p(-out.prob), n=double(x.size());
  double m=0,t=0;for(int k:x){m+=k;t+=cmb_detail::logchoose(N,k);}m/=n;t/=n;
  const auto tab=ledger.table(N,eta,out.nu);
  const bool fp=!std::isnan(prob),fn=!std::isnan(nu);
  double a=0,b=0,c=0;
  if(fp)a=1/tab.vh;
  else if(fn)a=1/tab.vx;
  else {const double det=tab.vx*tab.vh-tab.cov*tab.cov;
    a=tab.vh/det;b=-tab.cov/det;c=tab.vx/det;}
  const auto evaluate=[&](double u,double v) {
    const double e=fp?eta:u, dispersion=fp?u:(fn?out.nu:v);
    const auto q=ledger.table(N,e,dispersion);
    if(!std::isfinite(q.mean))return concave_accept_detail::Evaluation{};
    double ll=0;for(const auto& kv:counts)ll+=kv.second*q.lp[kv.first];
    return concave_accept_detail::Evaluation{ll/n,fp?t-q.mh:m-q.mean,
      (!fp && !fn)?t-q.mh:0,std::isfinite(ll) && std::isfinite(q.mean)};
  };
  const auto accurate=[&](const concave_accept_detail::Bound& z) {
    if(!fp && (std::abs(cmb_detail::probability(z.lo1)-out.prob)>1e-8*(1+out.prob) ||
               std::abs(cmb_detail::probability(z.hi1)-out.prob)>1e-8*(1+out.prob)))return false;
    const double low=fp?z.lo1:z.lo2,high=fp?z.hi1:z.hi2;
    return fn || std::max(std::abs(low-out.nu),std::abs(high-out.nu))<=1e-8*(1+std::abs(out.nu));
  };
  const auto bound=concave_accept_detail::certify(fp?out.nu:eta,out.nu,out.logLik/n,
    a,b,c,(!fp && !fn)?2:1,false,maxit,evaluate,accurate);
  out.stop_detail.loglik_lower=out.logLik-1e-9*(n+std::abs(out.logLik));
  out.stop_detail.loglik_upper=n*bound.upper;
  if(!bound.ok || ledger.exhausted) {
    out.status=CmbStatus::IterationFailure;
    out.stop_detail.candidates.push_back({{double(N),out.prob,out.nu},out.stop_detail.loglik_lower,
      out.logLik+1e-9*(n+std::abs(out.logLik))});
    if(ledger.exhausted)out.stop_detail=discrete_budget(out.stop_detail);
  }
  return out;
}

inline CmbResult cmb_mle(ispan x,int N,double tol=1e-10,int maxit=2000,
                        double prob=std::numeric_limits<double>::quiet_NaN(),
                        double nu=std::numeric_limits<double>::quiet_NaN()) {
  std::uint64_t work=0;
  const auto proposed=cmb_mle_work(x,N,tol,maxit,prob,nu,work,enumeration_budget(maxit));
  auto out=cmb_accept_work(x,N,proposed,maxit,prob,nu,work);
  // A flat profile can meet the historical moment tolerance before its
  // coefficients are accurate. Retain passing proposals exactly; otherwise
  // one tighter solve and its own certificate share the remaining ledger.
  // A moment tolerance of 1e-14 can be unattainable in double arithmetic
  // (no long double); looser proposals are still only proposals, so try
  // them in turn and keep the first one the certificate accepts.
  if(proposed.status==CmbStatus::Ok && out.status==CmbStatus::IterationFailure &&
     !out.stop_detail.budget_exhausted) {
    for(const double refine_tol:{1e-14,1e-13,1e-12}) {
      if(!(tol>refine_tol))continue;
      auto refined=cmb_mle_work(x,N,refine_tol,maxit,prob,nu,work,enumeration_budget(maxit));
      if(refined.status!=CmbStatus::Ok && !refined.stop_detail.budget_exhausted)continue;
      if(refined.status==CmbStatus::Ok)refined=cmb_accept_work(x,N,refined,maxit,prob,nu,work);
      if(refined.status!=CmbStatus::Ok) {
        for(const auto& witness:out.stop_detail.candidates)refined.stop_detail.candidates.push_back(witness);
        refined.stop_detail.loglik_lower=std::fmax(refined.stop_detail.loglik_lower,out.stop_detail.loglik_lower);
      }
      out=refined;
      if(out.status==CmbStatus::Ok || out.stop_detail.budget_exhausted)break;
    }
  }
  return out;
}

struct CmbSizeResult { double size; CmbResult fit; };

namespace cmb_detail {
// A likelihood superlevel also bounds the probability of every observed
// category. Maximizing the other probabilities at fixed p[value] gives a
// binary relative-entropy inequality. Return the lower bracket endpoint.
inline double observed_log_lower(ispan x,int value,double B,int maxit=2000) {
  maxit = static_cast<int>(solver_budget(maxit));
  int mx=0;for(int v:x)mx=std::max(mx,v);
  std::vector<std::size_t> counts(std::size_t(mx)+1,0);
  for(int v:x)++counts[std::size_t(v)];
  const double n=double(x.size()),c=double(counts[std::size_t(value)]),f=c/n;
  if(!(f>0 && f<1))return -INFINITY;
  double saturated=0;
  for(std::size_t v:counts)if(v) saturated+=double(v)*std::log(double(v)/n);
  saturated+=1e-9*(1+std::abs(saturated));
  if(saturated<B)return -INFINITY;
  double lo=0,hi=f;
  for(int i=0;i<maxit;++i) {
    const double mid=(lo+hi)/2;
    if(mid==0)break;
    const double bound=saturated+c*std::log(mid/f)+(n-c)*(std::log1p(-mid)-std::log1p(-f));
    if(bound>=B)hi=mid;else lo=mid;
  }
  if(!(lo>0))return -INFINITY;
  const double result=std::log(lo);
  return result-1e-10*(1+std::abs(result));
}
inline double observed_log_upper(ispan x,int value,double B,int maxit=2000) {
  maxit = static_cast<int>(solver_budget(maxit));
  int mx=0;for(int v:x)mx=std::max(mx,v);
  std::vector<std::size_t> counts(std::size_t(mx)+1,0);
  for(int v:x)++counts[std::size_t(v)];
  const double n=double(x.size()),c=double(counts[std::size_t(value)]),f=c/n;
  if(!(f>0 && f<1))return 0;
  double saturated=0;
  for(std::size_t v:counts)if(v)saturated+=double(v)*std::log(double(v)/n);
  saturated+=1e-9*(1+std::abs(saturated));
  if(saturated<B)return 0;
  double lo=f,hi=1;
  for(int i=0;i<maxit;++i) {
    const double mid=(lo+hi)/2;
    if(mid>=1)break;
    const double bound=saturated+c*std::log(mid/f)+(n-c)*(std::log1p(-mid)-std::log1p(-f));
    if(bound>=B)lo=mid;else hi=mid;
  }
  return std::min(0.,std::log(hi)+1e-10*(1+std::abs(std::log(hi))));
}
// A damped natural-coordinate Newton solve for free-size enumeration. Each
// normalizer table is charged against the shared budget. Fixed-size public
// fits retain their historical solver and goldens.
inline CmbResult size_candidate(ispan x,int N,double prob,double tol,int maxit,
                                std::uint64_t& work,std::uint64_t budget) {
  maxit = static_cast<int>(solver_budget(maxit));
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const auto fail=[&](CmbStatus s){return CmbResult{nan,nan,nan,0,s};};
  const bool fixed=!std::isnan(prob);
  const std::size_t cost=std::size_t(N)+1;
  if(N==1) {
    return cmb_mle_work(x,N,tol,maxit,prob,nan,work,budget);
  }
  int mn=N,mx=0;double sx=0,sh=0;
  bool endpoints=true,centres=true;
  for(int v:x) {
    mn=std::min(mn,v);mx=std::max(mx,v);sx+=v;sh+=logchoose(N,v);
    endpoints=endpoints && (v==0 || v==N);
    centres=centres && (v==N/2 || v==(N+1)/2);
  }
  if(!fixed) {
    if(mx-mn==1)return fail(CmbStatus::AdjacentLimit);
    if(endpoints)return fail(CmbStatus::EndpointLimit);
  } else {
    if(endpoints)return fail(CmbStatus::EndpointLimit);
    if(centres)return fail(N%2==0?CmbStatus::PointMass:CmbStatus::AdjacentLimit);
  }
  const double n=double(x.size()),m=sx/n,t=sh/n;
  double eta=fixed?std::log(prob)-std::log1p(-prob):std::log(m/(N-m)),nu=1;
  for(int it=0;it<maxit;++it) {
    if(!std::isfinite(eta) || !std::isfinite(nu))return fail(CmbStatus::IterationFailure);
    if(cost>budget-work){auto out=fail(CmbStatus::IterationFailure);out.stop_detail=discrete_budget(out.stop_detail);return out;}
    work+=cost;
    const Table tab(N,eta,nu);
    double ll=0;for(int v:x)ll+=tab.lp[v];
    const double gx=m-tab.mean,gh=t-tab.mh;
    if((fixed || std::abs(gx)<=tol*(1+m)) && std::abs(gh)<=tol*(1+std::abs(t))) {
      const double p=fixed?prob:probability(eta);
      if(!(p>0 && p<1))return fail(CmbStatus::IterationFailure);
      if(cost>budget-work){auto out=fail(CmbStatus::IterationFailure);out.stop_detail=discrete_budget(out.stop_detail);return out;}
      work+=cost;
      const double reported_eta=std::log(p)-std::log1p(-p);
      const Table check(N,reported_eta,nu);
      if((!fixed && std::abs(m-check.mean)>tol*(1+m)) ||
         std::abs(t-check.mh)>tol*(1+std::abs(t)))return fail(CmbStatus::IterationFailure);
      ll=0;for(int v:x)ll+=check.lp[v];
      return {p,nu,ll,it,CmbStatus::Ok};
    }
    double de=0,dv=0;
    if(fixed) {
      if(!(tab.vh>0))return fail(CmbStatus::IterationFailure);
      dv=gh/tab.vh;
    } else {
      const double det=tab.vx*tab.vh-tab.cov*tab.cov;
      if(!(det>0) || !std::isfinite(det))return fail(CmbStatus::IterationFailure);
      de=(tab.vh*gx-tab.cov*gh)/det;
      dv=(tab.vx*gh-tab.cov*gx)/det;
    }
    if(!std::isfinite(de) || !std::isfinite(dv))return fail(CmbStatus::IterationFailure);
    // Near an endpoint face the covariance can be tiny, making an
    // unconstrained Newton step too large for finite backtracking. Limit
    // the displacement before the likelihood line search.
    const double displacement=std::max(std::abs(de),std::abs(dv));
    if(displacement>4) {de*=4/displacement;dv*=4/displacement;}
    bool accepted=false;
    double step=1;
    for(std::int64_t backtrack=0;backtrack<solver_budget(maxit) && step>1e-12;++backtrack,step*=.5) {
      if(cost>budget-work){auto out=fail(CmbStatus::IterationFailure);out.stop_detail=discrete_budget(out.stop_detail);return out;}
      work+=cost;
      const Table next(N,eta+step*de,nu+step*dv);
      double next_ll=0;for(int v:x)next_ll+=next.lp[v];
      if(std::isfinite(next_ll) && next_ll>=ll-1e-12*(1+std::abs(ll))) {
        eta+=step*de;nu+=step*dv;accepted=true;break;
      }
    }
    if(!accepted)return fail(CmbStatus::IterationFailure);
  }
  return fail(CmbStatus::IterationFailure);
}

inline CmbResult size_candidate_rescue(ispan x,int N,double prob,double tol,int maxit,
                                       std::uint64_t& work,std::uint64_t budget) {
  maxit = static_cast<int>(solver_budget(maxit));
  CmbResult result=size_candidate(x,N,prob,tol,maxit,work,budget);
  if(result.status!=CmbStatus::IterationFailure)return result;
  if(result.stop_detail.budget_exhausted)return result;
  // The historical monotone dispersion bisection and all nested mean
  // matches spend the same remaining cell allowance as their caller.
  return cmb_mle_work(x,N,tol,maxit,prob,std::numeric_limits<double>::quiet_NaN(),work,budget);
}
// Entropy dual upper bound for the two-coordinate truncated CMP profile.
// A nonnegative probability vector with the sample moments bounds the
// profile by minus its entropy. Correct three weights after the solve and
// reject the certificate if the correction is not feasible.
inline bool truncated_upper(ispan x,int K,double& upper,std::uint64_t& work,
                            std::uint64_t budget,StopDetail& evidence,int N=0,int maxit=2000) {
  maxit = static_cast<int>(solver_budget(maxit));
  const double n=double(x.size());
  if(N && N<K)return false;
  std::vector<double> h(K+1),q(K+1);
  double correction=0;
  for(int k=0;k<=K;++k) {
    if(N && k>0)correction+=std::log1p(-double(k-1)/N);
    h[k]=-std::lgamma(k+1.)+correction;
  }
  double target_x=0,target_h=0;
  for(int v:x) {target_x+=v;target_h+=h[v];}
  target_x/=n;target_h/=n;
  double a=0,b=1;
  for(int it=0;it<maxit;++it) {
    if(std::size_t(K+1)>budget-work){evidence=discrete_budget(evidence);return false;}
    work+=std::size_t(K+1);
    double peak=-INFINITY;
    for(int k=0;k<=K;++k)peak=std::max(peak,a*k+b*h[k]);
    double z=0,mx=0,mh=0;
    for(int k=0;k<=K;++k) {q[k]=std::exp(a*k+b*h[k]-peak);z+=q[k];}
    for(int k=0;k<=K;++k) {q[k]/=z;mx+=q[k]*k;mh+=q[k]*h[k];}
    const double gx=target_x-mx,gh=target_h-mh;
    if(std::max(std::abs(gx),std::abs(gh))<1e-13*(1+std::abs(target_h)))break;
    double xx=0,xh=0,hh=0;
    for(int k=0;k<=K;++k) {
      const double dx=k-mx,dh=h[k]-mh;
      xx+=q[k]*dx*dx;xh+=q[k]*dx*dh;hh+=q[k]*dh*dh;
    }
    const double det=xx*hh-xh*xh;
    if(!(det>0) || !std::isfinite(det))return false;
    const double da=(hh*gx-xh*gh)/det,db=(xx*gh-xh*gx)/det;
    const double objective=a*target_x+b*target_h-peak-std::log(z);
    bool accepted=false;
    double step=1;
    for(std::int64_t backtrack=0;backtrack<solver_budget(maxit) && step>1e-12;++backtrack,step*=.5) {
      if(std::size_t(K+1)>budget-work){evidence=discrete_budget(evidence);return false;}
      work+=std::size_t(K+1);
      double p=-INFINITY;
      for(int k=0;k<=K;++k)p=std::max(p,(a+step*da)*k+(b+step*db)*h[k]);
      double s=0;for(int k=0;k<=K;++k)s+=std::exp((a+step*da)*k+(b+step*db)*h[k]-p);
      const double next=(a+step*da)*target_x+(b+step*db)*target_h-p-std::log(s);
      if(next>=objective-1e-14) {a+=step*da;b+=step*db;accepted=true;break;}
    }
    if(!accepted)return false;
  }
  double peak=-INFINITY;
  for(int k=0;k<=K;++k)peak=std::max(peak,a*k+b*h[k]);
  double z=0;for(int k=0;k<=K;++k){q[k]=std::exp(a*k+b*h[k]-peak);z+=q[k];}
  double sx=0,sh=0,s=0;
  for(int k=0;k<=K;++k){q[k]/=z;s+=q[k];sx+=k*q[k];sh+=h[k]*q[k];}
  // Choose well-populated distinct support points to keep the adjustment
  // away from zero; their log-factorials are strictly convex.
  int i=0,j=1,l=2;
  for(int k=0;k<=K;++k)if(q[k]>q[i])i=k;
  j=i==0?1:0;for(int k=0;k<=K;++k)if(k!=i && q[k]>q[j])j=k;
  l=0;while(l==i || l==j)++l;
  for(int k=0;k<=K;++k)if(k!=i && k!=j && q[k]>q[l])l=k;
  const double ds=1-s,dx=target_x-sx,dh=target_h-sh;
  const double det=(j-i)*(h[l]-h[i])-(l-i)*(h[j]-h[i]);
  if(det==0 || !std::isfinite(det))return false;
  const double dj=((dx-i*ds)*(h[l]-h[i])-(dh-h[i]*ds)*(l-i))/det;
  const double dl=((j-i)*(dh-h[i]*ds)-(h[j]-h[i])*(dx-i*ds))/det;
  q[i]+=ds-dj-dl;q[j]+=dj;q[l]+=dl;
  double entropy=0,cs=0,cx=0,ch=0;
  for(int k=0;k<=K;++k) {
    if(q[k]<0 || !std::isfinite(q[k]))return false;
    if(q[k]>0)entropy+=q[k]*std::log(q[k]);
    cs+=q[k];cx+=k*q[k];ch+=h[k]*q[k];
  }
  if(std::max({std::abs(cs-1),std::abs(cx-target_x),std::abs(ch-target_h)})>1e-11*(1+std::abs(target_h)))return false;
  upper=n*entropy+1e-9*(1+n*std::abs(entropy));
  const double candidate=n*(a*target_x+b*target_h-peak-std::log(z));
  return std::isfinite(upper) && std::isfinite(candidate) &&
    upper>=candidate-1e-9*(1+std::abs(candidate));
}
// Preserve the internal helper's existing call contract for diagnostics.
inline bool truncated_upper(ispan x,int K,double& upper,std::uint64_t& work,
                            std::uint64_t budget,int N=0) {
  StopDetail evidence;
  return truncated_upper(x,K,upper,work,budget,evidence,N);
}


inline bool fixed_profile_upper(ispan x,int N,const CmbResult& fit,bool fixed,
                                double& upper) {
  if(N==1) {upper=fit.logLik;return std::isfinite(upper);}
  const double eta=std::log(fit.prob)-std::log1p(-fit.prob);
  const Table tab(N,eta,fit.nu);
  std::vector<double> q(N+1);
  double sx=0,sh=0,s=0,mx=0,mh=0;
  for(int v:x){sx+=v;sh+=tab.h[v];}
  sx/=double(x.size());sh/=double(x.size());
  for(int k=0;k<=N;++k){q[k]=std::exp(tab.lp[k]);s+=q[k];mx+=k*q[k];mh+=tab.h[k]*q[k];}
  int i=0;for(int k=1;k<=N;++k)if(q[k]>q[i])i=k;
  int j=-1;
  for(int k=0;k<=N;++k)if(k!=i && (!fixed || tab.h[k]!=tab.h[i]) && (j<0 || q[k]>q[j]))j=k;
  if(j<0)return false;
  const double ds=1-s,dh=sh-mh;
  if(fixed) {
    const double dj=(dh-tab.h[i]*ds)/(tab.h[j]-tab.h[i]);
    q[i]+=ds-dj;q[j]+=dj;
  } else {
    int l=-1;
    for(int k=0;k<=N;++k)if(k!=i && k!=j && (l<0 || q[k]>q[l]))l=k;
    if(l<0)return false;
    const double det=(j-i)*(tab.h[l]-tab.h[i])-(l-i)*(tab.h[j]-tab.h[i]);
    if(det==0 || !std::isfinite(det))return false;
    const double dx=sx-mx;
    const double dj=((dx-i*ds)*(tab.h[l]-tab.h[i])-(dh-tab.h[i]*ds)*(l-i))/det;
    const double dl=((j-i)*(dh-tab.h[i]*ds)-(tab.h[j]-tab.h[i])*(dx-i*ds))/det;
    q[i]+=ds-dj-dl;q[j]+=dj;q[l]+=dl;
  }
  double entropy=0,cs=0,cx=0,ch=0;
  for(int k=0;k<=N;++k) {
    if(q[k]<0 || !std::isfinite(q[k]))return false;
    if(q[k]>0)entropy+=q[k]*std::log(q[k]);
    cs+=q[k];cx+=k*q[k];ch+=tab.h[k]*q[k];
  }
  const double residual=std::max({std::abs(cs-1),std::abs(ch-sh),fixed?0.:std::abs(cx-sx)});
  if(residual>1e-11*(1+std::abs(sh)+std::abs(sx)))return false;
  const double correction=fixed?eta*(sx-cx):0;
  upper=double(x.size())*(entropy+correction);
  upper+=1e-9*(1+double(x.size())*(std::abs(entropy)+std::abs(correction)));
  return std::isfinite(upper);
}
}

inline CmbSizeResult cmb_size_adjacent_fixed_prob(ispan x,int low,double prob,
                                                  double tol,int maxit) {
  maxit = static_cast<int>(solver_budget(maxit));
  const double nan=std::numeric_limits<double>::quiet_NaN();
  StopDetail evidence;
  const auto fail=[&](CmbStatus s) {
    CmbSizeResult out{nan,{nan,nan,nan,0,s}};
    out.fit.stop_detail=evidence;
    if(s==CmbStatus::FaceTie || s==CmbStatus::FaceUnresolved) out.fit.stop_detail.loglik_upper=INFINITY;
    return out;
  };
  const int mx=low+1;
  if(static_cast<std::uint64_t>(mx)>static_cast<std::uint64_t>(enumeration_budget(maxit))){evidence=discrete_budget(evidence);return fail(CmbStatus::IterationFailure);}
  const double n=double(x.size()),eta=std::log(prob)-std::log1p(-prob);
  double count_low=0,count_high=0;
  for(int v:x){if(v==low)++count_low;else ++count_high;}
  const double mean=low+count_high/n;
  const double logz=std::max(0.,eta)+std::log1p(std::exp(-std::abs(eta)));
  const double face=count_high*eta-n*logz;
  evidence=discrete_two_point(x,low,low+1,prob,eta);
  const std::uint64_t budget=enumeration_budget(maxit);
  std::uint64_t work=0;
  CmbSizeResult best{nan,{nan,nan,-INFINITY,0,CmbStatus::IterationFailure}};
  const auto evaluate=[&](int N) {
    const auto f=cmb_detail::size_candidate_rescue(x,N,prob,tol,maxit,work,budget);
    if(f.status==CmbStatus::AdjacentLimit || f.status==CmbStatus::EndpointLimit)return true;
    if(f.stop_detail.budget_exhausted) {
      evidence=discrete_budget(evidence);
      evidence.candidates.insert(evidence.candidates.end(),f.stop_detail.candidates.begin(),f.stop_detail.candidates.end());
      if(std::isfinite(f.stop_detail.loglik_lower) && (!std::isfinite(evidence.loglik_lower)||f.stop_detail.loglik_lower>evidence.loglik_lower))evidence.loglik_lower=f.stop_detail.loglik_lower;
    }
    if(f.status!=CmbStatus::Ok || !std::isfinite(f.logLik))return false;
    if(std::size_t(N)+1>budget-work){evidence=discrete_budget(evidence);return false;}
    work+=std::size_t(N)+1;
    double upper=0;
    if(!cmb_detail::fixed_profile_upper(x,N,f,true,upper) ||
       std::abs(upper-f.logLik)>1e-8*(1+std::abs(f.logLik)))return false;
    if(f.logLik>best.fit.logLik) {
      best={double(N),f};
      evidence.candidates.erase(std::remove_if(evidence.candidates.begin(),evidence.candidates.end(),
        [](const FitCandidate& c){return c.model==StopModel::None;}),evidence.candidates.end());
      evidence.candidates.push_back({{double(N),f.prob,f.nu},f.logLik,f.logLik});
      evidence.loglik_lower=f.logLik;
    }
    return true;
  };
  const int initial=mx+32;
  for(int N=mx;N<=initial;++N)if(!evaluate(N))return fail(CmbStatus::IterationFailure);
  if(!std::isfinite(best.fit.logLik))return fail(CmbStatus::FaceUnresolved);
  const double margin=1e-8*(1+std::abs(best.fit.logLik));
  if(low>0 && 2*low+1<=10000000 && face>=best.fit.logLik-margin)
    return fail(std::abs(face-best.fit.logLik)<=margin?CmbStatus::FaceTie:CmbStatus::FaceUnresolved);
  const double B=best.fit.logLik-margin;
  // For a fixed log probability ratio d, the best possible two-outcome
  // likelihood assigns no mass elsewhere. Its superlevel interval bounds
  // d=eta+nu*log((N-low)/(low+1)) for every feasible CMB distribution.
  const auto binary_ll=[&](double d) {
    return -count_low*std::max(0.,d)+count_high*std::min(0.,d)
      -n*std::log1p(std::exp(-std::abs(d)));
  };
  const double center=std::log(count_high/count_low);
  double step=1,lo=center-1,hi=center+1;
  for(int j=0;j<maxit && binary_ll(lo)>=B;++j){step*=2;lo=center-step;}
  step=1;for(int j=0;j<maxit && binary_ll(hi)>=B;++j){step*=2;hi=center+step;}
  if(!(binary_ll(lo)<B && binary_ll(hi)<B && binary_ll(center)>=B))return fail(CmbStatus::IterationFailure);
  double inside=center;
  for(int j=0;j<maxit;++j){const double mid=(lo+inside)/2;if(binary_ll(mid)>=B)inside=mid;else lo=mid;}
  inside=center;
  for(int j=0;j<maxit;++j){const double mid=(inside+hi)/2;if(binary_ll(mid)>=B)inside=mid;else hi=mid;}
  const double C=std::max(std::abs(lo-eta),std::abs(hi-eta))+1e-9*(1+std::abs(lo)+std::abs(hi)+std::abs(eta));
  for(int K=std::max(mx+1,16);K<=enumeration_budget(maxit);K*=2) {
    if((static_cast<std::uint64_t>(K)+1)*2*(static_cast<std::uint64_t>(maxit)+3)>budget-work){evidence=discrete_budget(evidence);return fail(CmbStatus::IterationFailure);}
    work+=(static_cast<std::uint64_t>(K)+1)*2*(static_cast<std::uint64_t>(maxit)+3);
    const auto geometric=[&](double a,double& m) {
      double peak=std::max(0.,K*a),z=0,s=0;
      for(int k=0;k<=K;++k){const double v=std::exp(k*a-peak);z+=v;s+=k*v;}
      m=s/z;return n*(mean*a-peak-std::log(z));
    };
    double lo=-64,hi=64,ml=0,mh=0;
    geometric(lo,ml);geometric(hi,mh);
    if(!(ml<mean && mh>mean))return fail(CmbStatus::IterationFailure);
    for(int j=0;j<maxit;++j){double m=0,mid=(lo+hi)/2;geometric(mid,m);if(m<mean)lo=mid;else hi=mid;}
    double m=0;const double trunc=geometric((lo+hi)/2,m)+n*K*(hi-lo)+margin;
    if(trunc>=B)continue;
    const auto bound=[&](int N) {
      const double limit=C/std::log(double(N-low)/double(low+1));
      double ck=0;for(int j=0;j<K;++j)ck-=std::log1p(-double(j)/N);
      return trunc+n*limit*(std::lgamma(K+1.)+ck);
    };
    int left=std::max({K,initial+1,2*low+2}),right=left;
    for(std::int64_t bracket_pass=0; bracket_pass<solver_budget(maxit) && (bound(right)>=B && right<10000000); ++bracket_pass)right=right>5000000?10000000:2*right;
    if(bound(right)>=B)return fail(CmbStatus::IterationFailure);
    for(std::int64_t bracket_pass=0; bracket_pass<solver_budget(maxit) && (right-left>1); ++bracket_pass){const int mid=left+(right-left)/2;if(bound(mid)<B)right=mid;else left=mid;}
    if(right-left>1)return fail(CmbStatus::IterationFailure);
    for(int N=initial+1;N<right;++N)if(!evaluate(N))return fail(CmbStatus::IterationFailure);
    return best;
  }
  return fail(CmbStatus::FaceUnresolved);
}

inline CmbSizeResult cmb_size_nonconstant_free_nu(ispan x,int mn,int mx,
    double prob,double tol,int maxit) {
  maxit = static_cast<int>(solver_budget(maxit));
  const double nan=std::numeric_limits<double>::quiet_NaN();
  StopDetail evidence;
  const auto fail=[&](CmbStatus s) {
    CmbSizeResult out{nan,{nan,nan,nan,0,s}};
    out.fit.stop_detail=evidence;
    if(s==CmbStatus::FaceTie || s==CmbStatus::FaceUnresolved) out.fit.stop_detail.loglik_upper=INFINITY;
    return out;
  };
  const bool fixed=!std::isnan(prob);
  const double n=double(x.size()),eta=fixed?std::log(prob)-std::log1p(-prob):0;
  std::size_t count_u=0,count_v=0;
  for(int v:x){if(v==mn)++count_u;if(v==mx)++count_v;}
  const bool two=count_u+count_v==x.size();
  // The adjacent case has unbounded positive dispersion on every relevant
  // superlevel set; it needs its separate two-outcome tail analysis.
  if(mx-mn<2)return fixed?cmb_size_adjacent_fixed_prob(x,mn,prob,tol,maxit):fail(CmbStatus::FaceUnresolved);
  const std::uint64_t budget=enumeration_budget(maxit);
  std::uint64_t work=0;
  CmbSizeResult best{nan,{nan,nan,-INFINITY,0,CmbStatus::IterationFailure}};
  double face=-INFINITY;
  if(fixed && two && mn==0) {
    const double z=mx*eta;
    face=double(count_v)*z-n*std::max(0.,z)-n*std::log1p(std::exp(-std::abs(z)));
    evidence=discrete_two_point(x,0,mx,cmb_detail::probability(z),z);
  }
  const auto evaluate=[&](int N) {
    const auto f=cmb_detail::size_candidate_rescue(x,N,prob,tol,maxit,work,budget);
    if(f.status==CmbStatus::EndpointLimit || f.status==CmbStatus::AdjacentLimit || f.status==CmbStatus::PointMass)
      return true; // The only possible face here is the endpoint above.
    if(f.stop_detail.budget_exhausted) {
      evidence=discrete_budget(evidence);
      evidence.candidates.insert(evidence.candidates.end(),f.stop_detail.candidates.begin(),f.stop_detail.candidates.end());
      if(std::isfinite(f.stop_detail.loglik_lower) && (!std::isfinite(evidence.loglik_lower)||f.stop_detail.loglik_lower>evidence.loglik_lower))evidence.loglik_lower=f.stop_detail.loglik_lower;
    }
    if(f.status!=CmbStatus::Ok || !std::isfinite(f.logLik))return false;
    if(std::size_t(N)+1>budget-work){evidence=discrete_budget(evidence);return false;}
    work+=std::size_t(N)+1;
    double upper=0;
    if(!cmb_detail::fixed_profile_upper(x,N,f,fixed,upper) ||
       std::abs(upper-f.logLik)>1e-8*(1+std::abs(f.logLik)))return false;
    if(f.logLik>best.fit.logLik) {
      best={double(N),f};
      evidence.candidates.erase(std::remove_if(evidence.candidates.begin(),evidence.candidates.end(),
        [](const FitCandidate& c){return c.model==StopModel::None;}),evidence.candidates.end());
      evidence.candidates.push_back({{double(N),f.prob,f.nu},f.logLik,f.logLik});
      evidence.loglik_lower=f.logLik;
    }
    return true;
  };
  if(static_cast<std::uint64_t>(mx)>static_cast<std::uint64_t>(enumeration_budget(maxit))){evidence=discrete_budget(evidence);return fail(CmbStatus::IterationFailure);}
  const int initial=std::min(10000000,mx+32);
  for(int N=mx;N<=initial;++N)if(!evaluate(N))return fail(CmbStatus::IterationFailure);
  if(!std::isfinite(best.fit.logLik))return fail(CmbStatus::FaceUnresolved);
  const double margin=1e-8*(1+std::abs(best.fit.logLik));
  if(!fixed) {
    std::vector<double> data;data.reserve(x.size());
    for(int v:x)data.push_back(double(v));
    CmpFitControl ctl;ctl.tol=1e-10;ctl.maxit=maxit;
    CmpWorkLedger ledger{work,budget};
    {
      const auto cmp=cmp_mle({data.data(),data.size()},ctl,ledger);
      if(cmp.status==CmpStatus::BudgetLimit) {
        evidence=discrete_budget(evidence);
        for(auto candidate:cmp.stop_detail.candidates) {
          candidate.model=StopModel::Cmp;evidence.candidates.push_back(candidate);
        }
        return fail(CmbStatus::IterationFailure);
      }
      if(cmp.status==CmpStatus::NumericalFailure || cmp.status==CmpStatus::InvalidInput ||
         cmp.status==CmpStatus::IterationFailure)return fail(CmbStatus::IterationFailure);
      if(cmp.status==CmpStatus::Ok && std::isfinite(cmp.logLik) && cmp.logLik>face) {
        face=cmp.logLik;
        evidence.limit_model=StopModel::Cmp;evidence.limit_coef={std::exp(cmp.eta),cmp.nu};evidence.limit_loglik=face;
        evidence.candidates.push_back({evidence.limit_coef,face,face,StopModel::Cmp});
      }
    }
  }
  if(face>=best.fit.logLik-margin)
    return fail(std::abs(face-best.fit.logLik)<=margin?CmbStatus::FaceTie:CmbStatus::FaceUnresolved);
  const double B=best.fit.logLik-margin;
  const double w=double(mx-mn-1)/double(mx-mn);
  const double left=w*std::lgamma(mn+1.),right=(1-w)*std::lgamma(mx+1.),middle=std::lgamma(mn+2.);
  const double delta=left+right-middle-1e-10*(1+std::abs(left)+std::abs(right)+std::abs(middle));
  if(!(delta>0))return fail(CmbStatus::IterationFailure);
  const double lower_u=cmb_detail::observed_log_lower(x,mn,B,maxit);
  const double lower_v=cmb_detail::observed_log_lower(x,mx,B,maxit);
  if(!std::isfinite(lower_u) || !std::isfinite(lower_v))return fail(CmbStatus::IterationFailure);
  const double remainder=std::min(1.,1-std::exp(lower_u)-std::exp(lower_v)+1e-9);
  if(!(remainder>0))return fail(CmbStatus::IterationFailure);
  const double upper_j=std::min(std::log(remainder),cmb_detail::observed_log_upper(x,mn+1,B,maxit));
  const double raw_positive=(upper_j-w*lower_u-(1-w)*lower_v)/delta;
  if(!std::isfinite(raw_positive))return fail(CmbStatus::IterationFailure);
  const double positive=std::max(0.,raw_positive);
  if(best.fit.nu>positive+1e-8*(1+positive))return fail(CmbStatus::IterationFailure);
  for(int K=std::max(mx+1,8);K<=enumeration_budget(maxit);K*=2) {
    double trunc=0;
    if(!cmb_detail::truncated_upper(x,K,trunc,work,budget,evidence,0,maxit))return fail(CmbStatus::IterationFailure);
    if(trunc>=B)continue;
    const double negative=-lower_v/std::log(double(K));
    const auto bound=[&](int N) {
      double ck=0,s=0;
      for(int j=0;j<K;++j)ck-=std::log1p(-double(j)/N);
      for(int v:x)for(int j=0;j<v;++j)s-=std::log1p(-double(j)/N);
      return trunc+std::max(positive*n*ck,negative*s);
    };
    int lo=std::max(K,initial+1),hi=lo;
    for(std::int64_t bracket_pass=0; bracket_pass<solver_budget(maxit) && (bound(hi)>=B && hi<10000000); ++bracket_pass)hi=hi>5000000?10000000:hi*2;
    if(bound(hi)>=B)return fail(CmbStatus::IterationFailure);
    for(std::int64_t bracket_pass=0; bracket_pass<solver_budget(maxit) && (hi-lo>1); ++bracket_pass){int mid=lo+(hi-lo)/2;if(bound(mid)<B)hi=mid;else lo=mid;}
    if(hi-lo>1)return fail(CmbStatus::IterationFailure);
    // For a block [L,R], retain 0..K and use the entropy dual at L.
    // c_R(k)-c_L(k) is nonnegative and increases with k. Relaxing
    // the odds even when fixed gives an upper bound for both subsets.
    std::vector<std::pair<int,int>> blocks;
    if(initial+1<hi)blocks.push_back({initial+1,hi-1});
    while(!blocks.empty()) {
      const int L=blocks.back().first,R=blocks.back().second;
      blocks.pop_back();
      if(L<K) {
        if(!evaluate(L))return fail(CmbStatus::IterationFailure);
        if(L<R)blocks.push_back({L+1,R});
        continue;
      }
      double anchor=0;
      if(!cmb_detail::truncated_upper(x,K,anchor,work,budget,evidence,L,maxit))
        return fail(CmbStatus::IterationFailure);
      if(std::size_t(K+1)>budget-work){evidence=discrete_budget(evidence);return fail(CmbStatus::IterationFailure);}
      work+=std::size_t(K+1);
      double d=0,sum=0;
      std::vector<double> diff(K+1,0.);
      for(int k=1;k<=K;++k) {
        d+=std::log1p(-double(k-1)/R)-std::log1p(-double(k-1)/L);
        diff[k]=d;
      }
      for(int v:x)sum+=diff[v];
      const double extra=std::max(positive*sum,negative*(n*d-sum));
      // Allow for rounding in the block correction and comparison.
      const double block_upper=anchor+extra+1e-9*(1+std::abs(anchor)+std::abs(extra));
      if(block_upper<B)continue;
      if(L==R) {
        if(!evaluate(L))return fail(CmbStatus::IterationFailure);
      } else {
        const int mid=L+(R-L)/2;
        blocks.push_back({mid+1,R});blocks.push_back({L,mid});
      }
    }
    return best;
  }
  return fail(CmbStatus::FaceUnresolved);
}

// Constant samples have an exact solution (or point-mass supremum). Other
// free-dispersion samples need the global size and face comparison.
inline CmbSizeResult cmb_size_free_nu_mle(ispan x,
    double prob=std::numeric_limits<double>::quiet_NaN(),
    double tol=1e-10,int maxit=2000) {
  maxit = static_cast<int>(solver_budget(maxit));
  const double nan=std::numeric_limits<double>::quiet_NaN();
  StopDetail evidence;
  const auto fail=[&](CmbStatus s) {
    CmbSizeResult out{nan,{nan,nan,nan,0,s}};
    out.fit.stop_detail=evidence;
    if(s==CmbStatus::FaceTie || s==CmbStatus::FaceUnresolved) out.fit.stop_detail.loglik_upper=INFINITY;
    return out;
  };
  const bool fixed=!std::isnan(prob);
  if(x.size()==0 || !(tol>0) || !std::isfinite(tol) || maxit<1 ||
     (fixed && !(prob>=0 && prob<=1)))return fail(CmbStatus::InvalidInput);
  int mn=10000001,mx=0;
  for(int v:x) {
    if(v<0 || v>10000000)return fail(CmbStatus::InvalidInput);
    mn=std::min(mn,v);mx=std::max(mx,v);
  }
  if(fixed && prob==0 && mx>0)return fail(CmbStatus::InvalidInput);
  if(fixed && prob==1 && mn!=mx)return fail(CmbStatus::InvalidInput);
  if(mn!=mx) {
    // Uniform counts on the entire support attain the saturated likelihood
    // at size mx, probability 1/2 and zero dispersion.
    if(mn==0 && (!fixed || prob==.5) &&
       x.size()%std::size_t(mx+1)==0) {
      std::vector<std::size_t> counts(std::size_t(mx)+1,0);
      for(int v:x)++counts[std::size_t(v)];
      bool uniform=true;
      for(std::size_t c:counts)uniform=uniform && c==counts[0];
      if(uniform) {
        if(mx==1 && !fixed){evidence=discrete_two_point(x,0,1);return fail(CmbStatus::FaceTie);}
        return {double(mx),{.5,mx==1?1.:0.,-double(x.size())*std::log(double(mx)+1),0,CmbStatus::Ok}};
      }
    }
    if(fixed && prob==.5 && mn==0 && mx==1) {
      double zero=0,one=0;
      for(int v:x){if(v==0)++zero;else ++one;}
      const double n=zero+one;
      const double ll1=-n*std::log(2.);
      const double ll2=zero*std::log(zero/(2*n))+one*std::log(one/n);
      if(std::abs(ll2-ll1)<=1e-12*(1+std::max(std::abs(ll1),std::abs(ll2))))
        return fail(CmbStatus::IterationFailure);
      if(ll2>ll1)
        return {2.,{.5,std::log(2*one/zero)/std::log(2.),ll2,0,CmbStatus::Ok}};
      return {1.,{.5,1.,ll1,0,CmbStatus::Ok}};
    }
    // With odds one, equally frequent observations on the two central
    // points of an odd design reach their saturated likelihood on its
    // positive-dispersion face. Likewise, two equally frequent endpoints
    // reach it on the negative-dispersion face. The size-one Bernoulli is
    // an attained exception.
    if(fixed && prob==.5) {
      std::size_t low=0,high=0;
      for(int v:x) {if(v==mn)++low;else if(v==mx)++high;}
      if(low==high && low+high==x.size()) {
        if(mn==0 && mx==1)
          return {1.,{prob,1.,-double(x.size())*std::log(2.),0,CmbStatus::Ok}};
        if(mx-mn==1 && mx<=5000000){evidence=discrete_two_point(x,mn,mx,.5);return fail(CmbStatus::AdjacentLimit);}
        if(mn==0){evidence=discrete_two_point(x,0,mx,.5);return fail(CmbStatus::EndpointLimit);}
      }
    }
    // The empirical two-point likelihood bounds every model. For {0,1}
    // the size-one Bernoulli member ties the CMP adjacent face; for {k,k+1},
    // k>0, the adjacent face at size k+1 approaches it but no finite
    // member can attain it, since its support contains additional points.
    if(!fixed && mx-mn==1) {
      evidence=discrete_two_point(x,mn,mx);
      if(mn>0)return fail(CmbStatus::AdjacentLimit);
      evidence.candidates.push_back({{1,evidence.limit_coef[1],1},evidence.limit_loglik,evidence.limit_loglik});
      return fail(CmbStatus::FaceTie);
    }
    if(!fixed && mn==0) {
      bool endpoints=true;
      for(int v:x)endpoints=endpoints && (v==0 || v==mx);
      // At size mx the endpoint face matches the empirical proportions;
      // all finite members have positive mass at an unobserved interior.
      if(endpoints){evidence=discrete_two_point(x,0,mx);return fail(CmbStatus::EndpointLimit);}
    }
    return cmb_size_nonconstant_free_nu(x,mn,mx,prob,tol,maxit);
  }
  if(!fixed || mx==0 || prob==1)
    return {double(mx),{fixed?prob:(mx==0?0.:1.),1.,0.,0,CmbStatus::Ok}};
  // If 2*mx is in the supported size range, nu -> +infinity at size
  // 2*mx concentrates on mx. No finite interior-probability member does.
  if(mx<=5000000){evidence=discrete_point(mx);return fail(CmbStatus::PointMass);}
  return fail(CmbStatus::NotImplemented);
}

// Free probability and fixed dispersion. All finite-size profiles are checked;
// the truncation comparison below bounds every unvisited size simultaneously.
inline CmbSizeResult cmb_size_fixed_nu_mle(ispan x,double nu,
                                           double tol=1e-10,int maxit=2000) {
  maxit = static_cast<int>(solver_budget(maxit));
  const double nan=std::numeric_limits<double>::quiet_NaN();
  StopDetail evidence;
  const auto fail=[&](CmbStatus s) {
    CmbSizeResult out{nan,{nan,nu,nan,0,s}};
    out.fit.stop_detail=evidence;
    if(s==CmbStatus::FaceTie || s==CmbStatus::FaceUnresolved) out.fit.stop_detail.loglik_upper=INFINITY;
    return out;
  };
  if(x.size()==0 || !std::isfinite(nu) || !(tol>0) || !std::isfinite(tol) || maxit<1)
    return fail(CmbStatus::InvalidInput);
  int mn=10000001,mx=0; double sx=0,sh=0;
  for(int v:x) {
    if(v<0 || v>10000000)return fail(CmbStatus::InvalidInput);
    mn=std::min(mn,v);mx=std::max(mx,v);sx+=v;sh+=std::lgamma(v+1.0);
  }
  if(mn==mx) return {double(mx),{mx==0?0.:1.,nu,0.,0,CmbStatus::Ok}};
  const double n=double(x.size()),mean=sx/n;
  const std::uint64_t budget=enumeration_budget(maxit);
  std::uint64_t work=0;
  CmbSizeResult best{nan,{nan,nu,-INFINITY,0,CmbStatus::IterationFailure}};
  const auto evaluate=[&](int N) {
    if(N>10000000)return false;
    const auto f=cmb_mle_work(x,N,tol,maxit,nan,nu,work,budget);
    if(f.stop_detail.budget_exhausted) {
      evidence=discrete_budget(evidence);
      evidence.candidates.insert(evidence.candidates.end(),f.stop_detail.candidates.begin(),f.stop_detail.candidates.end());
      if(std::isfinite(f.stop_detail.loglik_lower) && (!std::isfinite(evidence.loglik_lower)||f.stop_detail.loglik_lower>evidence.loglik_lower))evidence.loglik_lower=f.stop_detail.loglik_lower;
    }
    if(f.status!=CmbStatus::Ok || !std::isfinite(f.logLik))return false;
    if(f.logLik>best.fit.logLik) {
      best={double(N),f};
      evidence.candidates.erase(std::remove_if(evidence.candidates.begin(),evidence.candidates.end(),
        [](const FitCandidate& c){return c.model==StopModel::None;}),evidence.candidates.end());
      evidence.candidates.push_back({{double(N),f.prob,f.nu},f.logLik,f.logLik});
      evidence.loglik_lower=f.logLik;
    }
    return true;
  };
  if(!evaluate(mx))return fail(CmbStatus::IterationFailure);
  if(nu==0)return best;
  // The endpoint terms imply L_N <= prod choose(N,x_i)^nu for nu<0.
  if(nu<0) {
    for(int N=mx+1;N<=10000000;++N) {
      double upper=0;
      for(int v:x)upper+=nu*cmb_detail::logchoose(N,v);
      if(upper<best.fit.logLik-1e-9*(1+std::abs(best.fit.logLik)))return best;
      if(!evaluate(N))return fail(CmbStatus::IterationFailure);
    }
    return fail(CmbStatus::IterationFailure);
  }
  CmpFitControl ctl; ctl.fixed_nu=nu; ctl.tol=1e-12; ctl.maxit=maxit;
  CmpWorkLedger ledger{work,budget};
  double eta=0; std::uint64_t face_work=0;
  const CmpMoments face_mom=cmp_detail::mean_match(mean,nu,eta,ctl,face_work,ledger);
  if(face_mom.status==CmpStatus::BudgetLimit)evidence=discrete_budget(evidence);
  if(face_mom.status!=CmpStatus::Ok || !std::isfinite(face_mom.log_z))
    return fail(CmbStatus::IterationFailure);
  const double face=sx*eta-n*face_mom.log_z-nu*sh;
  evidence.limit_model=StopModel::Cmp;evidence.limit_coef={std::exp(eta),nu};evidence.limit_loglik=face;
  evidence.candidates.push_back({evidence.limit_coef,face,face,StopModel::Cmp});
  if(!std::isfinite(face))return fail(CmbStatus::IterationFailure);
  const double margin=1e-9*(1+std::abs(face));
  bool tie=std::abs(best.fit.logLik-face)<=margin;
  for(int N=mx+1;N<=mx+255;++N) {
    if(!evaluate(N))return fail(CmbStatus::IterationFailure);
    tie=tie || std::abs(best.fit.logLik-face)<=margin;
  }
  if(best.fit.logLik<=face+margin)
    return fail(tie?CmbStatus::FaceTie:CmbStatus::FaceUnresolved);
  // Optimize the truncated CMP likelihood. Mean matching is monotone; the
  // bracket supplies a numerical upper allowance for its remaining width.
  int K=std::max(mx+1,32);
  double truncated=INFINITY;
  for(;;K*=2) {
    const std::uint64_t cost=(static_cast<std::uint64_t>(K)+1)*2*(static_cast<std::uint64_t>(maxit)+3);
    if(cost>budget-work){evidence=discrete_budget(evidence);return fail(CmbStatus::IterationFailure);}
    work+=cost;
    double lo=-64,hi=64;
    const auto objective=[&](double a,double& m) {
      double top=-INFINITY;
      for(int k=0;k<=K;++k)top=std::max(top,k*a-nu*std::lgamma(k+1.));
      double z=0,s=0;
      for(int k=0;k<=K;++k) {const double w=std::exp(k*a-nu*std::lgamma(k+1.)-top);z+=w;s+=k*w;}
      m=s/z; return sx*a-n*(top+std::log(z))-nu*sh;
    };
    double ml=0,mh=0; objective(lo,ml);objective(hi,mh);
    if(!(ml<mean && mh>mean))return fail(CmbStatus::IterationFailure);
    for(int i=0;i<maxit;++i) {double m=0;const double mid=(lo+hi)/2;objective(mid,m);if(m<mean)lo=mid;else hi=mid;}
    double m=0;truncated=objective((lo+hi)/2,m)+n*K*(hi-lo)+margin;
    if(truncated<best.fit.logLik-margin)break;
  }
  if(K>10000000)return fail(CmbStatus::IterationFailure);
  const auto bound=[&](int N) {
    double ck=0,s=0;
    for(int j=0;j<K;++j)ck+=std::log1p(-double(j)/N);
    for(int v:x)for(int j=0;j<v;++j)s+=std::log1p(-double(j)/N);
    return truncated+nu*(s-n*ck);
  };
  int hi=std::max(K,mx+256);
  for(std::int64_t bracket_pass=0; bracket_pass<solver_budget(maxit) && (bound(hi)>=best.fit.logLik-margin); ++bracket_pass) {
    if(hi>5000000)return fail(CmbStatus::IterationFailure);
    hi*=2;
  }
  if(!(bound(hi)<best.fit.logLik-margin))return fail(CmbStatus::IterationFailure);
  int lo=std::max(K,mx+256);
  for(std::int64_t bracket_pass=0; bracket_pass<solver_budget(maxit) && (hi-lo>1); ++bracket_pass) {const int mid=lo+(hi-lo)/2;if(bound(mid)<best.fit.logLik-margin)hi=mid;else lo=mid;}
  if(hi-lo>1)return fail(CmbStatus::IterationFailure);
  // No feasible size is skipped, including sizes below K.
  for(int N=mx+256;N<hi;++N)if(!evaluate(N))return fail(CmbStatus::IterationFailure);
  return best;
}
// Both canonical coordinates held. Enumerate all integers, and bound the
// entire remaining tail by a single normalizer term. This does not assume
// unimodality in size (which is not implied by exponential-family concavity).
inline CmbSizeResult cmb_size_mle(ispan x,double prob,double nu,
                                 double tol=1e-10,int maxit=2000) {
  maxit = static_cast<int>(solver_budget(maxit));
  const double nan=std::numeric_limits<double>::quiet_NaN();
  StopDetail evidence;
  const auto fail=[&](CmbStatus s) {CmbSizeResult out{nan,{prob,nu,nan,0,s}};out.fit.stop_detail=evidence;return out;};
  if(x.size()==0 || !std::isfinite(prob)||prob<0||prob>1||!std::isfinite(nu)||
     !(tol>0)||!std::isfinite(tol)||maxit<1)return fail(CmbStatus::InvalidInput);
  int mx=0;double sx=0;
  for(int v:x){if(v<0)return fail(CmbStatus::InvalidInput);mx=std::max(mx,v);sx+=v;}
  if(prob==0 || prob==1) {
    for(int v:x)if(v!=(prob==0 ? 0 : mx))return fail(CmbStatus::InvalidInput);
    return {double(mx),{prob,nu,0,0,CmbStatus::Ok}};
  }
  if(mx==0)return {0,{prob,nu,0,0,CmbStatus::Ok}};
  const double eta=std::log(prob)-std::log1p(-prob),n=double(x.size());
  CmbSizeResult best{nan,{prob,nu,-INFINITY,0,CmbStatus::IterationFailure}};
  // Work budget counts enumerated normalizer terms, not size magnitude.
  // Table also computes moments and tails, so charge its full support.
  std::uint64_t work=0;
  for(int N=mx;;++N) {
    const auto f=cmb_mle_work(x,N,tol,maxit,prob,nu,work,enumeration_budget(maxit));
    if(f.stop_detail.budget_exhausted){
      evidence=discrete_budget(evidence);
      evidence.candidates.insert(evidence.candidates.end(),f.stop_detail.candidates.begin(),f.stop_detail.candidates.end());
      return fail(CmbStatus::IterationFailure);
    }
    if(f.status!=CmbStatus::Ok)return {double(N),f};
    if(!std::isfinite(f.logLik))return fail(CmbStatus::IterationFailure);
    if(f.logLik>best.fit.logLik) {
      best={double(N),f};
      evidence.candidates.erase(std::remove_if(evidence.candidates.begin(),evidence.candidates.end(),
        [](const FitCandidate& c){return c.model==StopModel::None;}),evidence.candidates.end());
      evidence.candidates.push_back({{double(N),f.prob,f.nu},f.logLik,f.logLik});
      evidence.loglik_lower=f.logLik;
    }
    // nu=0: weights r^x are independent of size and the normalizer strictly
    // increases, so the first feasible size is the exact optimum.
    if(nu==0)return best;
    double upper=0;
    if(nu>0) {
      const double next=double(N)+1;
      const int k=mx+1;
      for(int v:x) {
        double ratio=0;
        for(int j=v;j<k;++j)ratio+=std::log((j+1)/(next-j));
        upper+=nu*ratio+(v-double(k))*eta;
      }
      // C(M,v)/C(M,k) decreases in M for v<k; one term of Z
      // gives this bound simultaneously for every M>=N+1.
    } else {
      // Z>=max(1,exp(M*eta)); every positive logchoose(M,v)
      // increases in M, while nu<0. Thus this bound also decreases.
      for(int v:x)upper+=nu*cmb_detail::logchoose(N+1,v);
      upper+=eta*sx-n*std::max(0.,(N+1.)*eta);
    }
    if(upper<best.fit.logLik-1e-10*n)return best;
  }
}
}
