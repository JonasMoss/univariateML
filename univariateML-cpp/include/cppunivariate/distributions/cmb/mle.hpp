// Full finite exponential family in (logit(prob), nu).
#pragma once
#include "dpqr.hpp"
#include "../../core/span.hpp"
#include "../../special/cmp.hpp"
#include <limits>
namespace cppunivariate {
enum class CmbStatus { Ok, InvalidInput, PointMass, AdjacentLimit, EndpointLimit,
                       Unidentified, IterationFailure, FaceUnresolved, FaceTie,
                       NotImplemented };
struct CmbResult { double prob,nu,logLik; int iterations; CmbStatus status; };
namespace cmb_detail {
inline double probability(double eta) {
  return eta>=0 ? 1/(1+std::exp(-eta)) : std::exp(eta)/(1+std::exp(eta));
}
// Mean matching has derivative Var(X). Safeguarded Newton within a bracket.
inline double match_mean(int N,double nu,double m) {
  double lo=-1,hi=1;
  for (int j=0;j<60 && Table(N,lo,nu).mean>m;++j) lo*=2;
  for (int j=0;j<60 && Table(N,hi,nu).mean<m;++j) hi*=2;
  double eta=0;
  for (int j=0;j<100;++j) {
    const Table tab(N,eta,nu);
    const double score=tab.mean-m;
    if (std::abs(score)<2e-14*(1+m)) return eta;
    if (score<0) lo=eta; else hi=eta;
    const double next=eta-score/tab.vx;
    eta=std::isfinite(next) && next>lo && next<hi ? next : lo+(hi-lo)/2;
  }
  return eta;
}
}
inline CmbResult cmb_mle(ispan x,int N,double tol=1e-10,int maxit=100,
                        double prob=std::numeric_limits<double>::quiet_NaN(),
                        double nu=std::numeric_limits<double>::quiet_NaN()) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const bool fp=!std::isnan(prob),fn=!std::isnan(nu);
  const auto fail=[&](CmbStatus status) { return CmbResult{nan,nan,nan,0,status}; };
  if (x.size()==0 || N<0 || N>10000000 || !(tol>0) || !std::isfinite(tol) || maxit<1 ||
      (fp && !(prob>=0 && prob<=1)) || (fn && !std::isfinite(nu))) return fail(CmbStatus::InvalidInput);
  int xmin=N,xmax=0; double sx=0,sh=0;
  bool endpoints=true,centres=true;
  for (int v:x) {
    if (v<0 || v>N) return fail(CmbStatus::InvalidInput);
    xmin=std::min(xmin,v); xmax=std::max(xmax,v); sx+=v; sh+=cmb_detail::logchoose(N,v);
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
      const double eta=fp ? fixed_eta : cmb_detail::match_mean(N,v,m);
      return cmb_detail::Table(N,eta,v).mh-t;
    };
    // Profile derivative Var(h)-Cov(X,h)^2/Var(X) is positive.
    double lo=-1,hi=1;
    for (int j=0;j<50 && score(lo)>0;++j) lo*=2;
    for (int j=0;j<50 && score(hi)<0;++j) hi*=2;
    if (!(score(lo)<=0 && score(hi)>=0)) return fail(CmbStatus::IterationFailure);
    for (;iterations<maxit;++iterations) {
      nu=lo+(hi-lo)/2;
      const double g=score(nu);
      if (std::abs(g)<tol*(1+std::abs(t))) break;
      if (g<0) lo=nu; else hi=nu;
    }
    if (iterations==maxit) return fail(CmbStatus::IterationFailure);
  } else if (!fn) nu=1;
  const double eta=fp ? fixed_eta : cmb_detail::match_mean(N,nu,m);
  const double fitted_prob=fp ? prob : cmb_detail::probability(eta);
  if (!(fitted_prob>0 && fitted_prob<1)) return fail(CmbStatus::IterationFailure);
  // Evaluate the coordinates actually returned to the caller, including
  // the rounding incurred by converting canonical log odds to probability.
  const double reported_eta=std::log(fitted_prob)-std::log1p(-fitted_prob);
  const auto tab=cmb_detail::Table(N,reported_eta,nu);
  if ((!fp && std::abs(tab.mean-m)>1e-10*(1+m)) ||
      (!fn && N>1 && std::abs(tab.mh-t)>tol*(1+std::abs(t))))
    return fail(CmbStatus::IterationFailure);
  double ll=0; for (int v:x) ll+=tab.lp[v];
  return {fitted_prob,nu,ll,iterations,CmbStatus::Ok};
}

struct CmbSizeResult { double size; CmbResult fit; };

// Constant samples have an exact solution (or point-mass supremum). Other
// free-dispersion samples need the global size and face comparison.
inline CmbSizeResult cmb_size_free_nu_mle(ispan x,
    double prob=std::numeric_limits<double>::quiet_NaN(),
    double tol=1e-10,int maxit=100) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const auto fail=[&](CmbStatus s) {return CmbSizeResult{nan,{nan,nan,nan,0,s}};};
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
  if(mn!=mx)return fail(CmbStatus::NotImplemented);
  if(!fixed || mx==0 || prob==1)
    return {double(mx),{fixed?prob:(mx==0?0.:1.),1.,0.,0,CmbStatus::Ok}};
  // If 2*mx is in the supported size range, nu -> +infinity at size
  // 2*mx concentrates on mx. No finite interior-probability member does.
  if(mx<=5000000)return fail(CmbStatus::PointMass);
  return fail(CmbStatus::NotImplemented);
}

// Free probability and fixed dispersion. All finite-size profiles are checked;
// the truncation comparison below bounds every unvisited size simultaneously.
inline CmbSizeResult cmb_size_fixed_nu_mle(ispan x,double nu,
                                           double tol=1e-10,int maxit=100) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const auto fail=[&](CmbStatus s) {return CmbSizeResult{nan,{nan,nu,nan,0,s}};};
  if(x.size()==0 || !std::isfinite(nu) || !(tol>0) || !std::isfinite(tol) || maxit<1)
    return fail(CmbStatus::InvalidInput);
  int mn=10000001,mx=0; double sx=0,sh=0;
  for(int v:x) {
    if(v<0 || v>10000000)return fail(CmbStatus::InvalidInput);
    mn=std::min(mn,v);mx=std::max(mx,v);sx+=v;sh+=std::lgamma(v+1.0);
  }
  if(mn==mx) return {double(mx),{mx==0?0.:1.,nu,0.,0,CmbStatus::Ok}};
  const double n=double(x.size()),mean=sx/n;
  const std::size_t budget=10000000;
  std::size_t work=0;
  CmbSizeResult best{nan,{nan,nu,-INFINITY,0,CmbStatus::IterationFailure}};
  const auto evaluate=[&](int N) {
    if(N>10000000 || std::size_t(N)+1>budget-work)return false;
    work+=std::size_t(N)+1;
    const auto f=cmb_mle(x,N,tol,maxit,nan,nu);
    if(f.status!=CmbStatus::Ok || !std::isfinite(f.logLik))return false;
    if(f.logLik>best.fit.logLik)best={double(N),f};
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
  ctl.max_terms=std::min(std::size_t(1000000),budget/std::size_t(maxit));
  if(ctl.max_terms<2)return fail(CmbStatus::IterationFailure);
  double eta=0; std::size_t face_work=0;
  const CmpMoments face_mom=cmp_detail::mean_match(mean,nu,eta,ctl,face_work);
  if(face_mom.status!=CmpStatus::Ok || !std::isfinite(face_mom.log_z) || face_work>budget-work)
    return fail(CmbStatus::IterationFailure);
  work+=face_work;
  const double face=sx*eta-n*face_mom.log_z-nu*sh;
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
  for(;K<=8192;K*=2) {
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
    for(int i=0;i<100;++i) {double m=0;const double mid=(lo+hi)/2;objective(mid,m);if(m<mean)lo=mid;else hi=mid;}
    double m=0;truncated=objective((lo+hi)/2,m)+n*K*(hi-lo)+margin;
    if(truncated<best.fit.logLik-margin)break;
  }
  if(K>8192 || K>10000000)return fail(CmbStatus::IterationFailure);
  const auto bound=[&](int N) {
    double ck=0,s=0;
    for(int j=0;j<K;++j)ck+=std::log1p(-double(j)/N);
    for(int v:x)for(int j=0;j<v;++j)s+=std::log1p(-double(j)/N);
    return truncated+nu*(s-n*ck);
  };
  int hi=std::max(K,mx+256);
  while(bound(hi)>=best.fit.logLik-margin) {
    if(hi>5000000)return fail(CmbStatus::IterationFailure);
    hi*=2;
  }
  int lo=std::max(K,mx+256);
  while(hi-lo>1) {const int mid=lo+(hi-lo)/2;if(bound(mid)<best.fit.logLik-margin)hi=mid;else lo=mid;}
  // No feasible size is skipped, including sizes below K.
  for(int N=mx+256;N<hi;++N)if(!evaluate(N))return fail(CmbStatus::IterationFailure);
  return best;
}
// Both canonical coordinates held. Enumerate all integers, and bound the
// entire remaining tail by a single normalizer term. This does not assume
// unimodality in size (which is not implied by exponential-family concavity).
inline CmbSizeResult cmb_size_mle(ispan x,double prob,double nu,
                                 double tol=1e-10,int maxit=100) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const auto fail=[&](CmbStatus s) {return CmbSizeResult{nan,{prob,nu,nan,0,s}};};
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
  std::size_t work=0;
  for(int N=mx;;++N) {
    if(static_cast<std::size_t>(N)+1>4000000-work)return fail(CmbStatus::IterationFailure);
    work+=static_cast<std::size_t>(N)+1;
    const auto f=cmb_mle(x,N,tol,maxit,prob,nu);
    if(f.status!=CmbStatus::Ok)return {double(N),f};
    if(!std::isfinite(f.logLik))return fail(CmbStatus::IterationFailure);
    if(f.logLik>best.fit.logLik)best={double(N),f};
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
