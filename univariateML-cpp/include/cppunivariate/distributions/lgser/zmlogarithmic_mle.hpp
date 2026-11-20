#pragma once
#include "../../core/work_budget.hpp"
#include "mle.hpp"
#include "../pois/zmpois_profile.hpp"
namespace cppunivariate {
namespace zmlogarithmic_detail {
// Solve in u = -log(1 - theta), where the conditional mean is
// expm1(u)/u. Spend one work allowance on the solve and any replay.
inline LgserResult positive_fit(ispan x,double tol,int maxit) {
  const double n=x.size();
  double sum=0,logsum=0;
  for(int v:x){sum+=v;logsum+=std::log(v);}
  const double excess=(sum-n)/n;
  if(excess==0)return {0,0,true};
  int work=0;
  double theta=NAN;
  const double mean=sum/n,log_mean=std::log(mean);
  const bool short_tail_solve=log_mean>16 && maxit<std::max(2.,-std::log2(tol));
  if(short_tail_solve) {
    // For large means, start from the expansion of u = log(1 + mean*u).
    // One Newton step can already resolve theta on its public scale.
    const double l=std::log(log_mean);
    double u=log_mean+l+l/log_mean;
    u-=(u-std::log1p(mean*u))/(1-mean/(1+mean*u));
    theta=-std::expm1(-u);
    ++work;
  } else {
    double hi=1;
    while(work<maxit && lgser_detail::q(hi)<excess){++work;hi*=2;}
    double lo=hi;
    while(work<maxit && lgser_detail::q(lo)>=excess){++work;lo*=.5;}
    for(;work<maxit && hi/lo-1>4*std::numeric_limits<double>::epsilon();++work) {
      const double mid=std::sqrt(lo)*std::sqrt(hi);
      if(lgser_detail::q(mid)<excess)lo=mid;else hi=mid;
    }
    theta=-std::expm1(-std::sqrt(lo)*std::sqrt(hi));
  }
  const auto sign=[&](double p) {
    if(p<=0)return -1;
    if(p>=1)return 1;
    const double u=-std::log1p(-p),v=lgser_detail::q(u);
    const double pad=16*std::numeric_limits<double>::epsilon()*(v+excess+(u<1e-3?0:1));
    return v<excess-pad?-1:v>excess+pad?1:0;
  };
  // The two signs enclose the maximizing theta in public coordinates.
  bool closed=theta>0 && theta<1 && sign(theta-tol/2)<0 && sign(theta+tol/2)>0 &&
    theta-(theta-tol/2)<=tol && theta+tol/2-theta<=tol;
  if(!closed && work<maxit) {
    const auto replay=zero_detail::logarithmic_positive_root(x,tol,maxit-work);
    theta=replay.coefficient;closed=replay.closed;
  }
  const double ll=-n*std::log(-std::log1p(-theta))+sum*std::log(theta)-logsum;
  LgserResult out{theta,ll,false};
  if(closed && short_tail_solve && work==maxit)out.stop_detail.budget_exhausted=true;
  if(!closed || !std::isfinite(ll)) {
    out.status=LgserStatus::NumericalFailure;
    record_accuracy(out.stop_detail,tol,{theta},ll,{1});
  }
  return out;
}
}
// Status and result types retain the shared factorised likelihood contract.
typedef zero_detail::ModifiedStatus ZmlogarithmicStatus;
typedef zero_detail::ModifiedResult ZmlogarithmicResult;
inline ZmlogarithmicResult zmlogarithmic_mle(ispan x, double tol=1e-10,int maxit=200,double fixed_prob=std::numeric_limits<double>::quiet_NaN(),double fixed_p0=std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
 ZmlogarithmicResult out{1,1,.5,0,0,0,ZmlogarithmicStatus::Ok};
 const bool fb=!std::isnan(fixed_prob), fz=!std::isnan(fixed_p0);
 if(x.empty() || !(tol>0)||!std::isfinite(tol)||maxit<1 ||
    (fz && (!(fixed_p0>=0 && fixed_p0<=1)||!std::isfinite(fixed_p0))) ||
    (fb && (!(fixed_prob>=0 && fixed_prob<1)||!std::isfinite(fixed_prob)))) {out.status=ZmlogarithmicStatus::InvalidInput;return out;}
 std::vector<int> pos;
 for(int v:x) {
   if(v<0) {out.status=ZmlogarithmicStatus::InvalidInput;return out;}
   if(v>0)pos.push_back(v);
 }
 const double n=x.size(), k=pos.size();
 out.p0=fz?fixed_p0:(n-k)/n;
 if((n>k && out.p0==0)||(k>0 && out.p0==1)) {out.status=ZmlogarithmicStatus::InvalidInput;return out;}

 if(fb)out.prob=fixed_prob;
 if(pos.empty()) {out.logLik=n*std::log(out.p0);out.status=fb?ZmlogarithmicStatus::Ok:ZmlogarithmicStatus::AllZero;return out;}
 const ispan positive{pos.data(),pos.size()};
 if(!fb) {
   const auto r=zmlogarithmic_detail::positive_fit(positive,tol,maxit);
   out.prob=r.all_ones?0:r.theta;out.logLik=r.all_ones?0:r.logLik;out.stop_detail=r.stop_detail;
   if(r.status!=LgserStatus::Ok && !r.all_ones) {out.status=ZmlogarithmicStatus::IterationFailure;return out;}
   if(!fz && r.all_ones)out.status=ZmlogarithmicStatus::TwoPoint;
 }
 else for(int v:positive)out.logLik+=fixed_prob==0?(v==1?0:-std::numeric_limits<double>::infinity()):
    v*std::log(fixed_prob)-std::log(v)-std::log(-std::log1p(-fixed_prob));
 if(n>k)out.logLik+=(n-k)*std::log(out.p0);
 out.logLik+=k*std::log1p(-out.p0);
 if(!std::isfinite(out.logLik))out.status=fb && fixed_prob==0?ZmlogarithmicStatus::InvalidInput:ZmlogarithmicStatus::IterationFailure;
 if(!fz && (out.status==ZmlogarithmicStatus::Ok || out.status==ZmlogarithmicStatus::TwoPoint) &&
    !zero_detail::zero_fraction_accurate(n-k,n,out.p0,tol,out.stop_detail)) {
   out.status=ZmlogarithmicStatus::IterationFailure;
   out.stop_detail.candidates.push_back({{out.prob,out.p0},out.logLik,out.logLik});
 }
 return out;
}
}
