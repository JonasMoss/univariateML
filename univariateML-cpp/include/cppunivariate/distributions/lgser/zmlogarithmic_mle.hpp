#pragma once
#include "../../core/work_budget.hpp"
#include "mle.hpp"
#include "../pois/zmpois_profile.hpp"
namespace cppunivariate {
namespace zmlogarithmic_detail {
// Rounding theta near one amplifies the public mean residual. Failure of
// its optional replay must not discard a finite primary likelihood fit.
inline LgserResult positive_fit(ispan x,int maxit) {
  maxit=static_cast<int>(solver_budget(maxit));
  auto initial=lgser_mle(x);
  if(initial.status==LgserStatus::InvalidInput)return initial;
  const double m=zero_detail::mean(x);
  if(m==1)return {0,0,true};
  const bool primary=initial.status==LgserStatus::Ok && initial.theta>0 && initial.theta<1 &&
    std::isfinite(initial.logLik);
  if(primary && std::abs(-initial.theta/((1-initial.theta)*std::log1p(-initial.theta))-m)<1e-10*m)
    return initial;
  double lo=0,hi=1;
  for(int i=0;i<maxit;++i) {
    const double p=(lo+hi)/2;
    if(p==lo || p==hi)break;
    if(-p/((1-p)*std::log1p(-p))<m)lo=p;else hi=p;
  }
  if(hi-lo>=1e-12) {
    if(primary) {initial.stop_detail.budget_exhausted=true;return initial;}
    initial.status=LgserStatus::NumericalFailure;return initial;
  }
  LgserResult replay{(lo+hi)/2,0,false};
  for(int v:x)replay.logLik+=v*std::log(replay.theta)-std::log(v)-std::log(-std::log1p(-replay.theta));
  if(!std::isfinite(replay.logLik))replay.status=LgserStatus::NumericalFailure;
  return replay;
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
   const auto r=zmlogarithmic_detail::positive_fit(positive,maxit);
   out.prob=r.all_ones?0:r.theta;out.logLik=r.all_ones?0:r.logLik;out.stop_detail=r.stop_detail;
   if(r.status!=LgserStatus::Ok && !r.all_ones) {out.status=ZmlogarithmicStatus::IterationFailure;return out;}
   if(!fz && r.all_ones)out.status=ZmlogarithmicStatus::TwoPoint;
 }
 else for(int v:positive)out.logLik+=fixed_prob==0?(v==1?0:-std::numeric_limits<double>::infinity()):
    v*std::log(fixed_prob)-std::log(v)-std::log(-std::log1p(-fixed_prob));
 if(n>k)out.logLik+=(n-k)*std::log(out.p0);
 out.logLik+=k*std::log1p(-out.p0);
 if(!std::isfinite(out.logLik))out.status=fb && fixed_prob==0?ZmlogarithmicStatus::InvalidInput:ZmlogarithmicStatus::IterationFailure;
 return out;
}
}
