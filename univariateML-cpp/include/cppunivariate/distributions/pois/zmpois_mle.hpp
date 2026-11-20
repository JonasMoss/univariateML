#pragma once
#include "../../core/work_budget.hpp"
#include "zmpois_profile.hpp"
namespace cppunivariate {
// Status and result types retain the shared factorised likelihood contract.
typedef zero_detail::ModifiedStatus ZmpoisStatus;
typedef zero_detail::ModifiedResult ZmpoisResult;
inline ZmpoisResult zmpois_mle(ispan x, double tol=1e-10,int maxit=200,double fixed_lambda=std::numeric_limits<double>::quiet_NaN(),double fixed_p0=std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
 if(std::isnan(fixed_lambda) && std::isnan(fixed_p0)) {
  auto out=zero_detail::modified_mle(x,0,tol,maxit,std::numeric_limits<double>::quiet_NaN());
  if((out.status==ZmpoisStatus::Ok || out.status==ZmpoisStatus::TwoPoint) && !std::isfinite(out.logLik))
    out.status=ZmpoisStatus::IterationFailure;
  return out;
 }
 ZmpoisResult out{1,1,.5,0,0,0,ZmpoisStatus::Ok};
 const bool fb=!std::isnan(fixed_lambda), fz=!std::isnan(fixed_p0);
 if(x.empty() || !(tol>0)||!std::isfinite(tol)||maxit<1 ||
    (fz && (!(fixed_p0>=0 && fixed_p0<=1)||!std::isfinite(fixed_p0))) ||
    (fb && (!(fixed_lambda>=0)||!std::isfinite(fixed_lambda)))) {out.status=ZmpoisStatus::InvalidInput;return out;}
 std::vector<int> pos;
 for(int v:x) {
   if(v<0) {out.status=ZmpoisStatus::InvalidInput;return out;}
   if(v>0)pos.push_back(v);
 }
 const double n=x.size(), k=pos.size();
 out.p0=fz?fixed_p0:(n-k)/n;
 if((n>k && out.p0==0)||(k>0 && out.p0==1)) {out.status=ZmpoisStatus::InvalidInput;return out;}

 if(fb)out.lambda=fixed_lambda;
 if(pos.empty()) {out.logLik=n*std::log(out.p0);out.status=fb?ZmpoisStatus::Ok:ZmpoisStatus::AllZero;return out;}
 const ispan positive{pos.data(),pos.size()};
 if(!fb) {
   const auto r=ztpois_mle(positive,NAN,tol,maxit);
   out.lambda=r.lambda;out.logLik=r.logLik;out.iterations=r.iterations;out.stop_detail=r.stop_detail;
   if(r.status!=ZtpoisStatus::Ok)out.status=ZmpoisStatus::IterationFailure;
 }
 else for(int v:positive)out.logLik+=distributions::dztpois(v,fixed_lambda,true);
 if(n>k)out.logLik+=(n-k)*std::log(out.p0);
 out.logLik+=k*std::log1p(-out.p0);
 if(!std::isfinite(out.logLik))out.status=out.lambda>0?ZmpoisStatus::IterationFailure:ZmpoisStatus::InvalidInput;
 if(!fz && out.status==ZmpoisStatus::Ok &&
    !zero_detail::zero_fraction_accurate(n-k,n,out.p0,tol,out.stop_detail)) {
   out.status=ZmpoisStatus::IterationFailure;
   out.stop_detail.candidates.push_back({{out.lambda,out.p0},out.logLik,out.logLik});
 }
 return out;
}
}
