#pragma once
#include "../zmpois/profile.hpp"
namespace cppunivariate {
// Status and result types retain the shared factorised likelihood contract.
typedef zero_detail::ModifiedStatus ZmgeomStatus;
typedef zero_detail::ModifiedResult ZmgeomResult;
inline ZmgeomResult zmgeom_mle(ispan x, double tol=1e-10,int maxit=200,double fixed_prob=std::numeric_limits<double>::quiet_NaN(),double fixed_p0=std::numeric_limits<double>::quiet_NaN()) {
 if(std::isnan(fixed_prob) && std::isnan(fixed_p0)) {
  return zero_detail::modified_mle(x,1,tol,maxit,std::numeric_limits<double>::quiet_NaN());
 }
 ZmgeomResult out{1,1,.5,0,0,0,ZmgeomStatus::Ok};
 const bool fb=!std::isnan(fixed_prob), fz=!std::isnan(fixed_p0);
 if(x.empty() || !(tol>0)||!std::isfinite(tol)||maxit<1 ||
    (fz && (!(fixed_p0>=0 && fixed_p0<=1)||!std::isfinite(fixed_p0))) ||
    (fb && (!(fixed_prob>0 && fixed_prob<=1)||!std::isfinite(fixed_prob)))) {out.status=ZmgeomStatus::InvalidInput;return out;}
 std::vector<int> pos;
 for(int v:x) {
   if(v<0) {out.status=ZmgeomStatus::InvalidInput;return out;}
   if(v>0)pos.push_back(v);
 }
 const double n=x.size(), k=pos.size();
 out.p0=fz?fixed_p0:(n-k)/n;
 if((n>k && out.p0==0)||(k>0 && out.p0==1)) {out.status=ZmgeomStatus::InvalidInput;return out;}

 if(fb)out.prob=fixed_prob;
 if(pos.empty()) {out.logLik=n*std::log(out.p0);out.status=ZmgeomStatus::AllZero;return out;}
 const ispan positive{pos.data(),pos.size()};
 if(!fb) {const auto r=ztgeom_mle(positive);out.prob=r.prob;out.logLik=r.logLik;}
 else for(int v:positive)out.logLik+=fixed_prob==1?(v==1?0:-std::numeric_limits<double>::infinity()):
    std::log(fixed_prob)+(v-1)*std::log1p(-fixed_prob);
 if(n>k)out.logLik+=(n-k)*std::log(out.p0);
 out.logLik+=k*std::log1p(-out.p0);
 if(!std::isfinite(out.logLik))out.status=ZmgeomStatus::InvalidInput;
 return out;
}
}
