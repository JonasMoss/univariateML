#pragma once
#include "../zmpois/profile.hpp"
namespace cppunivariate {
// Status and result types retain the shared factorised likelihood contract.
typedef zero_detail::ModifiedStatus ZmlogarithmicStatus;
typedef zero_detail::ModifiedResult ZmlogarithmicResult;
inline ZmlogarithmicResult zmlogarithmic_mle(ispan x, double tol=1e-10,int maxit=200,double fixed_prob=std::numeric_limits<double>::quiet_NaN(),double fixed_p0=std::numeric_limits<double>::quiet_NaN()) {
 if(std::isnan(fixed_prob) && std::isnan(fixed_p0)) {
  return zero_detail::modified_mle(x,4,tol,maxit,std::numeric_limits<double>::quiet_NaN());
 }
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
 if(pos.empty()) {out.logLik=n*std::log(out.p0);out.status=ZmlogarithmicStatus::AllZero;return out;}
 const ispan positive{pos.data(),pos.size()};
 if(!fb) {const auto r=zero_detail::logarithmic_fit(positive);out.prob=r.all_ones?0:r.theta;out.logLik=r.all_ones?0:r.logLik;}
 else for(int v:positive)out.logLik+=fixed_prob==0?(v==1?0:-std::numeric_limits<double>::infinity()):
    v*std::log(fixed_prob)-std::log(v)-std::log(-std::log1p(-fixed_prob));
 if(n>k)out.logLik+=(n-k)*std::log(out.p0);
 out.logLik+=k*std::log1p(-out.p0);
 if(!std::isfinite(out.logLik))out.status=ZmlogarithmicStatus::InvalidInput;
 return out;
}
}
