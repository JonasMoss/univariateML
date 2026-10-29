#pragma once
#include "../zmpois/profile.hpp"
namespace cppunivariate {
// Status and result types retain the shared factorised likelihood contract.
typedef zero_detail::ModifiedStatus ZmbinomStatus;
typedef zero_detail::ModifiedResult ZmbinomResult;
inline ZmbinomResult zmbinom_mle(ispan x, double tol=1e-10,int maxit=200,double fixed_size=std::numeric_limits<double>::quiet_NaN(),
 double fixed_prob=std::numeric_limits<double>::quiet_NaN(),double fixed_p0=std::numeric_limits<double>::quiet_NaN()) {
 if(std::isnan(fixed_prob) && std::isnan(fixed_p0))return zero_detail::modified_mle(x,3,tol,maxit,fixed_size);
 ZmbinomResult out{1,1,.5,0,0,0,ZmbinomStatus::InvalidInput};
 if(x.size()==0 || !(tol>0) || !std::isfinite(tol) || maxit<1)return out;
 if(!std::isnan(fixed_prob) && (!std::isfinite(fixed_prob)||fixed_prob<0||fixed_prob>1))return out;
 if(!std::isnan(fixed_p0) && (!std::isfinite(fixed_p0)||fixed_p0<0||fixed_p0>1))return out;
 const bool fs=!std::isnan(fixed_size);
 if(fs && (!std::isfinite(fixed_size)||fixed_size<1||fixed_size!=std::floor(fixed_size))) {out.status=ZmbinomStatus::InvalidSize;return out;}
 std::vector<int> pos;
 for(int v:x) {if(v<0)return out;if(v>0)pos.push_back(v);if(fs && v>fixed_size){out.status=ZmbinomStatus::InvalidSize;return out;}}
 const double k=pos.size(), n=x.size();
 out.p0=std::isnan(fixed_p0)?(n-k)/n:fixed_p0;
 out.size=fs?fixed_size:1;out.prob=std::isnan(fixed_prob)?.5:fixed_prob;
 out.logLik=0;
 if(n>k)out.logLik+=(n-k)*std::log(out.p0);
 if(k>0)out.logLik+=k*std::log1p(-out.p0);
 if(!std::isfinite(out.logLik))return out;
 out.status=ZmbinomStatus::Ok;
 if(k==0)return out;
 const auto fit=ztbinom_mle({pos.data(),pos.size()},tol,maxit,fixed_size,fixed_prob);
 out.size=fit.size;out.prob=fit.prob;out.logLik+=fit.logLik;
 if(fit.status==ZtbinomStatus::PoissonLimit)out.status=ZmbinomStatus::PoissonLimit;
 if(fit.status==ZtbinomStatus::IterationFailure)out.status=ZmbinomStatus::IterationFailure;
 if(fit.status==ZtbinomStatus::InvalidInput)out.status=ZmbinomStatus::InvalidInput;
 if(fit.status==ZtbinomStatus::InvalidSize)out.status=ZmbinomStatus::InvalidSize;
 return out;
}
}
