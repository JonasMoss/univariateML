#pragma once
#include "../../core/work_budget.hpp"
#include "../pois/zmpois_profile.hpp"
namespace cppunivariate {
// Status and result types retain the shared factorised likelihood contract.
typedef zero_detail::ModifiedStatus ZmnbinomStatus;
struct ZmnbinomResult { double lambda,size,prob,p0,logLik; int iterations; ZmnbinomStatus status; StopDetail stop_detail{}; bool budget_exhausted=false; ZtnbinomCertificateFailure certificate_failure=ZtnbinomCertificateFailure::None; };
inline ZmnbinomResult zmnbinom_mle(ispan x, double tol=1e-10,int maxit=200,double fixed_size=std::numeric_limits<double>::quiet_NaN(),double fixed_prob=std::numeric_limits<double>::quiet_NaN(),double fixed_p0=std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
 ZmnbinomResult out{1,1,.5,0,0,0,ZmnbinomStatus::Ok};
 const bool fb=!std::isnan(fixed_prob), fz=!std::isnan(fixed_p0);
 if(x.empty() || !(tol>0)||!std::isfinite(tol)||maxit<1 ||
    (fz && (!(fixed_p0>=0 && fixed_p0<=1)||!std::isfinite(fixed_p0))) ||
    (fb && (!(fixed_prob>0 && fixed_prob<=1)||!std::isfinite(fixed_prob)))) {out.status=ZmnbinomStatus::InvalidInput;return out;}
 std::vector<int> pos;
 for(int v:x) {
   if(v<0) {out.status=ZmnbinomStatus::InvalidInput;return out;}
   if(v>0)pos.push_back(v);
 }
 const double n=x.size(), k=pos.size();
 out.p0=fz?fixed_p0:(n-k)/n;
 if((n>k && out.p0==0)||(k>0 && out.p0==1)) {out.status=ZmnbinomStatus::InvalidInput;return out;}
 if(!std::isnan(fixed_size)) {
   if(!(fixed_size>=0)||!std::isfinite(fixed_size)) {out.status=ZmnbinomStatus::InvalidSize;return out;}
   out.size=fixed_size;
 }
 if(fb)out.prob=fixed_prob;
 if(pos.empty()) {out.logLik=n*std::log(out.p0);out.status=ZmnbinomStatus::AllZero;return out;}
 const ispan positive{pos.data(),pos.size()};
 const auto r=ztnbinom_mle(positive,tol,maxit,fixed_size,fixed_prob);
 out.size=r.size;out.prob=r.prob;out.logLik=r.logLik;out.iterations=r.iterations;
 out.stop_detail=r.stop_detail;out.budget_exhausted=r.budget_exhausted;out.certificate_failure=r.certificate_failure;
 double indicator=0;
 if(n>k)indicator+=(n-k)*std::log(out.p0);
 indicator+=k*std::log1p(-out.p0);
 const double allowance=1024*std::numeric_limits<double>::epsilon()*(1+n+std::abs(indicator));
 if(r.status==ZtnbinomStatus::PoissonLimit) {
   out.status=ZmnbinomStatus::PoissonLimit;
   out.lambda=r.stop_detail.limit_coef[0];
   out.stop_detail.limit_coef={out.lambda,out.p0};
   out.stop_detail.limit_loglik+=indicator;
   out.stop_detail.loglik_lower+=indicator-allowance;out.stop_detail.loglik_upper+=indicator+allowance;
   return out;
 }
 if(r.status==ZtnbinomStatus::IterationFailure) {out.status=ZmnbinomStatus::IterationFailure;out.stop_detail.loglik_lower+=indicator-allowance;out.stop_detail.loglik_upper+=indicator+allowance;return out;}
 if(r.status==ZtnbinomStatus::InvalidInput)out.status=ZmnbinomStatus::InvalidInput;
 if(n>k)out.logLik+=(n-k)*std::log(out.p0);
 out.logLik+=k*std::log1p(-out.p0);
 if(!std::isfinite(out.logLik))out.status=ZmnbinomStatus::InvalidInput;
 if(out.status==ZmnbinomStatus::Ok && !fb && zero_detail::mean(positive)==1)out.status=ZmnbinomStatus::TwoPoint;
 return out;
}
}
