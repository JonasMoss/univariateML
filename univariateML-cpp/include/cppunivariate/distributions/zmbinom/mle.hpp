#pragma once
#include "../../core/work_budget.hpp"
#include "../zmpois/profile.hpp"
namespace cppunivariate {
// Status and result types retain the shared factorised likelihood contract.
typedef zero_detail::ModifiedStatus ZmbinomStatus;
struct ZmbinomResult : zero_detail::ModifiedResult {
  bool certified = true;
  bool accuracy_failure = false;
};
inline ZmbinomResult zmbinom_mle(ispan x, double tol=1e-10,int maxit=200,double fixed_size=std::numeric_limits<double>::quiet_NaN(),
 double fixed_prob=std::numeric_limits<double>::quiet_NaN(),double fixed_p0=std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));

 ZmbinomResult out{{1,1,.5,0,0,0,ZmbinomStatus::InvalidInput}};
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
 if(k==0) {if(std::isnan(fixed_prob)&&std::isnan(fixed_p0))out.status=ZmbinomStatus::AllZero;return out;}
 const auto fit=ztbinom_mle({pos.data(),pos.size()},tol,maxit,fixed_size,fixed_prob);
 if(std::isnan(fixed_prob)&&std::isnan(fixed_p0)&&fit.prob==0)out.status=ZmbinomStatus::TwoPoint;
 out.certified=fit.certified;out.accuracy_failure=fit.accuracy_failure;
 const double zero_shift=out.logLik;
 out.size=fit.size;out.prob=fit.prob;out.logLik+=fit.logLik;
 if(fit.status==ZtbinomStatus::PoissonLimit) {
   out.status=ZmbinomStatus::PoissonLimit;
   const auto& d=fit.stop_detail;
   out.stop_detail=discrete_face(StopModel::Zmpois,{d.limit_coef[0],out.p0},zero_shift+d.limit_loglik);
 }
 if(!fit.stop_detail.candidates.empty() || fit.stop_detail.budget_exhausted) {
   const double shift=zero_shift;
   out.stop_detail=fit.stop_detail;
   for(auto& c:out.stop_detail.candidates) {
     if(c.model==StopModel::None)c.coef.push_back(out.p0);
     else if(c.model==StopModel::Ztpois){c.model=StopModel::Zmpois;c.coef.push_back(out.p0);}
     c.loglik_lower+=shift;c.loglik_upper+=shift;
   }
   if(out.stop_detail.limit_model==StopModel::Ztpois) {
     out.stop_detail.limit_model=StopModel::Zmpois;out.stop_detail.limit_coef.push_back(out.p0);
   }
   out.stop_detail.limit_loglik+=shift;out.stop_detail.loglik_lower+=shift;out.stop_detail.loglik_upper+=shift;
 }
 if(fit.status==ZtbinomStatus::IterationFailure)out.status=ZmbinomStatus::IterationFailure;
 if(fit.status==ZtbinomStatus::InvalidInput)out.status=ZmbinomStatus::InvalidInput;
 if(fit.status==ZtbinomStatus::InvalidSize)out.status=ZmbinomStatus::InvalidSize;
 // The empirical zero probability is a separately estimated count ratio.
 if(std::isnan(fixed_p0) && out.p0>0 && out.p0<1 &&
    tol<8*std::numeric_limits<double>::epsilon()) {
   out.certified=false;
   if(!binomial_accuracy::ratio_accuracy(n-k,n,out.p0,tol)) {
     out.status=ZmbinomStatus::IterationFailure;out.accuracy_failure=true;record_accuracy(out,tol,{out.size,out.prob,out.p0},{0,1,1});
     const auto exact=binomial_accuracy::divide(binomial_accuracy::Pair(n-k),binomial_accuracy::Pair(n));
     const double nearest=exact.hi+exact.lo;
     const double probability_floor=fit.stop_detail.accuracy_stop==AccuracyStop::Tolerance?fit.stop_detail.attainable_tol:0;
     out.stop_detail.attainable_tol=std::max(probability_floor,binomial_accuracy::magnitude(binomial_accuracy::add(exact,binomial_accuracy::Pair(-nearest))));
     out.stop_detail.accuracy_stop=tol<out.stop_detail.attainable_tol?AccuracyStop::Tolerance:
       fit.stop_detail.accuracy_stop==AccuracyStop::Conditioning?AccuracyStop::Conditioning:AccuracyStop::None;
     binomial_certificate_detail::witness(out.stop_detail,{out.size,out.prob,out.p0},out.logLik,false);
   }
 }
 return out;
}
}
