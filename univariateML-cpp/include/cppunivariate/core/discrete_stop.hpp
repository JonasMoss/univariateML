#pragma once
#include "stop_detail.hpp"
#include <algorithm>
namespace cppunivariate {
// Evaluated face values are witnesses, not global upper bounds.
inline StopDetail discrete_face(StopModel model, std::vector<double> coef, double ll) {
  StopDetail d; d.limit_model=model; d.limit_coef=coef; d.limit_loglik=ll;
  d.candidates.push_back({coef,ll,ll,model}); d.loglik_lower=ll; return d;
}
inline StopDetail discrete_budget(StopDetail d) {
  d.budget_exhausted=true; d.loglik_upper=INFINITY;
  for(const auto& c:d.candidates)
    if(std::isnan(d.loglik_lower)||c.loglik_lower>d.loglik_lower)d.loglik_lower=c.loglik_lower;
  return d;
}
template<class Sample>
inline StopDetail discrete_two_point(Sample x, double lo, double hi, double prob=NAN, double log_odds=NAN) {
  double high=0; for(auto v:x) if(v==hi) ++high;
  if(std::isnan(prob)) prob=high/x.size();
  double lp=std::log(prob),lq=std::log1p(-prob);
  if(std::isfinite(log_odds)) {
    lp=std::min(0.,log_odds)-std::log1p(std::exp(-std::abs(log_odds)));
    lq=std::min(0.,-log_odds)-std::log1p(std::exp(-std::abs(log_odds)));
    prob=std::exp(lp);
  }
  double ll=0;
  if(high>0) ll+=high*lp;
  if(x.size()>high) ll+=(x.size()-high)*lq;
  if(lo==0 && hi==1) return discrete_face(StopModel::Binom,{1,prob},ll);
  return discrete_face(StopModel::TwoPoint,{lo,hi,prob},ll);
}
inline StopDetail discrete_point(double point) {
  return discrete_face(StopModel::PointMass,{point},0);
}
} // namespace cppunivariate
