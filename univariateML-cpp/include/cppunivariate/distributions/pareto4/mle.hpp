#pragma once
#include "../../core/outcome.hpp"
#include "../../core/work_budget.hpp"
#include "../burr/mle.hpp"
#include <vector>
#include <cmath>
namespace cppunivariate {
struct Pareto4Result { double shape1,shape2,scale,min,logLik; int iterations; BurrStatus status; StopDetail stop_detail{}; bool certified=true; bool incompatible_support=false; bool unsupported_threshold=false; bool singleton_proved=false; double limit_endpoint_low=0; bool threshold_unattained=false; };
} // namespace cppunivariate
#include "shifted_scale.hpp"
#include "centered.hpp"
#include "shifted_faces.hpp"
#include "free_threshold.hpp"
namespace cppunivariate {

inline Pareto4Result pareto4_mle_certified(span x, double min, double tol, int maxit);

inline Pareto4Result pareto4_free_threshold(span x,double tol,int maxit,double scale=NAN,double shape1=NAN,double shape2=NAN) {
  const auto fit=gb2_threshold::fit(x,1,shape1,shape2,scale,tol,maxit);
  Pareto4Result out{fit.upper,shape2,fit.scale,fit.threshold,fit.logLik,fit.iterations,
                    BurrStatus::NumericalFailure,{},fit.certified};
  switch(fit.status) {
    case gb2_threshold::Status::Invalid: out.status=BurrStatus::InvalidInit;break;
    case gb2_threshold::Status::Numerical: break;
    case gb2_threshold::Status::Unbounded: out.status=BurrStatus::Unbounded;break;
    case gb2_threshold::Status::Unattained:
      out.threshold_unattained=true;
      out.stop_detail.loglik_lower=fit.logLik-fit.loglik_error;
      out.stop_detail.loglik_upper=fit.logLik+fit.loglik_error;
      break;
    case gb2_threshold::Status::Ok:
      out.status=BurrStatus::Ok;
      out.singleton_proved=fit.certified;
      break;
  }
  return out;
}
inline Pareto4Result pareto4_mle(span x, double min, double tol, int maxit, double scale=std::numeric_limits<double>::quiet_NaN(), double shape1=std::numeric_limits<double>::quiet_NaN(), double shape2=std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  if(std::isnan(min))return pareto4_free_threshold(x,tol,maxit,scale,shape1,shape2);
  for(double v:x)if(!(v>min)) {
    Pareto4Result bad{NAN,NAN,NAN,min,NAN,0,BurrStatus::InvalidInit};
    bad.incompatible_support=true;return bad;
  }
  if(std::isnan(scale) && std::isnan(shape1) && std::isnan(shape2))
    return pareto4_mle_certified(x,min,tol,maxit);
  std::vector<double> y(x.size());
  for (std::size_t i=0;i<x.size();++i) y[i]=(x[i]-min);
  bool valid=tol>0 && std::isfinite(tol) && maxit>0 && std::isfinite(min);
  for(double v:x) valid=valid && std::isfinite(v) && v>min;
  if(!valid) y.clear();
  bool inexact=false;
  if(valid)for(std::size_t i=0;i<x.size();++i)
    inexact=inexact || pareto4_detail::excess_residual(x[i],min,y[i])!=0;
  if(valid && std::isnan(shape1) && std::isnan(shape2) && !std::isnan(scale))
    for(double v:y)if(!std::isfinite(v))return {NAN,NAN,scale,min,NAN,0,BurrStatus::NumericalFailure};
  if(valid && shape1>0 && std::isfinite(shape1) && std::isnan(shape2) && std::isnan(scale) && !x.empty()) {
    double relative_spread=0;
    for(double v:x)relative_spread=std::max(relative_spread,std::abs((v-x[0])/y[0]));
    if(relative_spread>0 && relative_spread<=1e-3)
      return pareto4_detail::centered_shape(x,min,shape1,tol,maxit);
  }
  const bool shifted_scale=valid && !x.empty() && inexact && scale>0 && std::isfinite(scale) &&
    (std::isnan(shape1) || (shape1>0 && std::isfinite(shape1))) &&
    (std::isnan(shape2) || (shape2>0 && std::isfinite(shape2)));
  const bool shifted_power=valid && inexact && !x.empty() && std::isnan(scale) && std::isnan(shape1) &&
     shape2>0 && std::isfinite(shape2) &&
     std::any_of(x.begin(),x.end(),[&](double v) {
       const double y=v-min;
       return std::isfinite(y) && std::abs(pareto4_detail::excess_residual(v,min,y)/y)*shape2>tol/64;
     });
  BurrMLE r{shape1,shape2,scale,NAN,0,BurrStatus::NumericalFailure};
  if(shifted_scale)r=pareto4_detail::shifted_scale_fit(x,min,scale,shape1,shape2,tol,maxit);
  if(shifted_power)r=pareto4_detail::shifted_power_fit(x,min,shape2,tol,maxit);
  if(!shifted_power && (!shifted_scale || r.status==BurrStatus::NumericalFailure)) {
    r=(!std::isnan(shape1)||!std::isnan(shape2)) ? burr_mle_holds(span{y.data(),y.size()},shape1,shape2,scale,tol,maxit) : std::isnan(scale) ? burr_mle_legacy(span{y.data(),y.size()},tol,maxit) : burr_mle_fixed_scale(span{y.data(),y.size()},scale,tol,maxit);
    if(shifted_scale) {
      if(r.status!=BurrStatus::Ok)
        return {shape1,shape2,scale,min,NAN,r.iterations,BurrStatus::NumericalFailure};
      r.certified=false;
      r.stop_detail={};
    }
  }
  if(r.status==BurrStatus::Ok&&!r.certified&&!burr_local_accuracy(x,r.shape1,r.shape2,r.scale,{{std::isnan(shape1),std::isnan(shape2),std::isnan(scale)}},tol,maxit,false,min,r.stop_detail)){r.status=BurrStatus::NumericalFailure;record_accuracy(r,tol,{r.shape1,r.shape2,r.scale},{std::isnan(shape1)?r.shape1:0,std::isnan(shape2)?r.shape2:0,std::isnan(scale)?r.scale:0});}
  if(r.status==BurrStatus::Ok && (!r.certified || inexact)){r.logLik=burr_local_loglik(x,r.shape1,r.shape2,r.scale,false,min);if(!std::isfinite(r.logLik))r.status=BurrStatus::NumericalFailure;}
  if (r.status == BurrStatus::WeibullBoundary && inexact && !shifted_power)
    r.certified = false;
  auto detail=r.stop_detail;
  if(detail.limit_model==StopModel::None && !detail.limit_coef.empty()) {
    if(r.status==BurrStatus::AllGeOne)detail.limit_model=StopModel::Pareto;
    if(r.status==BurrStatus::WeibullBoundary)detail.limit_model=StopModel::Weibull;
  }
  translate_stop_faces(detail,min);
  for(auto& c:detail.candidates) if(c.model==StopModel::None && c.coef.size()==3)c.coef.push_back(min);
  Pareto4Result out{r.shape1,r.shape2,r.scale,min,r.logLik,r.iterations,r.status,detail,r.certified};
  out.singleton_proved=r.status==BurrStatus::Ok && (shifted_scale ? r.certified : r.singleton_proved && (min==0 || std::isfinite(shape1)));
  return out;
}
// Certified joint excess fit shared by the public all-free regular path.
inline Pareto4Result pareto4_mle_certified(span x, double min, double tol, int maxit) {
  maxit = static_cast<int>(solver_budget(maxit));
  if(std::isnan(min))return pareto4_free_threshold(x,tol,maxit);
  for(double v:x)if(!(v>min)) {
    Pareto4Result bad{NAN,NAN,NAN,min,NAN,0,BurrStatus::InvalidInit};
    bad.incompatible_support=true;return bad;
  }
  const double nan=std::numeric_limits<double>::quiet_NaN();
  if(!std::isfinite(min))
    return {nan,nan,nan,min,nan,0,BurrStatus::InvalidInit};
  std::vector<double> y;
  for(double v:x) {
    if(!std::isfinite(v)||!(v>min))
      return {nan,nan,nan,min,nan,0,BurrStatus::InvalidInit};
    if(!std::isfinite(v-min))
      return {nan,nan,nan,min,nan,0,BurrStatus::NumericalFailure};
    y.push_back(v-min);
  }
  const span excesses{y.data(),y.size()};
  bool inexact=false;
  for(std::size_t i=0;i<x.size();++i)
    inexact=inexact || pareto4_detail::excess_residual(x[i],min,y[i])!=0;
  auto r=inexact?pareto4_detail::shifted_joint_fit(x,min,tol,maxit):
                  burr_mle(excesses,tol,maxit);
  double endpoint_low=0;
  if(inexact && r.status==BurrStatus::ParetoBoundary)
    endpoint_low=pareto4_detail::pareto_endpoint_low(x,min,r.scale);
  if(inexact && r.status==BurrStatus::NumericalFailure) {
    std::vector<std::size_t> order;
    for(std::size_t i=0;i<x.size();++i)order.push_back(i);
    std::sort(order.begin(),order.end(),[&](std::size_t i,std::size_t j){return x[i]<x[j];});
    for(std::size_t i=1;i<order.size();++i)
      if(x[order[i]]!=x[order[i-1]] && y[order[i]]==y[order[i-1]])
        return {nan,nan,nan,min,nan,0,BurrStatus::NumericalFailure};
    r=burr_mle(excesses,tol,maxit);
    bool transported=false;
    if(r.status==BurrStatus::ParetoBoundary && !order.empty()) {
      const double first=x[order.front()], last=x[order.back()];
      if(first!=last && std::all_of(x.begin(),x.end(),[&](double v){return v==first || v==last;})) {
        r=pareto4_detail::shifted_two_support_pareto(x,min,r);
        transported=r.status==BurrStatus::ParetoBoundary;
        if(transported)endpoint_low=pareto4_detail::pareto_endpoint_low(x,min,r.scale);
      }
    }
    r.certified=r.certified && transported;
  }
  if(r.status==BurrStatus::Ok&&!r.certified&&!burr_local_accuracy(x,r.shape1,r.shape2,r.scale,{{true,true,true}},tol,maxit,false,min,r.stop_detail)){r.status=BurrStatus::NumericalFailure;record_accuracy(r,tol,{r.shape1,r.shape2,r.scale},{r.shape1,r.shape2,r.scale});}
  if(r.status==BurrStatus::Ok&&!r.certified){r.logLik=burr_local_loglik(x,r.shape1,r.shape2,r.scale,false,min);if(!std::isfinite(r.logLik))r.status=BurrStatus::NumericalFailure;}
  auto detail=r.stop_detail;
  if(detail.limit_model==StopModel::None && !detail.limit_coef.empty()) {
    if(r.status==BurrStatus::AllGeOne)detail.limit_model=StopModel::Pareto;
    if(r.status==BurrStatus::WeibullBoundary)detail.limit_model=StopModel::Weibull;
  }
  translate_stop_faces(detail,min);
  for(auto& c:detail.candidates) if(c.model==StopModel::None && c.coef.size()==3)c.coef.push_back(min);
  Pareto4Result out{r.shape1,r.shape2,r.scale,min,r.logLik,r.iterations,r.status,detail,r.certified};
  out.singleton_proved=r.singleton_proved && min==0;
  out.limit_endpoint_low=endpoint_low;
  return out;
}

}

namespace cppunivariate {
inline bool family_point(const Pareto4Result&, const FitCandidate& c) {
  return c.model == StopModel::None && c.coef.size() == 4 && std::isfinite(c.coef[0]) &&
         std::isfinite(c.coef[1]) && std::isfinite(c.coef[2]) && std::isfinite(c.coef[3]) &&
         c.coef[0] > 0 && c.coef[1] > 0 && c.coef[2] > 0;
}
// CertificateTie transports the shifted Burr certificate's witnesses, with
// likelihood bounds; an accurate converged search is already an uncertified Ok.
inline bool accurate_point(const Pareto4Result&, const FitCandidate&) { return false; }
inline FitOutcome outcome(const Pareto4Result& r) {
  if(r.threshold_unattained)return FitOutcome::nonexistence(OutcomeReason::Unattained);
  if(r.unsupported_threshold)return FitOutcome::failure(OutcomeReason::Unsupported);
  if (r.stop_detail.accuracy_stop == AccuracyStop::Tolerance) return FitOutcome::failure(OutcomeReason::Tolerance);
  if (r.stop_detail.accuracy_stop == AccuracyStop::Conditioning) return FitOutcome::failure(OutcomeReason::Conditioning);
  switch (r.status) {
    case BurrStatus::Ok: return r.singleton_proved ? FitOutcome::singleton(r.certified) : FitOutcome::unknown_cardinality(r.certified);
    case BurrStatus::WeibullBoundary: return FitOutcome::limit(r.certified);
    case BurrStatus::ParetoBoundary: return FitOutcome::limit(r.certified);
    case BurrStatus::InvalidInit: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case BurrStatus::IterationFailure: return FitOutcome::failure(OutcomeReason::Unresolved);
    case BurrStatus::AllGeOne: return FitOutcome::limit(r.certified);
    case BurrStatus::Unbounded: return FitOutcome::nonexistence(OutcomeReason::Unbounded);
    case BurrStatus::CertificateTie: return comparison_outcome_of(r);
    case BurrStatus::CertificateBudget: return FitOutcome::budget();
    case BurrStatus::NumericalFailure: return FitOutcome::failure(OutcomeReason::Evaluation);
  }
  return {};
}
} // namespace cppunivariate
