#pragma once
#include "dpqr.hpp"
#include "../../core/outcome.hpp"
#include "../../core/work_budget.hpp"
#include "../gb2/mle.hpp"
#include "accuracy.hpp"
#include <vector>
#include <cmath>
namespace cppunivariate {
enum class FparetoStatus { Ok, TrGammaBoundary, InvTrGammaBoundary,
  LognormalBoundary, ParetoBoundary, PowerBoundary, DoubleParetoBoundary,
  InvalidData, InvalidFixed, SearchUnresolved, ScaleOutOfRange,
  WeibullBoundary, InvWeibullBoundary, CertificateTie, CertificateBudget, NumericalFailure , Unbounded, UnsupportedThreshold, FaceRefuted };
struct FparetoResult { double shape1,shape2,shape3,scale,min,logLik; int iterations; FparetoStatus status; StopDetail stop_detail{}; bool certified=true; std::int64_t profile_evaluations=0; };

inline FparetoResult fpareto_mle(span x, double min, double tol, int maxit, double shape1=std::numeric_limits<double>::quiet_NaN(), double shape2=std::numeric_limits<double>::quiet_NaN(), double shape3=std::numeric_limits<double>::quiet_NaN(), double scale=std::numeric_limits<double>::quiet_NaN()) {
  if(std::isnan(min))return {NAN,NAN,NAN,NAN,min,NAN,0,FparetoStatus::UnsupportedThreshold};
  maxit = static_cast<int>(solver_budget(maxit));
  std::vector<double> y(x.size());
  for (std::size_t i=0;i<x.size();++i) y[i]=(x[i]-min);
  bool valid=tol>0 && std::isfinite(tol) && maxit>0 && std::isfinite(min);
  for(double v:x) valid=valid && std::isfinite(v) && v>min;
  if(!valid) y.clear();
  for(double value:{shape1,shape2,shape3,scale})if(!std::isnan(value) && !(value>0 && std::isfinite(value)))
    return {shape1,shape2,shape3,scale,min,NAN,0,FparetoStatus::InvalidFixed};
  const auto excess_order=[&]() {
    const double excess=x[0]-min;
    if(excess<scale)return -1;if(excess>scale)return 1;
    const double virtual_b=excess-x[0];
    const double residual=(x[0]-(excess-virtual_b))+(-min-virtual_b);
    return residual<0?-1:residual>0?1:0;
  };
  const bool concentrating=(std::isnan(shape1)&&std::isnan(shape3)) ||
    (std::isnan(shape2) && (std::isnan(scale) || (!x.empty() &&
      (excess_order()==0 || (excess_order()>0 && std::isnan(shape3)) || (excess_order()<0 && std::isnan(shape1))))));
  if(valid && !x.empty() && concentrating &&
      std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];})) {
    return {NAN,NAN,NAN,NAN,min,INFINITY,0,FparetoStatus::Unbounded};
  }
  // Overflow in the excess transformation is arithmetic failure, rather
  // than evidence that the original finite observations violate support.
  if(valid)for(double v:y)if(!std::isfinite(v))
    return {NAN,NAN,NAN,scale,min,NAN,0,FparetoStatus::NumericalFailure};
  const bool power_certificate=!std::isnan(shape2) && std::isnan(scale) &&
    (std::isnan(shape1)!=std::isnan(shape3)) && shape1!=1 && shape3!=1;
  if(valid && ((!std::isnan(shape1)&&!std::isnan(shape3)) || power_certificate || (!std::isnan(scale) && std::isnan(shape2) &&
     ((!std::isnan(shape1)&&std::isnan(shape3))||(!std::isnan(shape3)&&std::isnan(shape1)))))) {
    // A rounded equality at held scale must be an exact subtraction.
    // Error-free TwoSum residual for x[i] + (-min).
    for(std::size_t i=0;i<x.size();++i)if(y[i]==scale) {
      const double virtual_b=y[i]-x[i];
      const double residual=(x[i]-(y[i]-virtual_b))+(-min-virtual_b);
      if(!std::isfinite(residual)||residual!=0)return {NAN,NAN,NAN,scale,min,NAN,0,FparetoStatus::NumericalFailure};
    }
    std::vector<std::size_t> order;
    for(std::size_t i=0;i<x.size();++i)order.push_back(i);
    std::sort(order.begin(),order.end(),[&](std::size_t i,std::size_t j){return x[i]<x[j];});
    for(std::size_t i=1;i<order.size();++i)
      if(x[order[i]]!=x[order[i-1]]&&y[order[i]]==y[order[i-1]])
        return {NAN,NAN,NAN,scale,min,NAN,0,FparetoStatus::NumericalFailure};
  }
  // tol is the GB2 delegate's own tol (its default is also 1e-10): its local polish and transport accuracy check bound the
  // relative error of every free coefficient by tol or the fit is stopped, so no rule of its own is added here.
  auto r=gb2_mle(span{y.data(),y.size()},tol,maxit,shape1,shape2,shape3,scale);
  if(r.status==Gb2Status::Ok&&!r.certified&&!fpareto_transport_accuracy(x,min,r,{{std::isnan(shape1),std::isnan(shape2),std::isnan(shape3),std::isnan(scale)}},tol,maxit)){r.status=Gb2Status::NumericalFailure;record_accuracy(r,tol,{r.shape1,r.shape2,r.shape3,r.scale},{std::isnan(shape1)?r.shape1:0,std::isnan(shape2)?r.shape2:0,std::isnan(shape3)?r.shape3:0,std::isnan(scale)?r.scale:0});}
  auto detail=r.stop_detail;bool face_refuted=false;
  const auto attach_limit=[&](const StopDetail& face) {
    detail.limit_model=face.limit_model;detail.limit_coef=face.limit_coef;
    detail.limit_loglik=face.limit_loglik;
  };
  if(r.status==Gb2Status::LognormalBoundary) {
    const auto face=lognormal_mle({y.data(),y.size()});
    if(face.status==LognormalStatus::Ok)attach_limit(limit_detail(min==0?std::vector<double>{face.meanlog,face.sdlog}:std::vector<double>{face.meanlog,face.sdlog,min},face.logLik,min==0?StopModel::Lnorm:StopModel::None));
    if(face.status==LognormalStatus::Ok) {
      // Feasible beta-log-odds approaches to this normal face. Their first
      // two cumulants tend to the face moments. A strictly higher evaluated
      // likelihood refutes the selected face; no such point is returned as an MLE.
      for(int direction:std::vector<int>{-1,1}) {
        const double b=std::isnan(scale)?std::exp(face.meanlog-direction*face.sdlog):scale;
        if(!(b>0&&std::isfinite(b)))continue;
        const double delta=face.meanlog-std::log(b);
        double a=.25/std::max(face.sdlog,std::abs(delta));
        for(std::int64_t step=0;step<solver_budget(maxit);++step,a*=.5) {
          const double ratio=std::exp(a*delta),den=(a*face.sdlog)*(a*face.sdlog);
          const double p=(1+ratio)/den,q=(1+1/ratio)/den;
          if(!(a>0&&p>0&&q>0&&std::isfinite(p)&&std::isfinite(q)))break;
          double ll=0,carry=0;
          for(double v:x) {const double term=distributions::dfpareto(v,q,a,p,b,min,true)-carry;
            const double next=ll+term;carry=(next-ll)-term;ll=next;}
          const double margin=4096*std::numeric_limits<double>::epsilon()*(x.size()+std::abs(face.logLik)+std::abs(ll));
          if(std::isfinite(ll)&&ll>face.logLik+margin) {
            r.status=Gb2Status::NumericalFailure;face_refuted=true;detail.candidates.clear();
            detail.candidates.push_back({{q,a,p,b,min},ll-margin,ll+margin});
            detail.loglik_lower=ll-margin;detail.loglik_upper=INFINITY;break;
          }
        }
        if(r.status==Gb2Status::NumericalFailure)break;
        if(!std::isnan(scale))break;
      }
    }
  }
  if(r.status==Gb2Status::DoubleParetoBoundary) {
    // The log-excess law is asymmetric Laplace. At a fixed kink c, its
    // two rates are n/[sqrt(L)(sqrt(L)+sqrt(R))] and its right analogue.
    std::vector<double> logs;for(double v:y)logs.push_back(std::log(v));
    std::sort(logs.begin(),logs.end());
    CPPUNIV_EXTENDED_REAL total=0,prefix=0;for(double v:logs)total+=v;
    double best=-INFINITY;std::vector<double> coefficients;
    const auto at=[&](double c,CPPUNIV_EXTENDED_REAL left,std::size_t k) {
      const double L=static_cast<double>(k*static_cast<CPPUNIV_EXTENDED_REAL>(c)-left);
      const double R=static_cast<double>(total-left-(logs.size()-k)*static_cast<CPPUNIV_EXTENDED_REAL>(c));
      if(!(L>0 && R>0))return;
      const double a=logs.size()/(std::sqrt(L)*(std::sqrt(L)+std::sqrt(R)));
      const double b=logs.size()/(std::sqrt(R)*(std::sqrt(L)+std::sqrt(R)));
      const double kink=std::isnan(scale)?std::exp(c):scale;
      if(!(a>0&&b>0&&kink>0&&std::isfinite(a)&&std::isfinite(b)&&std::isfinite(kink)))return;
      const double public_c=std::log(kink),normalizer=-std::log(1/a+1/b);
      double ll=0,carry=0;
      for(double v:logs) {
        const double term=normalizer+(v<public_c?a*(v-public_c):-b*(v-public_c))-v-carry;
        const double next=ll+term;carry=(next-ll)-term;ll=next;
      }
      if(std::isfinite(ll)&&ll>best){best=ll;coefficients={kink,a,b,min};}
    };
    if(std::isnan(scale))for(std::size_t k=0;k<logs.size();++k){at(logs[k],prefix,k);prefix+=logs[k];}
    else {const double c=std::log(scale);std::size_t k=0;for(double v:logs)if(v<c){prefix+=v;++k;}at(c,prefix,k);}
    if(coefficients.empty())r.status=Gb2Status::NumericalFailure;
    else {attach_limit(limit_detail(coefficients,best));r.logLik=best;}
  }
  for(auto& c:detail.candidates)if(c.model==StopModel::None && c.coef.size()==4)c.coef.push_back(min);
  return {r.shape1,r.shape2,r.shape3,r.scale,min,r.logLik,r.iterations,face_refuted?FparetoStatus::FaceRefuted:static_cast<FparetoStatus>(r.status),detail,r.certified,r.profile_evaluations};
}

inline bool family_point(const FparetoResult&, const FitCandidate& c) {
  if (c.model != StopModel::None || c.coef.size() != 5) return false;
  for (std::size_t i = 0; i < 5; ++i) if (!std::isfinite(c.coef[i]) || (i < 4 && !(c.coef[i] > 0))) return false;
  return true;
}
// CertificateTie transports the shifted GB2 certificate's witnesses, each with
// a likelihood enclosure (ll -+ error), not a converged point log likelihood.
inline bool accurate_point(const FparetoResult&, const FitCandidate&) { return false; }

inline FitOutcome outcome(const FparetoResult& r) {

  if (r.stop_detail.accuracy_stop == AccuracyStop::Tolerance) return FitOutcome::failure(OutcomeReason::Tolerance);
  if (r.stop_detail.accuracy_stop == AccuracyStop::Conditioning) return FitOutcome::failure(OutcomeReason::Conditioning);
  // The generic face search returns no coefficients and compares by a
  // likelihood margin; exact delegate faces retain held/limit coefficients.
  switch (r.status) {
    case FparetoStatus::FaceRefuted:return FitOutcome::failure(OutcomeReason::NoConvergence);
    case FparetoStatus::Unbounded:return FitOutcome::nonexistence(OutcomeReason::Unbounded);
    case FparetoStatus::UnsupportedThreshold:return FitOutcome::failure(OutcomeReason::Unsupported);
    case FparetoStatus::Ok: return FitOutcome::estimate(r.certified);
    case FparetoStatus::TrGammaBoundary: return FitOutcome::limit(r.certified && !(std::isnan(r.shape1) && std::isnan(r.shape2) && std::isnan(r.shape3) && std::isnan(r.scale)));
    case FparetoStatus::InvTrGammaBoundary: return FitOutcome::limit(r.certified && !(std::isnan(r.shape1) && std::isnan(r.shape2) && std::isnan(r.shape3) && std::isnan(r.scale)));
    case FparetoStatus::LognormalBoundary: return r.stop_detail.limit_coef.empty()?FitOutcome::failure(OutcomeReason::Evaluation):FitOutcome::limit(false);
    case FparetoStatus::ParetoBoundary: return FitOutcome::limit(r.certified && !(std::isnan(r.shape1) && std::isnan(r.shape2) && std::isnan(r.shape3) && std::isnan(r.scale)));
    case FparetoStatus::PowerBoundary: return FitOutcome::limit(r.certified && !(std::isnan(r.shape1) && std::isnan(r.shape2) && std::isnan(r.shape3) && std::isnan(r.scale)));
    case FparetoStatus::DoubleParetoBoundary: return FitOutcome::limit(false);
    case FparetoStatus::InvalidData: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case FparetoStatus::InvalidFixed: return FitOutcome::failure(OutcomeReason::InvalidArgument);
    case FparetoStatus::SearchUnresolved: return FitOutcome::failure(OutcomeReason::Unresolved);
    case FparetoStatus::ScaleOutOfRange: return FitOutcome::failure(OutcomeReason::Evaluation);
    case FparetoStatus::WeibullBoundary: return FitOutcome::limit(r.certified && !(std::isnan(r.shape1) && std::isnan(r.shape2) && std::isnan(r.shape3) && std::isnan(r.scale)));
    case FparetoStatus::InvWeibullBoundary: return FitOutcome::limit(r.certified && !(std::isnan(r.shape1) && std::isnan(r.shape2) && std::isnan(r.shape3) && std::isnan(r.scale)));
    case FparetoStatus::CertificateTie: return comparison_outcome_of(r);
    case FparetoStatus::CertificateBudget: return FitOutcome::budget();
    case FparetoStatus::NumericalFailure: return FitOutcome::failure(OutcomeReason::Evaluation);
  }
  return {};
}


}
