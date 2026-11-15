#pragma once
#include "dpqr.hpp"
#include "../../core/outcome.hpp"
#include "../../core/work_budget.hpp"
#include "../burr/mle.hpp"
#include "../lomax/mle.hpp"
#include <vector>
#include <cmath>
namespace cppunivariate {
enum class Pareto2Status { Ok, ExponentialLimit, InvalidStart, IterationFailure, AccuracyFailure,
  Unbounded, Unattained, UnsupportedThreshold, LimitRepresentationFailure };
struct Pareto2Result { double shape,scale,min,logLik; int iterations; Pareto2Status status; StopDetail stop_detail{}; LomaxCertificateFailure certificate_failure = LomaxCertificateFailure::None; };

inline Pareto2Result pareto2_mle(span x, double min, double tol, int maxit, double shape=std::numeric_limits<double>::quiet_NaN(), double scale=std::numeric_limits<double>::quiet_NaN()) {
  if(std::isnan(min)) {return {NAN,NAN,min,NAN,0,Pareto2Status::UnsupportedThreshold};}
  maxit = static_cast<int>(solver_budget(maxit));
  std::vector<double> y(x.size());
  for (std::size_t i=0;i<x.size();++i) y[i]=(x[i]-min);
  bool valid=tol>0 && std::isfinite(tol) && maxit>0 && std::isfinite(min);
  for(double v:x) valid=valid && std::isfinite(v) && v>=min;
  for(double value:{shape,scale})if(!std::isnan(value)&&!(value>0 && std::isfinite(value)))valid=false;
  if(valid && !x.empty()) {
    std::size_t zeros=0;for(double value:x)if(value==min)++zeros;
    if(zeros) {
      const double n=static_cast<double>(x.size());
      const bool free_shape=std::isnan(shape),free_scale=std::isnan(scale);
      if((zeros==x.size() && (free_shape||free_scale)) || (free_shape&&free_scale))
        return {shape,scale,min,INFINITY,0,Pareto2Status::Unbounded};
      const auto point=[&](double a,double b,int iterations,double input_error=0) {
        double ll=0,carry=0;
        for(double v:x){const double term=distributions::dpareto2(v,a,b,min,true)-carry;
          const double next=ll+term;carry=(next-ll)-term;ll=next;}
        Pareto2Result out{a,b,min,ll,iterations,std::isfinite(ll)&&a>0&&b>0&&std::isfinite(a)&&std::isfinite(b)?Pareto2Status::Ok:Pareto2Status::IterationFailure};
        if(out.status!=Pareto2Status::Ok)out.certificate_failure=LomaxCertificateFailure::Arithmetic;
        if(out.status==Pareto2Status::Ok)record_accuracy(out,tol,{a,b,min},{free_shape?a:0,free_scale?b:0,0},input_error);
        if(out.stop_detail.accuracy_stop!=AccuracyStop::None)out.status=Pareto2Status::AccuracyFailure;
        return out;
      };
      if(!free_scale) {
        if(!free_shape)return point(shape,scale,0);
        double sum=0,carry=0,max_logratio=0;
        for(double v:x)if(v>min) {
          const double logratio=special::log_abs_difference(v,min)-std::log(scale);
          max_logratio=std::max(max_logratio,std::abs(logratio));
          const double term=(std::max(logratio,0.)+std::log1p(std::exp(-std::abs(logratio))))-carry;
          const double next=sum+term;carry=(next-sum)-term;sum=next;
        }
        return point(n/sum,scale,0,64*std::numeric_limits<double>::epsilon()*(1+n+max_logratio));
      }
      const double rest=static_cast<double>(x.size()-zeros),k=static_cast<double>(zeros);
      const double rounded=shape*rest,residual=std::fma(shape,rest,-k);
      const int order=rounded<k?-1:rounded>k?1:residual<0?-1:residual>0?1:0;
      if(order<0)return {shape,scale,min,INFINITY,0,Pareto2Status::Unbounded};
      if(order==0) {
        double face=n*std::log(shape),magnitude=std::abs(face);
        for(double v:x)if(v>min){const double term=(shape+1)*special::log_abs_difference(v,min);face-=term;magnitude+=std::abs(term);}
        Pareto2Result out{shape,0,min,face,0,Pareto2Status::Unattained};
        const double margin=512*std::numeric_limits<double>::epsilon()*(n+magnitude);
        out.stop_detail.loglik_lower=face-margin;out.stop_detail.loglik_upper=face+margin;return out;
      }
      struct Score {double value,margin;};
      const auto score=[&](double logscale) {
        double sum=0,carry=0,magnitude=0;
        for(double v:x)if(v>min) {
          const double t=special::log_abs_difference(v,min)-logscale;
          const double logistic=t>0?std::exp(-t)/(1+std::exp(-t)):1/(1+std::exp(t));
          const double term=logistic-carry,next=sum+term;
          carry=(next-sum)-term;sum=next;magnitude+=logistic;
        }
        const double base=std::isfinite(residual)?residual/(shape+1):rest-n/(shape+1);
        return Score{base-sum,128*std::numeric_limits<double>::epsilon()*(n+magnitude+std::abs(base))};
      };
      double lo=std::log(std::numeric_limits<double>::denorm_min()),hi=std::log(std::numeric_limits<double>::max());
      if(score(lo).value<=score(lo).margin || score(hi).value>=-score(hi).margin)
        return {shape,NAN,min,NAN,0,Pareto2Status::IterationFailure,{},LomaxCertificateFailure::Arithmetic};
      int iterations=0;
      for(;iterations<maxit;++iterations) {
        const double mid=lo/2+hi/2;const auto value=score(mid);
        if(value.value>value.margin)lo=mid;
        else if(value.value< -value.margin)hi=mid;
        else {
          const double left=mid-tol/8,right=mid+tol/8;const auto a=score(left),b=score(right);
          if(a.value>a.margin && b.value< -b.margin){lo=left;hi=right;}
          else return {shape,NAN,min,NAN,iterations,Pareto2Status::AccuracyFailure};
        }
        if(hi-lo<=tol/2)return point(shape,std::exp(lo/2+hi/2),iterations+1);
        if(mid==lo && mid==hi)break;
      }
      return {shape,NAN,min,NAN,iterations,Pareto2Status::IterationFailure};
    }
    for(double v:y)if(!std::isfinite(v))return {shape,scale,min,NAN,0,Pareto2Status::IterationFailure,{},LomaxCertificateFailure::Arithmetic};
  }
  if(!valid) y.clear();
  const auto r=lomax_mle(span{y.data(),y.size()},tol,maxit,NAN,
    std::isnan(scale)?scale:1/scale,shape);
  auto detail=r.stop_detail;
  for(auto& c:detail.candidates) if(c.model==StopModel::None && c.coef.size()==2)
    c.coef={c.coef[1],std::isnan(scale)?1/c.coef[0]:scale,min};
  const double fitted_scale=std::isnan(scale)?1/r.lambda:scale;
  if((r.status==LomaxStatus::Ok || r.status==LomaxStatus::AccuracyFailure) &&
      (!(r.kappa>0 && fitted_scale>0) || !std::isfinite(r.kappa) || !std::isfinite(fitted_scale))) {
    detail.accuracy_stop=AccuracyStop::None;detail.attainable_tol=NAN;detail.conditioning_bound=NAN;
    detail.candidates.erase(std::remove_if(detail.candidates.begin(),detail.candidates.end(),
      [](const FitCandidate& c){return c.model==StopModel::None &&
        (c.coef.size()!=3 || !(c.coef[0]>0 && c.coef[1]>0) ||
         !std::isfinite(c.coef[0]) || !std::isfinite(c.coef[1]) || !std::isfinite(c.coef[2]));}),detail.candidates.end());
    return {r.kappa,fitted_scale,min,r.logLik,r.iterations,Pareto2Status::IterationFailure,detail,LomaxCertificateFailure::Arithmetic};
  }
  if(r.status==LomaxStatus::ExponentialLimit && (detail.limit_coef.empty() || !std::isfinite(detail.limit_coef[0]))) {
    Pareto2Result out{r.kappa,fitted_scale,min,r.logLik,r.iterations,Pareto2Status::IterationFailure,{},LomaxCertificateFailure::Arithmetic};
    out.status=Pareto2Status::LimitRepresentationFailure;return out;
  }
  Pareto2Result out{r.kappa,fitted_scale,min,r.logLik,r.iterations,static_cast<Pareto2Status>(r.status),detail,r.certificate_failure};
  if(out.stop_detail.accuracy_stop!=AccuracyStop::None && out.shape>0 && out.scale>0)
    record_accuracy(out,tol,{out.shape,out.scale,min},{std::isnan(shape)?out.shape:0,std::isnan(scale)?out.scale:0,0});
  return out;
}

inline FitOutcome outcome(const Pareto2Result& r) {
  if (r.stop_detail.accuracy_stop == AccuracyStop::Tolerance) return FitOutcome::failure(OutcomeReason::Tolerance);
  if (r.stop_detail.accuracy_stop == AccuracyStop::Conditioning) return FitOutcome::failure(OutcomeReason::Conditioning);
  switch (r.status) {
    case Pareto2Status::Unbounded:return FitOutcome::nonexistence(OutcomeReason::Unbounded);
    case Pareto2Status::Unattained:return FitOutcome::nonexistence(OutcomeReason::Unattained);
    case Pareto2Status::UnsupportedThreshold:return FitOutcome::failure(OutcomeReason::Unsupported);
    case Pareto2Status::LimitRepresentationFailure:return FitOutcome::failure(OutcomeReason::Evaluation);
    case Pareto2Status::Ok: return FitOutcome::estimate(true);
    case Pareto2Status::ExponentialLimit: return FitOutcome::limit(false); // lomax decides its stop by a 1e-8 n margin (lomax/certificate.hpp), as lomax and invpareto report
    case Pareto2Status::InvalidStart: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case Pareto2Status::IterationFailure: return r.certificate_failure == LomaxCertificateFailure::Budget ? FitOutcome::budget() : FitOutcome::failure(r.certificate_failure==LomaxCertificateFailure::Arithmetic?OutcomeReason::Evaluation:OutcomeReason::NoConvergence);
    case Pareto2Status::AccuracyFailure: return FitOutcome::failure(OutcomeReason::Evaluation);
  }
  return {};
}


}
