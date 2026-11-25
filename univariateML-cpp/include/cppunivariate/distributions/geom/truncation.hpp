// Exponential-family traits for closed-window truncation.
#pragma once
#include "../../core/outcome.hpp"
#include "../../core/discrete_stop.hpp"
#include "../../core/truncation.hpp"
#include "../../special/special.hpp"
namespace cppunivariate {
struct GeomTruncationTraits {
  double size=1;
  double lower() const {return 0.;}
  double upper() const {return INFINITY;}
  bool valid() const {return true;}
  bool valid_parameter(double p) const {return p>0 && p<=1 && std::isfinite(p);}
  double natural(double p) const {return std::log1p(-p);}
  double coefficient(double e) const {return -std::expm1(e);}
  double mode(double e) const {return 0.;}
  double kernel(double k,double e) const {return k*e;}
  double ratio(double k,double e) const {return std::exp(e);}
  double tail_bound(double r,double e,int direction) const {return direction>0?std::max(r,r):r;}
  double atom(double p) const {return p==1?0:NAN;}
  double start(double m) const {return std::log(m/(m+1));}
  double eta_lower() const {return -744.;}
  double eta_upper() const {return -1e-16;}
  double lower_member() const {return 1.;}
  double upper_member() const {return NAN;}
  bool positive_valid() const {return true;}
  double boundary_mean(double lo,double hi) const {return lo+(hi-lo)/2;}
  double positive_member() const {return 1.;}
  bool positive_special() const {return false;}
  TruncationResult positive_fit(ispan,double,double,int) const {return {NAN,NAN,0,TruncationStatus::NotImplemented};}
  double reference() const {return .5;}
};
}

namespace cppunivariate {
// Shared closed-window result used by the count modifier routes. Boundary
// statuses retain the seam's existing plain-error rendering.
inline FitOutcome outcome(const TruncationResult& r) {
  switch(r.status) {
    case TruncationStatus::Ok: return FitOutcome::singleton(true);
    case TruncationStatus::InvalidInput: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case TruncationStatus::NotImplemented: return FitOutcome::failure(OutcomeReason::Unsupported);
    case TruncationStatus::LowerLimit: return FitOutcome::failure(OutcomeReason::NoConvergence);
    case TruncationStatus::UpperLimit: return FitOutcome::failure(OutcomeReason::NoConvergence);
    case TruncationStatus::NumericalFailure: return FitOutcome::failure(OutcomeReason::NoConvergence);
  }
  return {};
}
} // namespace cppunivariate

namespace cppunivariate {
// Retain the generic solver's status while distinguishing analytic boundary
// tests from hitting its finite natural-coordinate bracket.
enum class CountWindowModel { None, Uniform };
struct CountWindowResult {
  double parameter, logLik;
  int iterations;
  TruncationStatus status;
  StopDetail stop_detail{};
  FitOutcome decision{};
  CountWindowModel model=CountWindowModel::None;
};
inline FitOutcome outcome(const CountWindowResult& r) { return r.decision; }
template<class Traits> inline CountWindowResult count_window_mle(ispan x,double a,double b,
    Traits traits,double held=NAN,double tol=1e-10,int maxit=200) {
  if(!x.empty()) {
    if(std::isnan(a))a=*std::min_element(x.begin(),x.end());
    if(std::isnan(b))b=*std::max_element(x.begin(),x.end());
  }
  const auto base=truncated<Traits>::fit(x,a,b,traits,held,tol,maxit);
  CountWindowResult r{base.parameter,base.logLik,base.iterations,base.status,{},outcome(base)};
  if(base.status==TruncationStatus::Ok && std::isnan(held) &&
     std::max(traits.lower(),std::ceil(a))==std::min(traits.upper(),std::floor(b)))
    r.decision=FitOutcome::estimate(true,false);
  if(base.status!=TruncationStatus::LowerLimit && base.status!=TruncationStatus::UpperLimit)return r;
  r.decision=FitOutcome::failure(OutcomeReason::Evaluation);
  r.status=TruncationStatus::NumericalFailure;
  const double lo=std::max(traits.lower(),std::ceil(a)),hi=std::min(traits.upper(),std::floor(b));
  bool lower=true,upper=true;
  double offset=0;
  for(int v:x) {lower=lower && v==lo;upper=upper && v==hi;offset+=(v-lo)/double(x.size());}
  if(lower || (upper && !(traits.eta_upper()<0))) {
    r.status=base.status;
    r.stop_detail=discrete_point(lower?lo:hi);
    r.decision=FitOutcome::limit();
  } else if(base.status==TruncationStatus::UpperLimit && traits.eta_upper()<0 && std::isfinite(hi)) {
    const double mean=traits.boundary_mean(lo,hi);
    const auto mass=truncated<Traits>::mass(traits,0,lo,hi,maxit);
    if((std::isfinite(mean) && offset>=mean-lo) ||
       (!std::isfinite(mean) && mass.ok && mass.offset<=(lo-mass.mode)+offset)) {
      r.status=base.status;
      r.decision=FitOutcome::nonexistence(OutcomeReason::Unattained);
    }
    if(r.decision.kind==OutcomeKind::Nonexistence && traits.size==1) {
      r.model=CountWindowModel::Uniform;
      r.stop_detail.limit_coef={lo,hi};
      r.stop_detail.limit_loglik=-double(x.size())*std::log(hi-lo+1);
      r.stop_detail.loglik_lower=r.stop_detail.limit_loglik;
      r.stop_detail.loglik_upper=r.stop_detail.limit_loglik;
      r.decision=FitOutcome::limit();
    }
  }
  return r;
}
}

namespace cppunivariate {
enum class HeldWindowStatus { Ok, InvalidInput, NumericalFailure, Empty, Nonfinite, Unbounded, EvaluationFailure };
struct HeldWindowResult { double logLik=NAN; HeldWindowStatus status=HeldWindowStatus::InvalidInput; };
inline FitOutcome outcome(const HeldWindowResult& r) {
  return r.status==HeldWindowStatus::Unbounded?FitOutcome::nonexistence(OutcomeReason::Unbounded):r.status==HeldWindowStatus::Ok?FitOutcome::singleton(true):
    FitOutcome::failure(r.status!=HeldWindowStatus::NumericalFailure && r.status!=HeldWindowStatus::EvaluationFailure?OutcomeReason::InvalidInput:OutcomeReason::Evaluation);
}
template<class Sample,class Density> inline HeldWindowResult held_window_likelihood(Sample x,Density density,bool validated_support=false) {
  if(x.empty())return {NAN,HeldWindowStatus::Empty};
  double sum=0,correction=0;
  for(double v:x) {
    if(!std::isfinite(v))return {NAN,HeldWindowStatus::Nonfinite};
    const double term=density(v);
    if(!std::isfinite(term))return {NAN,validated_support?HeldWindowStatus::EvaluationFailure:HeldWindowStatus::InvalidInput};
    const double y=term-correction,z=sum+y;correction=(z-sum)-y;sum=z;
  }
  return {sum,std::isfinite(sum)?HeldWindowStatus::Ok:HeldWindowStatus::NumericalFailure};
}
}

namespace cppunivariate {
// Count routes accept raw real observations before integer conversion. A
// successful conversion is not a fit; failed preparation carries its rejection.
enum class CountInputStatus { Ok, InvalidInput };
struct CountInputResult {
  std::vector<int> values;
  CountInputStatus status=CountInputStatus::Ok;
  double lower=NAN,upper=NAN;
};
inline FitOutcome outcome(const CountInputResult& r) {
  return FitOutcome::failure(r.status==CountInputStatus::InvalidInput?
    OutcomeReason::InvalidInput:OutcomeReason::Evaluation);
}
inline CountInputResult count_observations(span x,bool window=false,double lower=NAN,double upper=NAN) {
  CountInputResult r;r.lower=lower;r.upper=upper;
  r.values.reserve(x.size());
  for(double v:x) {
    if(!std::isfinite(v) || v<0 || v>std::numeric_limits<int>::max() || v!=std::floor(v)) {
      r.status=CountInputStatus::InvalidInput;return r;
    }
    r.values.push_back(static_cast<int>(v));
  }
  if(window && !r.values.empty()) {
    if(std::isnan(r.lower))r.lower=*std::min_element(r.values.begin(),r.values.end());
    if(std::isnan(r.upper))r.upper=*std::max_element(r.values.begin(),r.values.end());
  }
  return r;
}
}
