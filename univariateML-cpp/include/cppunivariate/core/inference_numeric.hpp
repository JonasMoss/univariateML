// Experimental numerical building block. Family-specific eligibility must be
// established by the caller; this header alone does not establish DQM or rank.
#pragma once
#include "inference.hpp"
#include "reparam.hpp"

namespace cppunivariate {
enum class NumericCoordinate { Real, Positive, UnitInterval, LowerBounded };
// A family adapter must derive this from the fitted point and free subset.
// Structural parameters (including design, threshold and endpoint kinds)
// must be absent from permitted_free_mask. An open or unclassified regime
// must not report Ok. The default is deliberately unavailable.
struct NumericEligibility {
  InferenceStatus status=InferenceStatus::FamilyUnavailable;
  unsigned permitted_free_mask=0;
  std::string reason;
};
struct NumericParameter {
  std::string name;
  double value=0;
  NumericCoordinate coordinate=NumericCoordinate::Real;
  double lower=0;
};
namespace inference_numeric_detail {
// A degenerate discrete submodel must still validate the supplied data.
inline InferenceResult singular_integer_support(span data,double lo,double hi,const std::string& reason) {
  InferenceResult r;r.observations=data.size();r.validity_conditions=reason;
  if(data.empty()) {r.status=InferenceStatus::EmptySample;return r;}
  for(double x:data) if(!std::isfinite(x) || x!=std::floor(x) || x<lo || x>hi) {
    r.status=InferenceStatus::InvalidObservation;return r;
  }
  r.status=InferenceStatus::Singular;return r;
}
inline InferenceResult singular_real_support(span data,const std::string& reason) {
  InferenceResult r;r.observations=data.size();r.validity_conditions=reason;
  if(data.empty()) {r.status=InferenceStatus::EmptySample;return r;}
  for(double x:data) if(!std::isfinite(x)) {r.status=InferenceStatus::InvalidObservation;return r;}
  r.status=InferenceStatus::Singular;return r;
}
inline double to_internal(const NumericParameter& p) {
  switch(p.coordinate) {
    case NumericCoordinate::Real: return p.value;
    case NumericCoordinate::Positive: return to_log(p.value);
    case NumericCoordinate::UnitInterval: return std::log(p.value)-std::log1p(-p.value);
    case NumericCoordinate::LowerBounded: return to_log_shift(p.value,p.lower);
  }
  return NAN;
}
inline double natural(double z,const NumericParameter& p) {
  switch(p.coordinate) {
    case NumericCoordinate::Real: return z;
    case NumericCoordinate::Positive: return from_log(z);
    case NumericCoordinate::UnitInterval:
      return z>=0 ? 1/(1+std::exp(-z)) : std::exp(z)/(1+std::exp(z));
    case NumericCoordinate::LowerBounded: return from_log_shift(z,p.lower);
  }
  return NAN;
}
inline double jacobian(double z,const NumericParameter& p) {
  const double v=natural(z,p);
  switch(p.coordinate) {
    case NumericCoordinate::Real: return 1;
    case NumericCoordinate::Positive: return v;
    case NumericCoordinate::UnitInterval: return v*(1-v);
    case NumericCoordinate::LowerBounded: return std::exp(z);
  }
  return NAN;
}
}
// log_density(x, theta) returns the original-data log density, with theta in
// README order. Only the active entries of theta are used by the callable.
// The caller must check family, regime, support, held design parameters, and
// successful interior fit before calling; this is not a public family dispatch.
// emit receives natural-coordinate per-observation scores in free order.
// A failure can occur after a prefix has been emitted; consume it only if
// the overall result is Ok. The callback must not throw.
template<class LogDensity, class Emit=inference_detail::Discard>
inline InferenceResult numeric_inference(span data,
    const std::array<NumericParameter,inference_max_parameters>& parameters,
    std::size_t count,unsigned mask,LogDensity log_density,Emit emit={}) {
  InferenceResult r; r.parameter_count=count; r.observations=data.size();r.free_mask=mask;
  r.fisher_status=InferenceStatus::NumericalFailure; r.fisher.fill(NAN);
  if(count>inference_max_parameters) {r.status=InferenceStatus::DimensionExceeded;return r;}
  if(mask >= (1u<<count)) {r.status=InferenceStatus::InvalidSubset;return r;}
  if(data.empty()) {r.status=InferenceStatus::EmptySample;return r;}
  std::array<double,inference_max_parameters> phi{},theta{};
  for(std::size_t j=0;j<count;++j) {
    r.parameter_names[j]=parameters[j].name;theta[j]=parameters[j].value;
    // Known truncation bounds can legitimately be infinite. A family density
    // must validate held infinities; free coordinates must remain finite.
    if(std::isnan(theta[j]) || ((mask & (1u<<j)) && !std::isfinite(theta[j]))) {
      r.status=InferenceStatus::InvalidParameter;return r;
    }
    if(mask & (1u<<j)) {
      phi[j]=inference_numeric_detail::to_internal(parameters[j]);
      if(!std::isfinite(phi[j])) {r.status=InferenceStatus::InvalidParameter;return r;}
      r.free_indices[r.dimension++]=j;
    }
  }
  const auto score_row=[&](double x,const std::array<double,inference_max_parameters>& at) {
    std::array<double,inference_max_parameters> row{},values=theta;
    for(std::size_t k=0;k<r.dimension;++k) {
      const auto j=r.free_indices[k];
      values[j]=inference_numeric_detail::natural(at[j],parameters[j]);
    }
    for(std::size_t k=0;k<r.dimension;++k) {
      const auto j=r.free_indices[k]; const double h=1e-4*(1+std::abs(at[j]));
      const auto difference=[&](double step) {
        const double center=inference_numeric_detail::natural(at[j],parameters[j]);
        const double up=inference_numeric_detail::natural(at[j]+step,parameters[j]);
        const double down=inference_numeric_detail::natural(at[j]-step,parameters[j]);
        // A rounded-to-boundary transform or coincident stencil would silently
        // turn a real derivative into zero; reject it before evaluating.
        if(!(std::isfinite(down) && std::isfinite(up) && down<center && center<up))
          return std::numeric_limits<double>::quiet_NaN();
        values[j]=up; const double plus=log_density(x,values);
        values[j]=down; const double minus=log_density(x,values);
        values[j]=center;
        return (plus-minus)/(2*step);
      };
      const double d1=difference(h),d2=difference(h/2);
      row[k]=(4*d2-d1)/(3*inference_numeric_detail::jacobian(at[j],parameters[j]));
    }
    return row;
  };
  for(double x:data) {
    if(!std::isfinite(x) || !std::isfinite(log_density(x,theta))) {r.status=InferenceStatus::InvalidObservation;return r;}
    const auto row=score_row(x,phi);
    for(std::size_t j=0;j<r.dimension;++j) {
      if(!std::isfinite(row[j])) {r.status=InferenceStatus::NumericalFailure;return r;}
      r.score[j]+=row[j];
      for(std::size_t k=0;k<r.dimension;++k) r.opg[j*r.dimension+k]+=row[j]*row[k];
    }
    emit(row);
  }
  for(std::size_t i=0;i<r.dimension;++i)
    if(!std::isfinite(r.score[i])) {r.status=InferenceStatus::NumericalFailure;return r;}
  for(std::size_t k=0;k<r.dimension;++k) {
    const auto j=r.free_indices[k];const double h=3e-4*(1+std::abs(phi[j]));
    auto plus=phi,minus=phi,halfplus=phi,halfminus=phi;
    plus[j]+=h;minus[j]-=h;halfplus[j]+=h/2;halfminus[j]-=h/2;
    std::array<double,inference_max_parameters> sp{},sm{},hp{},hm{};
    for(double x:data) {
      const auto a=score_row(x,plus),b=score_row(x,minus);
      const auto c=score_row(x,halfplus),d=score_row(x,halfminus);
      for(std::size_t i=0;i<r.dimension;++i) {sp[i]+=a[i];sm[i]+=b[i];hp[i]+=c[i];hm[i]+=d[i];}
    }
    const double jac=inference_numeric_detail::jacobian(phi[j],parameters[j]);
    for(std::size_t i=0;i<r.dimension;++i) {
      const double full=(sp[i]-sm[i])/(2*h),half=(hp[i]-hm[i])/h;
      r.observed[i*r.dimension+k]=-(4*half-full)/(3*jac);
    }
  }
  for(std::size_t i=0;i<r.dimension;++i) for(std::size_t j=0;j<i;++j) {
    const double v=(r.observed[i*r.dimension+j]+r.observed[j*r.dimension+i])/2;
    r.observed[i*r.dimension+j]=r.observed[j*r.dimension+i]=v;
  }
  r.opg_status=inference_detail::matrix_status(r.opg,r.dimension,true,r.opg_condition);
  r.observed_status=inference_detail::matrix_status(r.observed,r.dimension,false,r.observed_condition);
  if(r.opg_status==InferenceStatus::NumericalFailure || r.observed_status==InferenceStatus::NumericalFailure)
    r.status=InferenceStatus::NumericalFailure;
  return r;
}
// Checked adapter entry point: a family-specific assessment must be supplied.
// This does not replace that assessment or establish estimator asymptotics.
template<class LogDensity, class Emit=inference_detail::Discard>
inline InferenceResult numeric_inference_checked(span data,
    const std::array<NumericParameter,inference_max_parameters>& parameters,
    std::size_t count,unsigned mask,const NumericEligibility& eligibility,
    LogDensity log_density,Emit emit={}) {
  if(eligibility.status!=InferenceStatus::Ok || (mask & ~eligibility.permitted_free_mask)) {
    InferenceResult r;
    r.status=eligibility.status!=InferenceStatus::Ok ? eligibility.status : InferenceStatus::InvalidSubset;
    r.free_mask=mask; r.parameter_count=count; r.observations=data.size();
    r.validity_conditions=eligibility.reason;
    return r;
  }
  auto result=numeric_inference(data,parameters,count,mask,log_density,emit);
  result.validity_conditions=eligibility.reason;
  return result;
}
} // namespace cppunivariate
