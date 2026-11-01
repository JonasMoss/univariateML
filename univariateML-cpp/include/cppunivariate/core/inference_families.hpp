// Preliminary family dispatch. Only the explicit scopes below are classified;
// all other family names return FamilyUnavailable. The caller remains responsible
// for supplying a successful fit and for recording fitted versus held parameters.
#pragma once
#include "inference_numeric.hpp"
#include "inference_numeric_generated.hpp"
#include "inference_numeric_special.hpp"
#include "../distributions/norm/score.hpp"
#include "../distributions/gamma/score.hpp"
#include "../distributions/beta/score.hpp"
#include "../distributions/pois/score.hpp"
#include "../distributions/exp/dpqr.hpp"
#include "../distributions/geom/dpqr.hpp"
#include "../distributions/lnorm/dpqr.hpp"
#include "../distributions/logitnorm/dpqr.hpp"
#include "../distributions/lgamma/dpqr.hpp"
#include "../distributions/rayleigh/dpqr.hpp"
#include "../distributions/binom/dpqr.hpp"

namespace cppunivariate {
// Values are in README order. mask marks coefficients to estimate; held
// coefficients, including binomial size, are treated as known constants.
// Emits one fixed-capacity row; only the first result.dimension entries count.
template<class Emit=inference_detail::Discard>
inline InferenceResult family_inference(span data,const std::string& family,
    const std::array<double,inference_max_parameters>& value,unsigned mask,Emit emit={}) {
  std::size_t count=0;
  if(family=="norm" || family=="gamma" || family=="beta" || family=="lnorm" ||
     family=="logitnorm" || family=="lgamma" || family=="binom") count=2;
  else if(family=="pois" || family=="exp" || family=="geom" || family=="rayleigh") count=1;
  if(!count) {
    const auto generated=generated_family_inference(data,family,value,mask,emit);
    if(generated.status!=InferenceStatus::FamilyUnavailable) return generated;
    return special_family_inference(data,family,value,mask,emit);
  }
  if(mask >= (1u<<count)) {InferenceResult out;out.status=InferenceStatus::InvalidSubset;return out;}
  if(family=="binom" && (mask & 1u)) {
    InferenceResult out;out.status=InferenceStatus::InvalidSubset;
    out.validity_conditions="binomial size is a held integer design coefficient";return out;
  }
  for(std::size_t i=0;i<count;++i) if(!std::isfinite(value[i])) {
    InferenceResult out;out.status=InferenceStatus::BoundaryOrLimit;
    out.validity_conditions="finite interior model coefficients are required";return out;
  }
  const bool interior=(family=="norm" || family=="lnorm" || family=="logitnorm") ? value[1]>0 :
    (family=="gamma" || family=="beta" || family=="lgamma") ? value[0]>0 && value[1]>0 :
    family=="binom" ? value[0]>=1 && value[0]==std::floor(value[0]) && value[1]>0 && value[1]<1 :
    family=="geom" ? value[0]>0 && value[0]<1 : value[0]>0;
  if(!interior) {InferenceResult out;out.status=InferenceStatus::BoundaryOrLimit;
    out.validity_conditions="the family scope requires interior parameters and identifiable design";return out;}
  // Preserve the four analytic implementations, including their Fisher matrix.
  const auto forward=[&](const auto& source) {
    std::array<double,inference_max_parameters> row{};
    for(std::size_t i=0;i<source.size() && i<inference_max_parameters;++i) row[i]=source[i];
    emit(row);
  };
  if(family=="norm") return norm_inference(data,value[0],value[1],mask,forward);
  if(family=="gamma") return gamma_inference(data,value[0],value[1],mask,forward);
  if(family=="beta") return beta_inference(data,value[0],value[1],mask,forward);
  if(family=="pois") return pois_inference(data,value[0],mask,forward);
  std::array<NumericParameter,inference_max_parameters> p{};
  if(family=="exp") p[0]={"rate",value[0],NumericCoordinate::Positive,0};
  if(family=="geom") p[0]={"prob",value[0],NumericCoordinate::UnitInterval,0};
  if(family=="rayleigh") p[0]={"sigma",value[0],NumericCoordinate::Positive,0};
  if(family=="lnorm") {p[0]={"meanlog",value[0],NumericCoordinate::Real,0};p[1]={"sdlog",value[1],NumericCoordinate::Positive,0};}
  if(family=="logitnorm") {p[0]={"mu",value[0],NumericCoordinate::Real,0};p[1]={"sigma",value[1],NumericCoordinate::Positive,0};}
  if(family=="lgamma") {p[0]={"shapelog",value[0],NumericCoordinate::Positive,0};p[1]={"ratelog",value[1],NumericCoordinate::Positive,0};}
  if(family=="binom") {p[0]={"size",value[0],NumericCoordinate::Real,0};p[1]={"prob",value[1],NumericCoordinate::UnitInterval,0};}
  const auto log_density=[&](double x,const auto& a) {
    if(family=="exp") return distributions::dexp(x,a[0],true);
    if(family=="geom") return distributions::dgeom(x,a[0],true);
    if(family=="rayleigh") return distributions::drayleigh(x,a[0],true);
    if(family=="lnorm") return distributions::dlnorm(x,a[0],a[1],true);
    if(family=="logitnorm") return distributions::dlogitnorm(x,a[0],a[1],true);
    if(family=="lgamma") return distributions::dlgamma(x,a[0],a[1],true);
    if(family=="binom") return distributions::dbinom(x,a[0],a[1],true);
    return std::numeric_limits<double>::quiet_NaN();
  };
  const NumericEligibility eligibility{InferenceStatus::Ok,family=="binom"?2u:((1u<<count)-1u),
    family=="binom"?"known integer size >= 1; interior probability":"ordinary finite interior parameters"};
  return numeric_inference_checked(data,p,count,mask,eligibility,log_density,forward);
}
} // namespace cppunivariate
