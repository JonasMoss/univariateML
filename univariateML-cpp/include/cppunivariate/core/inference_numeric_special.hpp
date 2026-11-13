// Explicit conditional submodels; analytic moving-join adapters dispatch first.
#pragma once
#include "inference_numeric.hpp"
#include "inference_join.hpp"
#include "../distributions/laplace/dpqr.hpp"
#include "../distributions/ged/dpqr.hpp"
#include "../distributions/sged/dpqr.hpp"
#include "../distributions/cmb/dpqr.hpp"
#include "../distributions/cmp/dpqr.hpp"
namespace cppunivariate {
template<class Emit=inference_detail::Discard>
inline InferenceResult special_family_inference(span data,const std::string& family,
    const std::array<double,inference_max_parameters>& v,unsigned mask,Emit emit={}) {
  if(family=="snorm" || family=="sstd" || family=="huber" || family=="pig" || family=="gpd" || family=="gev")
    return join_family_inference(data,family,v,mask,emit);
  std::size_t n=0;unsigned allowed=0;
  if(family=="laplace") {n=2;allowed=2;}
  else if(family=="ged") {n=3;allowed=7;}
  else if(family=="sged") {n=4;allowed=15;}
  else if(family=="cmb") {n=3;allowed=6;}
  else if(family=="cmp") {n=2;allowed=3;}
  if(!n) {
    InferenceResult r;r.status=InferenceStatus::FamilyUnavailable;
    if(family=="triang") r.validity_conditions="the mode has a moving kink and endpoint directions are non-DQM";
    else if(family=="unif") r.validity_conditions="both endpoint coordinates are non-DQM and excluded";
    else if(family=="dunif" || family=="hyper" || family=="nhyper") r.validity_conditions="all potentially free coefficients are integer valued";
    else if(family=="tlambda") r.validity_conditions="DQM and full rank hold for lambda < 1/2; support-aware numerical derivatives and fitted covariance validation are not yet implemented";
    else r.validity_conditions="family inference not classified or implemented";
    return r;
  }
  if(mask >= (1u<<n)) {InferenceResult r;r.status=InferenceStatus::InvalidSubset;return r;}
  if(mask & ~allowed) {InferenceResult r;r.status=InferenceStatus::RegimeUnavailable;r.validity_conditions="the implemented derivative scope requires this coefficient to be held (nonsmooth, design, threshold or unvalidated direction)";return r;}
  std::array<NumericParameter,inference_max_parameters> p{};
  const auto set=[&](std::size_t i,const std::string& name,NumericCoordinate kind,double lower=0) {p[i]={name,v[i],kind,lower};};
  bool valid=true,regime=true;
  if(family=="laplace") {
    set(0,"mu",NumericCoordinate::Real);set(1,"sigma",NumericCoordinate::Positive);
    valid=std::isfinite(v[0]) && std::isfinite(v[1]) && v[1]>0;
  } else if(family=="ged" || family=="sged") {
    set(0,"mean",NumericCoordinate::Real);set(1,"sd",NumericCoordinate::Positive);set(2,"nu",NumericCoordinate::Positive);
    if(n==4) set(3,"xi",NumericCoordinate::Positive);
    valid=std::isfinite(v[0]) && std::isfinite(v[1]) && v[1]>0 && std::isfinite(v[2]) && v[2]>0 && (n==3 || (std::isfinite(v[3]) && v[3]>0));
    const bool symmetric=family=="ged" || (v[3]==1 && !(mask & 8u));
    regime=symmetric ? (!(mask & 1u) || v[2]>2 || (v[2]==2 && !(mask & 4u))) : v[2]>2;
    if((mask & 4u) && v[2]<=1) regime=false;
    if(mask & 4u) {
      p[2].coordinate=NumericCoordinate::LowerBounded;
      p[2].lower=(!symmetric || (mask & 1u)) ? 2 : 1;
    }
  } else if(family=="cmb") {
    set(0,"size",NumericCoordinate::Real);set(1,"prob",NumericCoordinate::UnitInterval);
    set(2,"nu",NumericCoordinate::Real);
    const bool prob_interior=v[1]>0 && v[1]<1;
    if(family=="cmb" && v[0]==0 && mask && prob_interior && std::isfinite(v[2]))
      return inference_numeric_detail::singular_integer_support(data,0,0,"size zero is a point mass");
    const bool known_binomial_edge=false;
    valid=std::isfinite(v[0]) && v[0]>=1 && v[0]==std::floor(v[0]) && (prob_interior || known_binomial_edge) && std::isfinite(v[2]) &&
      (family=="cmb" || (v[2]>0 && v[2]<1) || ((v[2]==0 || v[2]==1) && !(mask & 4u)));
    if(valid && false && v[2]==1 && mask) {
      return inference_numeric_detail::singular_integer_support(data,0,0,"known p0 = 1 yields a point mass");
    }
    if(valid && v[0]==1 && ((family=="cmb" && (mask & 4u)) || (false && (mask & 2u)))) {
      const double lo=false && v[2]==0 ? 1 : 0;
      return inference_numeric_detail::singular_integer_support(data,lo,1,"a free coefficient has zero information at size one");
    }
  } else if(family=="cmp") {
    set(0,"lambda",v[1]==0?NumericCoordinate::UnitInterval:NumericCoordinate::Positive);
    set(1,"nu",NumericCoordinate::Positive);
    valid=std::isfinite(v[0]) && v[0]>0 && std::isfinite(v[1]) &&
      (v[1]>0 || (v[1]==0 && !(mask & 2u) && v[0]<1));
  }
  if(!valid) {InferenceResult r;r.status=InferenceStatus::BoundaryOrLimit;r.validity_conditions="invalid or boundary parameters for the classified scope";return r;}
  if(!regime) {
    InferenceResult r;r.status=InferenceStatus::RegimeUnavailable;
    if(family=="ged" || family=="sged") r.validity_conditions="DQM holds above nu = 1/2; ordinary moving-location Hessians are invalid for nu <= 1; remaining excluded fitted cusp derivatives are unvalidated or on a fitting restriction";
    else r.validity_conditions="the requested held subset or regime is not classified for ordinary observed information";
    return r;
  }
  const auto density=[&](double x,const auto& a) {
    if(family=="laplace") return distributions::dlaplace(x,a[0],a[1],true);
    if(family=="ged") return distributions::dged(x,a[0],a[1],a[2],true);
    if(family=="sged") return distributions::dsged(x,a[0],a[1],a[2],a[3],true);
    if(family=="cmb") return distributions::dcmb(x,a[0],a[1],a[2],true);
    if(family=="cmp") return distributions::dcmp(x,a[0],a[1],true);
    return std::numeric_limits<double>::quiet_NaN();
  };
  const NumericEligibility eligibility{InferenceStatus::Ok,allowed,"classified interior submodel; structural coefficients held"};
  return numeric_inference_checked(data,p,n,mask,eligibility,density,emit);
}
} // namespace cppunivariate
