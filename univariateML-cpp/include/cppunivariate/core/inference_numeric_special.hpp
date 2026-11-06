// Explicit conditional submodels. Their numerical derivatives and calibration
// remain to be checked independently before general inference is advertised.
#pragma once
#include "inference_numeric.hpp"
#include "../distributions/laplace/dpqr.hpp"
#include "../distributions/gev/dpqr.hpp"
#include "../distributions/gpd/dpqr.hpp"
#include "../distributions/ged/dpqr.hpp"
#include "../distributions/sged/dpqr.hpp"
#include "../distributions/snorm/dpqr.hpp"
#include "../distributions/sstd/dpqr.hpp"
#include "../distributions/tnorm/dpqr.hpp"
#include "../distributions/cmb/dpqr.hpp"
#include "../distributions/cmp/dpqr.hpp"
#include "../distributions/tbinom/dpqr.hpp"
#include "../distributions/tpois/dpqr.hpp"
#include "../distributions/zmbinom/dpqr.hpp"
namespace cppunivariate {
template<class Emit=inference_detail::Discard>
inline InferenceResult special_family_inference(span data,const std::string& family,
    const std::array<double,inference_max_parameters>& v,unsigned mask,Emit emit={}) {
  std::size_t n=0;unsigned allowed=0;
  if(family=="laplace") {n=2;allowed=2;}
  else if(family=="gev") {n=3;allowed=3;}
  else if(family=="gpd") {n=3;allowed=6;}
  else if(family=="ged") {n=3;allowed=7;}
  else if(family=="sged") {n=4;allowed=15;}
  else if(family=="snorm") {n=3;allowed=3;}
  else if(family=="sstd") {n=4;allowed=7;}
  else if(family=="tnorm") {n=4;allowed=3;}
  else if(family=="cmb") {n=3;allowed=6;}
  else if(family=="cmp") {n=2;allowed=3;}
  else if(family=="tbinom") {n=4;allowed=1;}
  else if(family=="tpois") {n=3;allowed=1;}
  else if(family=="zmbinom") {n=3;allowed=6;}
  if(!n) {
    InferenceResult r;r.status=InferenceStatus::FamilyUnavailable;
    if(family=="triang") r.validity_conditions="the mode has a moving kink and endpoint directions are non-DQM";
    else if(family=="huber") r.validity_conditions="DQM and full-rank information hold; join-aware numerical derivatives and fitted covariance validation are not yet implemented";
    else if(family=="unif") r.validity_conditions="both endpoint coordinates are non-DQM and excluded";
    else if(family=="dunif" || family=="hyper" || family=="nhyper") r.validity_conditions="all potentially free coefficients are integer valued";
    else if(family=="pig") r.validity_conditions="DQM and score-tail domination hold; numerical derivatives and covariance validation are not yet implemented";
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
  } else if(family=="gev") {
    set(0,"mu",NumericCoordinate::Real);set(1,"sigma",NumericCoordinate::Positive);set(2,"xi",NumericCoordinate::Real);
    valid=std::isfinite(v[0]) && std::isfinite(v[1]) && v[1]>0 && std::isfinite(v[2]);regime=v[2]==0;
  } else if(family=="gpd") {
    set(0,"mu",NumericCoordinate::Real);set(1,"sigma",NumericCoordinate::Positive);
    set(2,"xi",(mask & 4u)?NumericCoordinate::Positive:NumericCoordinate::Real);
    valid=std::isfinite(v[0]) && std::isfinite(v[1]) && v[1]>0 && std::isfinite(v[2]);
    regime=v[2]>0 || (v[2]==0 && !(mask & 4u));
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
  } else if(family=="snorm" || family=="sstd") {
    set(0,"mean",NumericCoordinate::Real);set(1,"sd",NumericCoordinate::Positive);
    if(family=="snorm") set(2,"xi",NumericCoordinate::Positive);
    else {set(2,"nu",NumericCoordinate::LowerBounded,2);set(3,"xi",NumericCoordinate::Positive);}
    const std::size_t skew=n-1;
    valid=std::isfinite(v[0]) && std::isfinite(v[1]) && v[1]>0 && std::isfinite(v[skew]) && v[skew]>0 && (n==3 || (std::isfinite(v[2]) && v[2]>2));
    regime=v[skew]==1;
  } else if(family=="tnorm") {
    set(0,"mean",NumericCoordinate::Real);set(1,"sd",NumericCoordinate::Positive);
    set(2,"a",NumericCoordinate::Real);set(3,"b",NumericCoordinate::Real);
    valid=std::isfinite(v[0]) && std::isfinite(v[1]) && v[1]>0 && !std::isnan(v[2]) && !std::isnan(v[3]) && v[2]<v[3];
  } else if(family=="cmb" || family=="zmbinom") {
    set(0,"size",NumericCoordinate::Real);set(1,"prob",NumericCoordinate::UnitInterval);
    set(2,family=="cmb"?"nu":"p0",family=="cmb"?NumericCoordinate::Real:NumericCoordinate::UnitInterval);
    const bool prob_interior=v[1]>0 && v[1]<1;
    if(family=="cmb" && v[0]==0 && mask && prob_interior && std::isfinite(v[2]))
      return inference_numeric_detail::singular_integer_support(data,0,0,"size zero is a point mass");
    const bool known_binomial_edge=family=="zmbinom" && (v[1]==0 || v[1]==1) && !(mask & 2u);
    valid=std::isfinite(v[0]) && v[0]>=1 && v[0]==std::floor(v[0]) && (prob_interior || known_binomial_edge) && std::isfinite(v[2]) &&
      (family=="cmb" || (v[2]>0 && v[2]<1) || ((v[2]==0 || v[2]==1) && !(mask & 4u)));
    if(valid && family=="zmbinom" && v[2]==1 && mask) {
      return inference_numeric_detail::singular_integer_support(data,0,0,"known p0 = 1 yields a point mass");
    }
    if(valid && v[0]==1 && ((family=="cmb" && (mask & 4u)) || (family=="zmbinom" && (mask & 2u)))) {
      const double lo=family=="zmbinom" && v[2]==0 ? 1 : 0;
      return inference_numeric_detail::singular_integer_support(data,lo,1,"a free coefficient has zero information at size one");
    }
  } else if(family=="cmp") {
    set(0,"lambda",v[1]==0?NumericCoordinate::UnitInterval:NumericCoordinate::Positive);
    set(1,"nu",NumericCoordinate::Positive);
    valid=std::isfinite(v[0]) && v[0]>0 && std::isfinite(v[1]) &&
      (v[1]>0 || (v[1]==0 && !(mask & 2u) && v[0]<1));
  } else if(family=="tbinom") {
    set(0,"prob",NumericCoordinate::UnitInterval);set(1,"a",NumericCoordinate::Real);
    set(2,"b",NumericCoordinate::Real);set(3,"size",NumericCoordinate::Real);
    valid=v[0]>0 && v[0]<1 && !std::isnan(v[1]) && !std::isnan(v[2]) && v[1]<v[2] &&
      std::isfinite(v[3]) && v[3]>=1 && v[3]==std::floor(v[3]);
    if(valid) {
      const double span=std::min(v[3],std::floor(v[2]))-std::max(0.0,std::floor(v[1])+1);
      if(span==0 && mask) {
        const double point=std::max(0.0,std::floor(v[1])+1);
        return inference_numeric_detail::singular_integer_support(data,point,point,"retained support is a singleton");
      }
      regime=span>=1;
    }
  } else if(family=="tpois") {
    set(0,"lambda",NumericCoordinate::Positive);set(1,"a",NumericCoordinate::Real);set(2,"b",NumericCoordinate::Real);
    valid=std::isfinite(v[0]) && v[0]>0 && !std::isnan(v[1]) && !std::isnan(v[2]) && v[1]<v[2];
    if(valid) {
      const double span=std::floor(v[2])-std::max(0.0,std::floor(v[1])+1);
      if(span==0 && mask) {
        const double point=std::max(0.0,std::floor(v[1])+1);
        return inference_numeric_detail::singular_integer_support(data,point,point,"retained support is a singleton");
      }
      regime=span>=1;
    }
  }
  if(!valid) {InferenceResult r;r.status=InferenceStatus::BoundaryOrLimit;r.validity_conditions="invalid or boundary parameters for the classified scope";return r;}
  if(!regime) {
    InferenceResult r;r.status=InferenceStatus::RegimeUnavailable;
    if(family=="ged" || family=="sged") r.validity_conditions="DQM holds above nu = 1/2; ordinary moving-location Hessians are invalid for nu <= 1; remaining excluded fitted cusp derivatives are unvalidated or on a fitting restriction";
    else if(family=="snorm" || family=="sstd") r.validity_conditions="the implemented regular submodel requires skew fixed at one";
    else if(family=="gev") r.validity_conditions="only shape fixed at zero is classified here";
    else if(family=="gpd") r.validity_conditions="shape must be positive, or zero held known, with threshold held";
    else if(family=="tbinom" || family=="tpois") r.validity_conditions="the retained support has no eligible observations";
    else r.validity_conditions="the requested held subset or regime is not classified for ordinary observed information";
    return r;
  }
  const auto density=[&](double x,const auto& a) {
    if(family=="laplace") return distributions::dlaplace(x,a[0],a[1],true);
    if(family=="gev") return distributions::dgev(x,a[0],a[1],a[2],true);
    if(family=="gpd") return distributions::dgpd(x,a[0],a[1],a[2],true);
    if(family=="ged") return distributions::dged(x,a[0],a[1],a[2],true);
    if(family=="sged") return distributions::dsged(x,a[0],a[1],a[2],a[3],true);
    if(family=="snorm") return distributions::dsnorm(x,a[0],a[1],a[2],true);
    if(family=="sstd") return distributions::dsstd(x,a[0],a[1],a[2],a[3],true);
    if(family=="tnorm") return distributions::dtnorm(x,a[0],a[1],a[2],a[3],true);
    if(family=="cmb") return distributions::dcmb(x,a[0],a[1],a[2],true);
    if(family=="cmp") return distributions::dcmp(x,a[0],a[1],true);
    if(family=="tbinom") return distributions::dtbinom(x,a[0],a[1],a[2],a[3],true);
    if(family=="tpois") return distributions::dtpois(x,a[0],a[1],a[2],true);
    if(family=="zmbinom") return distributions::dzmbinom(x,a[0],a[1],a[2],true);
    return std::numeric_limits<double>::quiet_NaN();
  };
  const NumericEligibility eligibility{InferenceStatus::Ok,allowed,"classified interior submodel; structural coefficients held"};
  return numeric_inference_checked(data,p,n,mask,eligibility,density,emit);
}
} // namespace cppunivariate
