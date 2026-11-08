// Natural-coordinate, second-order chain rules for moving C1 density joins.
// No perturbed parameter evaluations: the piece is selected at the observation.
#pragma once
#include "inference.hpp"
#include "../special/special.hpp"
#include "../special/skewed_densities.hpp"
namespace cppunivariate { namespace inference_join_detail {
// Four is the largest parameter count among these adapters. Hessians are
// derivatives of log density, not information, until accumulation below.
struct Jet {
  double v=0;
  std::array<double,4> g{};
  std::array<double,16> h{};
  Jet(double value=0):v(value) {}
};
inline Jet variable(double v,std::size_t i) {Jet a(v);a.g[i]=1;return a;}
inline Jet operator+(const Jet& a,const Jet& b) {
  Jet c(a.v+b.v);for(std::size_t i=0;i<4;++i)c.g[i]=a.g[i]+b.g[i];
  for(std::size_t i=0;i<16;++i)c.h[i]=a.h[i]+b.h[i];
  return c;
}
inline Jet operator-(const Jet& a,const Jet& b) {
  Jet c(a.v-b.v);for(std::size_t i=0;i<4;++i)c.g[i]=a.g[i]-b.g[i];
  for(std::size_t i=0;i<16;++i)c.h[i]=a.h[i]-b.h[i];
  return c;
}
inline Jet operator*(const Jet& a,const Jet& b) {
  Jet c(a.v*b.v);
  for(std::size_t i=0;i<4;++i) {
    c.g[i]=a.g[i]*b.v+a.v*b.g[i];
    for(std::size_t j=0;j<4;++j)c.h[4*i+j]=a.h[4*i+j]*b.v+a.v*b.h[4*i+j]+a.g[i]*b.g[j]+a.g[j]*b.g[i];
  }return c;
}
inline Jet chain(const Jet& a,double value,double first,double second) {
  Jet c(value);for(std::size_t i=0;i<4;++i) {
    c.g[i]=first*a.g[i];for(std::size_t j=0;j<4;++j)c.h[4*i+j]=first*a.h[4*i+j]+second*a.g[i]*a.g[j];
  }return c;
}
inline Jet reciprocal(const Jet& a) {const double r=1/a.v;return chain(a,r,-r*r,2*r*r*r);}
inline Jet operator/(const Jet& a,const Jet& b) {return a*reciprocal(b);}
inline Jet log(const Jet& a) {return chain(a,std::log(a.v),1/a.v,-1/(a.v*a.v));}
inline Jet log1p(const Jet& a) {const double r=1/(1+a.v);return chain(a,std::log1p(a.v),r,-r*r);}
inline Jet exp(const Jet& a) {const double e=std::exp(a.v);return chain(a,e,e,e);}
inline Jet sqrt(const Jet& a) {const double s=std::sqrt(a.v);return chain(a,s,.5/s,-.25/(s*a.v));}
inline Jet lgamma(const Jet& a) {return chain(a,special::lgamma(a.v),special::digamma(a.v),special::trigamma(a.v));}
inline Jet logadd(const Jet& a,const Jet& b) {return a.v>=b.v ? a+log1p(exp(b-a)) : b+log1p(exp(a-b));}
// L(t)=log1p(t)/t and its removable second-order jet at t=0.
// For |t|<.01, degree 14 has value remainder <= |t|^15/(16*(1-|t|));
// first/second derivative tails are < 2e-28 / 3e-25 at the switch.
inline Jet log1p_ratio(const Jet& t) {
  if(std::abs(t.v)>=.01)return log1p(t)/t;
  Jet a(1./15);
  for(int k=13;k>=0;--k)a=Jet((k%2?-1.:1.)/(k+1))+t*a;
  return a;
}
// Large-shape gamma correction evaluated as a jet in reciprocal nu.
// Its differentiated remainder is bounded by differentiating the same
// signed Laplace remainder as the scalar series.
inline Jet half_gamma_correction(const Jet& nu) {
  const Jet t=reciprocal(nu),t2=t*t;
  return t*(-.25+t2*(1./24+t2*(-1./20+t2*(17./112-t2*(31./36)))));
}
inline Jet std_constant(const Jet& nu) {
  if(nu.v>=128)
    return -.5*std::log(2*cppunivariate::pi)-.5*log1p(Jet(-2)/nu)+half_gamma_correction(nu);
  Jet a=lgamma((nu+1)/2)-lgamma(nu/2)-.5*log((nu-2)*cppunivariate::pi);
  a.v=dens::log_dstd_unit_const(nu.v);
  return a;
}
inline Jet std_moment(const Jet& nu,const Jet& constant) {
  if(nu.v>=129)
    return std::sqrt(2/cppunivariate::pi)*sqrt(1-1/(nu-1))*exp(Jet(0)-half_gamma_correction(nu-1));
  Jet a=exp(constant)*2*(nu-2)/(nu-1);
  a.v=dens::std_abs_moment(nu.v);
  return a;
}
inline Jet density(double x,const std::string& f,const std::array<double,inference_max_parameters>& v) {
  std::array<Jet,4> p{};for(std::size_t i=0;i<4;++i)p[i]=variable(v[i],i);
  const double pi=3.14159265358979323846;
  if(f=="snorm" || f=="sstd") {
    const Jet xi=p[f=="snorm"?2:3], nu=p[2];
    const Jet constant=f=="snorm" ? Jet(-.5*std::log(2*pi)) :
      std_constant(nu);
    const Jet moment=f=="snorm" ? Jet(std::sqrt(2/pi)) : std_moment(nu,constant);
    const Jet m=moment*(xi-1/xi);
    const Jet c=sqrt((1-moment*moment)*(xi*xi+1/(xi*xi))+2*moment*moment-1);
    const Jet z=c*(x-p[0])/p[1]+m;
    // At z=0 choose the right-hand continuation. The score is continuous;
    // the Hessian need not be. This null-set convention is not an average.
    const Jet q=z.v<0 ? z*xi : z/xi;
    const Jet base=f=="snorm" ? constant-q*q/2 : nu.v>=128
      ? constant-.5*q*q*(1+3/(nu-2))*log1p_ratio(q*q/(nu-2))
      : constant-(nu+1)*log1p(q*q/(nu-2))/2;
    return log(c)-log(p[1])+std::log(2.)-log(xi+1/xi)+base;
  }
  if(f=="huber") {
    const Jet e=p[2],z=(x-p[0])/p[1];
    const double a=e.v,t=std::exp(-a*a/2),Z=std::sqrt(2*pi)*std::erf(a/std::sqrt(2.))+2*t/a;
    const double d=-2*t/(a*a),dd=2*t*(1/a+2/(a*a*a));
    const Jet logZ=chain(e,std::log(Z),d/Z,dd/Z-(d/Z)*(d/Z));
    // Central continuation at either join; both first derivatives agree.
    const Jet rho=std::abs(z.v)<=a ? z*z/2 : e*(z.v<0 ? Jet(0)-z : z)-e*e/2;
    return Jet(0)-log(p[1])-logZ-rho;
  }
  if(f=="gpd" || f=="gev") {
    const Jet z=(x-p[0])/p[1],t=p[2]*z;
    const Jet h=z*log1p_ratio(t); // log(1+xi*z)/xi, continuous at xi=0
    const Jet result=Jet(0)-log(p[1])-log1p(t)-h;
    return f=="gpd" ? result : result-exp(Jet(0)-h);
  }
  // PIG: p_k/p_{k-1}=A(1-3/(2k))+B/[k(k-1)r_{k-1}].
  // Work is determined by the observed count; O(1) storage, no truncation.
  const Jet m=p[0],s=p[1],u=2*m*m/s,t=log1p(u);
  Jet mass=Jet(0)-2*m/(1+sqrt(1+u));
  const Jet la=log(u)-t,lb=2*log(m)-t;
  Jet lr=log(m)-t/2;
  if(x>=1)mass=mass+lr;
  for(int k=2;k<=static_cast<int>(x);++k) {
    const double n=k;
    lr=logadd(la+std::log1p(-1.5/n),lb-std::log(n)-std::log(n-1)-lr);
    mass=mass+lr;
  }
  return mass;
}
} // namespace inference_join_detail

template<class Emit=inference_detail::Discard>
inline InferenceResult join_family_inference(span data,const std::string& family,
    const std::array<double,inference_max_parameters>& v,unsigned mask,Emit emit={}) {
  InferenceResult r;
  if(family!="snorm" && family!="sstd" && family!="huber" && family!="pig" && family!="gpd" && family!="gev") {r.status=InferenceStatus::FamilyUnavailable;return r;}
  const std::size_t n=family=="sstd"?4:family=="pig"?2:3;
  r.parameter_count=n;r.free_mask=mask;r.observations=data.size();
  r.fisher_status=InferenceStatus::NumericalFailure;r.fisher.fill(NAN);
  r.validity_conditions="finite regular interior; analytic piecewise natural-coordinate scores and a.e. Hessians; successful interior fit and nonsingular sensitivity required";
  if(mask >= (1u<<n)){r.status=InferenceStatus::InvalidSubset;return r;}
  const bool endpoint=family=="gpd" || family=="gev";
  const bool fs=family=="snorm" || family=="sstd";
  r.parameter_names=fs ? std::array<std::string,8>{"mean","sd",family=="sstd"?"nu":"xi","xi"} :
    (family=="huber" || endpoint) ? std::array<std::string,8>{"mu","sigma",endpoint?"xi":"epsilon"} : std::array<std::string,8>{"mean","shape"};
  for(std::size_t i=0;i<n;++i) {
    if(!std::isfinite(v[i]) || (((i>0 && !(endpoint && i==2)) || family=="pig") && !(v[i]>0)) || (family=="sstd" && i==2 && !(v[i]>2))) {
      r.status=InferenceStatus::BoundaryOrLimit;return r;
    }
    if(mask & (1u<<i))r.free_indices[r.dimension++]=i;
  }
  if(endpoint && (v[2]<=-.5 || (family=="gev" && (mask & 4u) && v[2]>=1) || (family=="gpd" && (mask & 1u)))) {
    r.status=InferenceStatus::RegimeUnavailable;
    r.validity_conditions="shape > -1/2; GPD threshold held; free GEV shape below fitted upper bound one";return r;
  }
  if(data.empty()){r.status=InferenceStatus::EmptySample;return r;}
  for(double x:data) {
    if(!std::isfinite(x) || (family=="pig" && (x<0 || x!=std::floor(x)))) {r.status=InferenceStatus::InvalidObservation;return r;}
    if(endpoint && ((family=="gpd" && x<v[0]) || !(1+v[2]*((x-v[0])/v[1])>0))) {
      r.status=InferenceStatus::InvalidObservation;return r;
    }
    // The large-count asymptotic density is deliberately not differentiated.
    // Fail rather than return derivatives of a truncated recurrence/expansion.
    if(family=="pig" && x>=std::numeric_limits<int>::max()) {r.status=InferenceStatus::NumericalFailure;return r;}
    const auto a=inference_join_detail::density(x,family,v);
    if(!std::isfinite(a.v)){r.status=InferenceStatus::NumericalFailure;return r;}
    std::array<double,inference_max_parameters> row{};
    for(std::size_t i=0;i<r.dimension;++i) {
      const auto u=r.free_indices[i];row[i]=a.g[u];r.score[i]+=row[i];
      if(!std::isfinite(row[i])){r.status=InferenceStatus::NumericalFailure;return r;}
      for(std::size_t j=0;j<r.dimension;++j) {
        r.opg[i*r.dimension+j]+=a.g[u]*a.g[r.free_indices[j]];
        r.observed[i*r.dimension+j]-=a.h[4*u+r.free_indices[j]];
      }
    }emit(row);
  }
  // Mixed chain-rule summands may differ by a rounding bit. Preserve the
  // exact symmetry contract before matrix classification and covariance.
  for(std::size_t i=0;i<r.dimension;++i)for(std::size_t j=0;j<i;++j) {
    const double a=(r.observed[i*r.dimension+j]+r.observed[j*r.dimension+i])/2;
    r.observed[i*r.dimension+j]=r.observed[j*r.dimension+i]=a;
  }
  r.opg_status=inference_detail::matrix_status(r.opg,r.dimension,true,r.opg_condition);
  r.observed_status=inference_detail::matrix_status(r.observed,r.dimension,false,r.observed_condition);
  for(std::size_t i=0;i<r.dimension;++i)if(!std::isfinite(r.score[i]))r.status=InferenceStatus::NumericalFailure;
  if(r.opg_status==InferenceStatus::NumericalFailure || r.observed_status==InferenceStatus::NumericalFailure)r.status=InferenceStatus::NumericalFailure;
  return r;
}
} // namespace cppunivariate
