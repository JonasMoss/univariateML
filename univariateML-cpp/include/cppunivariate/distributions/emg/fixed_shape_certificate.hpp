#pragma once
#include "location_certificate.hpp"
#include "shape_bounds.hpp"
#include "certificate_math.hpp"

namespace cppunivariate {
namespace emg_detail {
struct FixedShapeCertificate {
  double mu=NAN,b=NAN,k=NAN,loglik_lower=-INFINITY,loglik_upper=INFINITY;
  double b_lower=NAN,b_upper=NAN,u_lower=NAN,u_upper=NAN;
  bool ok=false;
};
struct FixedShapePoint {
  double b=NAN,mu=NAN,mu_lower=NAN,mu_upper=NAN;
  CPPUNIV_EXTENDED_REAL score_lower=NAN,score_upper=NAN;
  bool ok=false;
};
// The derivative is b*dP/db, where P profiles u (or imposes u=b*mu).
// At fixed b, the location enclosure bounds the envelope score componentwise.
inline FixedShapePoint fixed_shape_point(span x,double k,double b,double mu,double tol,int maxit) {
  FixedShapePoint out;out.b=b;
  const double sigma=1/b,lambda=k*b;
  if(!(sigma>0&&lambda>0&&std::isfinite(sigma)&&std::isfinite(lambda)))return out;
  if(std::isnan(mu)) {
    const auto c=location_certificate(x,sigma,lambda,tol,maxit);
    if(!c.ok)return out;
    out.mu=c.mu;out.mu_lower=c.lower;out.mu_upper=c.upper;
  } else out.mu=out.mu_lower=out.mu_upper=mu;
  CPPUNIV_EXTENDED_REAL lower=x.size(),upper=x.size(),magnitude=x.size();
  for(double v:x) {
    const std::array<double,1> value{{v}};
    const span one{value.data(),value.size()};
    const auto qlo=location_score(one,out.mu_lower,sigma,lambda);
    const auto qhi=location_score(one,out.mu_upper,sigma,lambda);
    const CPPUNIV_EXTENDED_REAL factor=static_cast<CPPUNIV_EXTENDED_REAL>(b)*
      (std::isnan(mu)?v:static_cast<CPPUNIV_EXTENDED_REAL>(v)-mu);
    const CPPUNIV_EXTENDED_REAL a=-factor*qlo[1],c=-factor*qhi[0];
    lower+=std::min(a,c);upper+=std::max(a,c);magnitude+=std::max(std::abs(a),std::abs(c));
  }
  const CPPUNIV_EXTENDED_REAL margin=128*std::numeric_limits<double>::epsilon()*magnitude;
  out.score_lower=lower-margin;out.score_upper=upper+margin;
  out.ok=std::isfinite(out.score_lower)&&std::isfinite(out.score_upper);return out;
}
// A global inner certificate at fixed dimensionless shape. Physical holds
// become affine constraints: u=b*mu, b=1/sigma, b=lambda/k. Conflicting
// fixed scales are rejected. This is separate from the legacy search shared
// with Bhattacharjee, whose behavior is deliberately unchanged.
inline FixedShapeCertificate fixed_shape_certificate(span x,double k,double tol=1e-8,int maxit=500,
    double mu=NAN,double sigma=NAN,double lambda=NAN) {
  FixedShapeCertificate out;out.k=k;
  if(x.empty()||!(k>0&&std::isfinite(k)&&tol>0&&std::isfinite(tol))||solver_budget(maxit)<1||
      (!std::isnan(mu)&&!std::isfinite(mu))||
      (!std::isnan(sigma)&&!(sigma>0&&std::isfinite(sigma)))||
      (!std::isnan(lambda)&&!(lambda>0&&std::isfinite(lambda))))return out;
  const double xmin=*std::min_element(x.begin(),x.end()),xmax=*std::max_element(x.begin(),x.end());
  for(double v:x)if(!std::isfinite(v))return out;
  if(!std::isnan(sigma)&&!std::isnan(lambda)&&sigma*lambda!=k)return out;
  if(std::isnan(sigma)&&std::isnan(lambda)&&xmin==xmax&&(std::isnan(mu)||mu==xmin))return out;
  // A validated Newton root is globally sufficient in this concave slice.
  // Keep the bracket solver below as a safe fallback, with the same contract.
  const CertModel model(x,mu,sigma,lambda,false,k);
  const auto fast=cert_newton(model,model.initial(k),std::min(tol,1e-8),maxit);
  if(fast.ok&&fast.accurate) {
    double m,s,l;model.coordinates(fast.center,m,s,l);
    const auto coef=model.coefficients(fast.enclosure);
    const CertInterval bb=CertInterval(1)/coef[1],uu=coef[0]*bb;
    out.mu=m;out.b=1/s;out.b_lower=bb.lo;out.b_upper=bb.hi;
    out.u_lower=uu.lo;out.u_upper=uu.hi;out.loglik_lower=fast.ll_lower;
    out.loglik_upper=fast.ll_upper;out.ok=bb.valid()&&uu.valid();return out;
  }
  const double fixed_b=!std::isnan(sigma)?1/sigma:(!std::isnan(lambda)?lambda/k:NAN);
  double lo=fixed_b,hi=fixed_b;
  FixedShapePoint point;
  const double inner_tol=std::min(tol/64,1e-10);
  if(std::isnan(fixed_b)) {
    double distance=std::isnan(mu)?xmax-xmin:std::max(std::abs(xmin-mu),std::abs(xmax-mu));
    if(!(distance>0&&std::isfinite(distance)))return out;
    double b=1/distance;bool left=false,right=false;
    for(int it=0;it<solver_budget(maxit);++it) {
      point=fixed_shape_point(x,k,b,mu,inner_tol,maxit);if(!point.ok)return out;
      if(point.score_lower>0) {lo=b;left=true;if(right)break;b*=2;}
      else if(point.score_upper<0) {hi=b;right=true;if(left)break;b/=2;}
      else {
        const auto a=fixed_shape_point(x,k,b/2,mu,inner_tol,maxit);
        const auto c=fixed_shape_point(x,k,b*2,mu,inner_tol,maxit);
        if(!(a.ok&&c.ok&&a.score_lower>0&&c.score_upper<0))return out;
        lo=b/2;hi=b*2;left=right=true;break;
      }
    }
    if(!(left&&right&&hi>lo))return out;
    bool narrowed=false;
    for(int it=0;it<solver_budget(maxit);++it) {
      const double b=lo+(hi-lo)/2;
      if(hi-lo<=tol*lo) {narrowed=true;break;}
      point=fixed_shape_point(x,k,b,mu,inner_tol,maxit);if(!point.ok)return out;
      if(point.score_lower>0)lo=b;
      else if(point.score_upper<0)hi=b;
      else {
        const double d=tol*lo/8;
        const auto a=fixed_shape_point(x,k,b-d,mu,inner_tol,maxit);
        const auto c=fixed_shape_point(x,k,b+d,mu,inner_tol,maxit);
        if(!(a.ok&&c.ok&&a.score_lower>0&&c.score_upper<0))return out;
        lo=b-d;hi=b+d;
      }
    }
    if(!narrowed)return out;
  }
  out.b=lo+(hi-lo)/2;
  point=fixed_shape_point(x,k,out.b,mu,inner_tol,maxit);if(!point.ok)return out;
  out.mu=point.mu;out.b_lower=lo;out.b_upper=hi;
  // u'(b) is a curvature-weighted average of the observations and therefore
  // lies in [xmin,xmax]. For held mu, u'=mu exactly.
  const CPPUNIV_EXTENDED_REAL delta=(hi-lo)/2;
  const CPPUNIV_EXTENDED_REAL lipschitz=std::isnan(mu)?std::max(std::abs(xmin),std::abs(xmax)):std::abs(mu);
  const CPPUNIV_EXTENDED_REAL ul=static_cast<CPPUNIV_EXTENDED_REAL>(out.b)*point.mu_lower-delta*lipschitz;
  const CPPUNIV_EXTENDED_REAL uh=static_cast<CPPUNIV_EXTENDED_REAL>(out.b)*point.mu_upper+delta*lipschitz;
  out.u_lower=std::nextafter(static_cast<double>(ul),-INFINITY);
  out.u_upper=std::nextafter(static_cast<double>(uh),INFINITY);
  CPPUNIV_EXTENDED_REAL ll=x.size()*std::log(static_cast<CPPUNIV_EXTENDED_REAL>(out.b));
  CPPUNIV_EXTENDED_REAL gu=0,gb=static_cast<CPPUNIV_EXTENDED_REAL>(x.size())/out.b,mag=std::abs(ll),gmag=0;
  const double s=1/out.b,l=k*out.b;
  for(double v:x) {
    const CPPUNIV_EXTENDED_REAL z=(static_cast<CPPUNIV_EXTENDED_REAL>(v)-out.mu)/s;
    const CPPUNIV_EXTENDED_REAL term=std::log(static_cast<CPPUNIV_EXTENDED_REAL>(k))+logterm(z,k);
    ll+=term;mag+=std::abs(term);
    const std::array<double,1> value{{v}};const span one{value.data(),value.size()};
    const auto q=location_score(one,out.mu,s,l);
    const CPPUNIV_EXTENDED_REAL center=(q[0]+q[1])/2;
    gu+=center;gb-=v*center;gmag+=(q[1]-q[0])/2*(1+std::abs(v));
  }
  const CPPUNIV_EXTENDED_REAL u=static_cast<CPPUNIV_EXTENDED_REAL>(out.b)*out.mu;
  const CPPUNIV_EXTENDED_REAL du=std::max(std::abs(ul-u),std::abs(uh-u));
  const CPPUNIV_EXTENDED_REAL margin=256*std::numeric_limits<double>::epsilon()*(x.size()+mag);
  out.loglik_lower=static_cast<double>(ll-margin);
  // Concavity: L(v) <= L(v0)+grad L(v0).(v-v0) on the entire half-plane.
  out.loglik_upper=static_cast<double>(ll+margin+(std::abs(gu)+gmag)*du+(std::abs(gb)+gmag)*delta);
  out.ok=std::isfinite(out.loglik_lower)&&std::isfinite(out.loglik_upper)&&out.loglik_upper>=out.loglik_lower;
  return out;
}
}
}
