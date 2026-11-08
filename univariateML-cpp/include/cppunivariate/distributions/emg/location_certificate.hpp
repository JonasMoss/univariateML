#pragma once
#include "dpqr.hpp"
#include "../../core/span.hpp"
#include "../../core/work_budget.hpp"
#include "../../core/stop_detail.hpp"
#include <array>

namespace cppunivariate {
namespace emg_detail {
// Bounds on sigma * dL/dmu. In the positive Mills tail, consecutive
// continued-fraction convergents enclose h(t)=1/M(t)-t in exact arithmetic.
// Other branches use the settled Mills evaluator, with a practical margin.
inline std::array<CPPUNIV_EXTENDED_REAL,2> location_score(
    span x, CPPUNIV_EXTENDED_REAL mu, double sigma, double lambda) {
  CPPUNIV_EXTENDED_REAL lower=0,upper=0,magnitude=0;
  const CPPUNIV_EXTENDED_REAL a=static_cast<CPPUNIV_EXTENDED_REAL>(sigma)*lambda;
  for(double v:x) {
    const CPPUNIV_EXTENDED_REAL z=(static_cast<CPPUNIV_EXTENDED_REAL>(v)-mu)/sigma,t=a-z;
    if(t>=12) {
      CPPUNIV_EXTENDED_REAL r0=0,r1=0;
      for(int j=40;j>=2;--j)r0=j/(t+r0);
      for(int j=41;j>=2;--j)r1=j/(t+r1);
      const CPPUNIV_EXTENDED_REAL h0=1/(t+r0),h1=1/(t+r1);
      lower+=z-std::max(h0,h1);upper+=z-std::min(h0,h1);
      magnitude+=std::abs(z)+std::max(h0,h1);
    } else {
      const CPPUNIV_EXTENDED_REAL logm=logmills(t),r=std::exp(-logm);
      lower+=a-r;upper+=a-r;
      magnitude+=std::abs(a)+(r>0?r*(1+std::abs(logm)):0);
    }
  }
  const CPPUNIV_EXTENDED_REAL margin=128*std::numeric_limits<double>::epsilon()*magnitude+
      128*static_cast<CPPUNIV_EXTENDED_REAL>(x.size())*std::numeric_limits<double>::denorm_min();
  return {{lower-margin,upper+margin}};
}
struct LocationCertificate {
  double mu=NAN,lower=NAN,upper=NAN,loglik=NAN;
  int iterations=0;
  bool ok=false,arithmetic_failure=false;
  StopDetail detail{};
};
inline LocationCertificate location_certificate(span x,double sigma,double lambda,double tol,int maxit) {
  LocationCertificate out;
  const double xmin=*std::min_element(x.begin(),x.end()),xmax=*std::max_element(x.begin(),x.end());
  CPPUNIV_EXTENDED_REAL step=std::max(static_cast<CPPUNIV_EXTENDED_REAL>(sigma),
      1/static_cast<CPPUNIV_EXTENDED_REAL>(lambda));
  CPPUNIV_EXTENDED_REAL lo=static_cast<CPPUNIV_EXTENDED_REAL>(xmin)-step,hi=xmax;
  bool bracket=false;
  for(int it=0;it<solver_budget(maxit);++it) {
    out.iterations=it+1;
    const auto left=location_score(x,lo,sigma,lambda),right=location_score(x,hi,sigma,lambda);
    if(!(std::isfinite(lo)&&std::isfinite(hi)&&std::isfinite(left[0])&&std::isfinite(right[1]))) {
      out.arithmetic_failure=true;break;
    }
    if(left[0]>0 && right[1]<0) {bracket=true;break;}
    // The mode lies below xmax. Expansion also covers tiny rates.
    step*=2;lo=static_cast<CPPUNIV_EXTENDED_REAL>(xmin)-step;hi=static_cast<CPPUNIV_EXTENDED_REAL>(xmax)+step;
  }
  if(!bracket)return out;
  bool enclosed=false;
  for(int it=out.iterations;it<solver_budget(maxit);++it) {
    out.iterations=it+1;
    const CPPUNIV_EXTENDED_REAL mid=lo+(hi-lo)/2;
    // sigma is the declared location accuracy floor. Using the smallest
    // absolute value in the bracket makes the test valid at the true root.
    const CPPUNIV_EXTENDED_REAL minabs=lo<=0&&hi>=0?0:std::min(std::abs(lo),std::abs(hi));
    const CPPUNIV_EXTENDED_REAL target=tol*std::max(static_cast<CPPUNIV_EXTENDED_REAL>(sigma),minabs);
    const double rounded=static_cast<double>(mid);
    if(std::isfinite(rounded) && std::max(std::abs(rounded-lo),std::abs(hi-rounded))<=target) {enclosed=true;break;}
    const auto score=location_score(x,mid,sigma,lambda);
    if(score[0]>0)lo=mid;
    else if(score[1]<0)hi=mid;
    else {
      // A small score alone cannot accept a coordinate. Confirm signs at
      // two probes strictly inside the requested error radius instead.
      const CPPUNIV_EXTENDED_REAL radius=target/4;
      const auto a=location_score(x,mid-radius,sigma,lambda),b=location_score(x,mid+radius,sigma,lambda);
      if(radius>0 && mid-radius>lo && mid+radius<hi && a[0]>0 && b[1]<0) {
        lo=mid-radius;hi=mid+radius;
      } else {out.arithmetic_failure=true;break;}
    }
  }
  const CPPUNIV_EXTENDED_REAL mid=lo+(hi-lo)/2;
  out.mu=static_cast<double>(mid);
  out.lower=std::nextafter(static_cast<double>(lo),-INFINITY);
  out.upper=std::nextafter(static_cast<double>(hi),INFINITY);
  if(!std::isfinite(out.mu)) {out.arithmetic_failure=true;return out;}
  CPPUNIV_EXTENDED_REAL ll=0,mag=0;
  for(double v:x) {
    const double value=distributions::demg(v,out.mu,sigma,lambda,true);
    ll+=value;mag+=std::abs(value);
  }
  out.loglik=static_cast<double>(ll);
  const auto score=location_score(x,out.mu,sigma,lambda);
  const CPPUNIV_EXTENDED_REAL margin=128*std::numeric_limits<double>::epsilon()*(x.size()+mag);
  const double lower=static_cast<double>(ll-margin);
  const double upper=static_cast<double>(ll+margin+
      std::max(std::abs(score[0]),std::abs(score[1]))*
      std::max(std::abs(out.mu-lo),std::abs(hi-out.mu))/sigma);
  if(std::isfinite(out.loglik)&&std::isfinite(lower)&&std::isfinite(upper)) {
    out.detail.candidates.push_back({{out.mu,sigma,lambda},lower,static_cast<double>(ll+margin),StopModel::None});
    out.detail.loglik_lower=lower;out.detail.loglik_upper=upper;
    out.ok=enclosed;
  } else out.arithmetic_failure=true;
  return out;
}
}
}
