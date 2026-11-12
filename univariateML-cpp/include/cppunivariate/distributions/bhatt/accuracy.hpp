#pragma once
#include "../../core/likelihood_polish.hpp"
#include "../burr/accuracy.hpp"
#include "normal_accuracy.hpp"
#include "pair_variance_accuracy.hpp"
namespace cppunivariate { namespace bhatt_detail {
inline accuracy_detail::Jet mills_denominator(const accuracy_detail::Jet& t) {
  accuracy_detail::Jet f(0);for(int k=48;k>=1;--k)f=accuracy_detail::Jet(k)/(t+f);return t+f;
}
inline accuracy_detail::Jet local_kernel(accuracy_detail::Jet z,const accuracy_detail::Jet& k) {
  if(z.v<0)z=-z;
  if(k.v*(1+z.v)<.05) {
    // Analytic expansion of E exp(z U-U^2/2), U uniform[-k,k].
    // The convergent power series has negligible remainder in this range.
    const auto a=z*k,b=-k*k/accuracy_detail::Jet(2);
    accuracy_detail::Jet total(1),previous(1),current=a;
    for(int n=2;n<=24;++n){const auto next=(a*current+accuracy_detail::Jet(2)*b*previous)/accuracy_detail::Jet(n);
      if(n%2==0)total=total+next/accuracy_detail::Jet(n+1);
      previous=current;current=next;}
    return -z*z/accuracy_detail::Jet(2)-accuracy_detail::Jet(.91893853320467274178)+accuracy_detail::log(total);
  }
  if(z.v-k.v>10) {
    const auto lo=z-k,hi=z+k,L=mills_denominator(lo),H=mills_denominator(hi);
    const auto gap=-accuracy_detail::Jet(2)*z*k+accuracy_detail::log(L/H);
    return -(z*z-accuracy_detail::Jet(2)*z*k+k*k)/accuracy_detail::Jet(2)-accuracy_detail::Jet(.91893853320467274178)-accuracy_detail::log(L)+accuracy_detail::log1p(-accuracy_detail::exp(gap))-accuracy_detail::log(accuracy_detail::Jet(2)*k);
  }
  const auto upper=accuracy_detail::logcdf(k-z),lower=accuracy_detail::logcdf(-k-z);
  return upper+accuracy_detail::log1p(-accuracy_detail::exp(lower-upper))-accuracy_detail::log(accuracy_detail::Jet(2)*k);
}
#include "variance_accuracy.hpp"
inline bool local_accuracy(BhattResult& r,span x,double mu,double sigma,double a,double tol,int maxit) {
  tol=std::min(tol,1e-10);
  if(std::isnan(a)&&r.a==0) {
    auto normal=r;
    if(pair_variance_accuracy(normal,x,mu,tol,maxit,!std::isnan(sigma))){r=normal;return true;}
  }
  if(!std::isnan(sigma)&&std::isnan(a)&&r.a>0&&r.a<.25*r.sigma) {
    auto finite=r;
    if(pair_variance_accuracy(finite,x,mu,tol,maxit,true)){r=finite;return true;}
  }
  const bool exact_normal=r.a==0&&normal_stationary_exact(x,r.mu,r.sigma,std::isnan(mu),std::isnan(sigma));
  if(a==0&&exact_normal)return true;
  if(std::isnan(mu)&&!std::isnan(sigma)&&a>0) {
    std::vector<double> ordered(x.begin(),x.end());std::sort(ordered.begin(),ordered.end());
    const double middle=ordered.front()/2+ordered.back()/2;bool symmetric=true;
    for(std::size_t i=0;i<ordered.size();++i)symmetric=symmetric&&normal_symmetric_exact(ordered[i],ordered[ordered.size()-1-i],middle);
    if(symmetric){r.mu=middle;return true;}
  }
  // Conditional location score: mu = mean(x) - mean(E[U|x]), |U|<=a.
  // This enclosure remains useful when endpoint arithmetic loses a entirely.
  if(std::isnan(mu)&&!std::isnan(sigma)&&a>0) {
    const auto moments=norm_detail::moments(x);
    if(a<=tol*.0625*std::max(std::abs(moments.mean),sigma)&&(tol>=4*std::numeric_limits<double>::epsilon()||normal_location_sum(x,moments.mean).sign()==0)) {r.mu=moments.mean;return true;}
  }
  // With sigma held and tiny relative to the range, Gaussian tail bounds
  // enclose the profiled width within T*sigma of the covering half-range,
  // and its centre within T*sigma of the midpoint. T includes log(n*range/s)
  // so that n*phi(T)/sigma < 1/(64*range). Rounded covering coefficients
  // add at most a few ulps; move width outward to preserve the likelihood.
  if(!std::isnan(sigma)&&std::isnan(a)) {
    const double low=*std::min_element(x.begin(),x.end()),high=*std::max_element(x.begin(),x.end());
    const double middle=low/2+high/2;
    const double center=std::isnan(mu)?middle:mu;
    const double radius=std::max(std::abs(low-center),std::abs(high-center));
    if(radius>0&&std::isfinite(radius)) {
      const double T=16+std::sqrt(2*std::max(0.,std::log(double(x.size()))+std::log(radius)-std::log(sigma)));
      const double pad=T*sigma+8*std::numeric_limits<double>::epsilon()*radius;
      if(pad<=tol*.03125*radius&&(!std::isnan(mu)||pad<=tol*.03125*std::max(std::abs(center),sigma))) {
        r.mu=center;r.a=std::nextafter(radius,INFINITY);return true;
      }
    }
  }
  if(std::isnan(sigma)&&std::isnan(a)&&r.a>0&&r.a<r.sigma)return variance_accuracy(r,x,mu,tol,maxit);
  const double center=r.mu,location_unit=std::max(std::abs(r.mu),r.sigma),unit=std::max(r.sigma,r.a);
  std::array<double,3> q{{0,std::log(r.sigma/unit),r.a>0?std::log(r.a/unit):0}};
  const std::array<bool,3> free{{std::isnan(mu),std::isnan(sigma),std::isnan(a)&&r.a>0}};
  const auto objective=[&](const std::array<accuracy_detail::Jet,3>& z) {
    const auto s=std::isnan(sigma)?accuracy_detail::exp(z[1]):accuracy_detail::Jet(static_cast<ExtendedReal>(sigma)/unit),width=r.a>0?(std::isnan(a)?accuracy_detail::exp(z[2]):accuracy_detail::Jet(static_cast<ExtendedReal>(a)/unit)):accuracy_detail::Jet(0);
    accuracy_detail::JetSum f;
    for(double v:x){const auto t=(accuracy_detail::Jet((static_cast<ExtendedReal>(v)-center)/unit)-z[0]*accuracy_detail::Jet(location_unit/unit))/s;
      f.add(-z[1]+(r.a>0?local_kernel(t,width/s):-t*t/accuracy_detail::Jet(2)));}
    return f.value;
  };
  if(!exact_normal&&!continuous_fallback_polish(q,free,tol,maxit,objective))return false;
  if(free[0]) {
    const double floor=std::max(std::abs(static_cast<double>(static_cast<ExtendedReal>(center)+location_unit*q[0])),unit*std::exp(q[1]));
    const double sensitivity=location_unit/floor;
    if(sensitivity>4&&!continuous_fallback_polish(q,free,tol*4/sensitivity,maxit,objective))return false;
  }
  if(free[0])r.mu=static_cast<double>(static_cast<ExtendedReal>(center)+location_unit*static_cast<ExtendedReal>(q[0]));
  if(free[1])r.sigma=unit*std::exp(q[1]);
  if(free[2])r.a=unit*std::exp(q[2]);
  if(!std::isnan(sigma)&&std::isnan(a)&&r.a>0&&r.a<.01*r.sigma)
    return pair_variance_accuracy(r,x,mu,tol,maxit,true);
  if(std::isnan(a)&&r.a==0) {
    // At free sigma, profiling variance cancels the quadratic width term;
    // the leading coefficient is -sum H4(z)/180. At held sigma the
    // quadratic coefficient is sum(z^2-1)/6, with a negative quartic
    // coefficient when it vanishes exactly.
    if(std::isnan(sigma)) {
      accuracy_detail::ScalarSum fourth,magnitude;
      for(double v:x){const ExtendedReal z=((static_cast<ExtendedReal>(v)-center)/unit-static_cast<ExtendedReal>(q[0])*(location_unit/unit))/std::exp(static_cast<ExtendedReal>(q[1])),z2=z*z,h4=z2*z2-6*z2+3;fourth.add(h4);magnitude.add(std::abs(h4));}
      const ExtendedReal margin=1024*std::numeric_limits<double>::epsilon()*(x.size()+magnitude.value);
      if(!(fourth.value>margin))return false;
    } else {
      if(normal_width_score_sign(x,r.mu,sigma,std::isnan(mu))>0)return false;
    }
  }
  return (!free[0]||accuracy_detail::location_projection(r.mu,center,location_unit,q[0],r.sigma,tol))&&
    (!free[1]||accuracy_detail::scale_projection(r.sigma,unit,q[1],tol))&&
    (!free[2]||accuracy_detail::scale_projection(r.a,unit,q[2],tol));
}
}}
