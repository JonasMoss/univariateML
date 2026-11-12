#pragma once
#include "../burr/accuracy.hpp"
namespace cppunivariate {
inline bool paralogis_local_accuracy(span x,double& shape,double& scale,double& logLik,double tol,int maxit,bool fixed_scale,bool inverse,StopDetail& detail) {
  tol=std::min(tol,1e-10);
  const double unit=scale;
  accuracy_detail::ScalarSum mean,square;
  for(double v:x){const ExtendedReal l=v>unit*.5&&v<unit*2?std::log1p((static_cast<ExtendedReal>(v)-unit)/unit):std::log(static_cast<ExtendedReal>(v))-std::log(static_cast<ExtendedReal>(unit));mean.add((inverse?-1:1)*l/x.size());}
  for(double v:x){const ExtendedReal l=(inverse?-1:1)*(v>unit*.5&&v<unit*2?std::log1p((static_cast<ExtendedReal>(v)-unit)/unit):std::log(static_cast<ExtendedReal>(v))-std::log(static_cast<ExtendedReal>(unit)))-mean.value;square.add(l*l/x.size());}
  const double spread=std::sqrt(square.value);
  // The log-moment scale supplies the concentrated-data starting point when
  // the original finite-difference proposal has lost all spread information.
  const bool reseed=!fixed_scale&&spread>0&&shape*spread<.1;
  if(reseed)shape=1.282549830161864/spread;
  const double center=fixed_scale?0:static_cast<double>(mean.value);
  std::array<double,3> q{{std::log(shape),fixed_scale?0:-shape*center-std::log(shape),0}};
  if(!fixed_scale)q[1]=shape<32?special::digamma(shape)+.5772156649015328606-std::log(shape):.5772156649015328606-1/(2*shape)-1/(12*shape*shape)+1/(120*shape*shape*shape*shape);
  const std::array<bool,3> free{{true,!fixed_scale,false}};
  const auto objective=[&](const std::array<accuracy_detail::Jet,3>& z) {
    const auto a=accuracy_detail::exp(z[0]);accuracy_detail::JetSum f;
    for(double v:x){const ExtendedReal l=(inverse?-1:1)*(v>unit*.5&&v<unit*2?std::log1p((static_cast<ExtendedReal>(v)-unit)/unit):std::log(static_cast<ExtendedReal>(v))-std::log(static_cast<ExtendedReal>(unit)))-center;
      if(fixed_scale){const auto t=a*accuracy_detail::Jet(l);f.add(accuracy_detail::Jet(2)*z[0]-accuracy_detail::softplus(-t)-a*accuracy_detail::softplus(t));}
      else {
        const auto h=a*accuracy_detail::Jet(l)-z[1],w=accuracy_detail::exp(h),r=w/a;
        if(r.v<.01)f.add(z[0]+h-w-accuracy_detail::log1p(r)-a*accuracy_detail::log1pmx(r));
        else {const auto t=h-z[0];f.add(accuracy_detail::Jet(2)*z[0]-accuracy_detail::softplus(-t)-a*accuracy_detail::softplus(t));}
      }
    }
    return f.value;
  };
  if(!continuous_fallback_polish(q,free,tol,maxit,objective,detail))return false;
  if(!fixed_scale) {
    const double sensitivity=(1+std::abs(1-q[1]-q[0]))/std::exp(q[0]);
    if(sensitivity>4&&!continuous_fallback_polish(q,free,tol*4/sensitivity,maxit,objective,detail))return false;
  }
  shape=std::exp(q[0]);
  const double coordinate=fixed_scale?0:(inverse?-1:1)*(center+(q[1]+q[0])/shape);
  if(!fixed_scale)scale=unit*std::exp(coordinate);
  if(!accuracy_detail::scale_projection(scale,unit,coordinate,tol))return false;
  accuracy_detail::ScalarSum ll;
  for(double v:x){const ExtendedReal t=(inverse?-1:1)*shape*(v>scale*.5&&v<scale*2?std::log1p((static_cast<ExtendedReal>(v)-scale)/scale):std::log(static_cast<ExtendedReal>(v))-std::log(static_cast<ExtendedReal>(scale)));
    ll.add(2*std::log(static_cast<ExtendedReal>(shape))-std::log(static_cast<ExtendedReal>(v))-std::max(-t,ExtendedReal(0))-std::log1p(std::exp(-std::abs(t)))-shape*(std::max(t,ExtendedReal(0))+std::log1p(std::exp(-std::abs(t)))));}
  logLik=static_cast<double>(ll.value);return true;
}
inline bool paralogis_local_accuracy(span x,double& shape,double& scale,double& logLik,double tol,int maxit,bool fixed_scale,bool inverse=false) {
  StopDetail detail;return paralogis_local_accuracy(x,shape,scale,logLik,tol,maxit,fixed_scale,inverse,detail);
}

}
