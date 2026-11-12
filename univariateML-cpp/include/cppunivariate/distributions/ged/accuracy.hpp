#pragma once
#include "../../core/likelihood_polish.hpp"
namespace cppunivariate {
inline bool ged_local_accuracy(GedResult& r,span x,double mean,double sd,double nu,double tol,int maxit) {
  tol=std::min(tol,1e-10);
  // Below one, exact knot enumeration supplies the location; only its smooth
  // conditional scale remains. The Laplace member likewise uses its median.
  const bool knot=r.nu<=1;
  if(r.nu<1&&std::isnan(mean)&&!x.empty()) {
    double chosen=x[0];
    for(double v:x)if(std::abs(static_cast<ExtendedReal>(v)-r.mu)<std::abs(static_cast<ExtendedReal>(chosen)-r.mu))chosen=v;
    r.mu=chosen;
  }
  const double center=r.mu,unit=r.sigma;
  std::array<double,3> q{{0,0,std::log(r.nu)}};
  const std::array<bool,3> free{{std::isnan(mean)&&!knot,std::isnan(sd),std::isnan(nu)&&!knot}};
  const auto objective=[&](const std::array<accuracy_detail::Jet,3>& z) {
    const auto p=std::isnan(nu)?accuracy_detail::exp(z[2]):accuracy_detail::Jet(nu),inv=accuracy_detail::reciprocal(p);
    const auto llambda=(-accuracy_detail::Jet(2)*inv*accuracy_detail::Jet(std::log(ExtendedReal(2)))+
      accuracy_detail::lgamma(inv)-accuracy_detail::lgamma(accuracy_detail::Jet(3)*inv))/accuracy_detail::Jet(2);
    accuracy_detail::JetSum f;
    const auto normalizer=z[2]-z[1]-accuracy_detail::Jet(std::log(ExtendedReal(2)))-llambda-accuracy_detail::lgamma(inv)-inv*accuracy_detail::Jet(std::log(ExtendedReal(2)));
    for(double v:x){auto d=accuracy_detail::Jet((static_cast<ExtendedReal>(v)-center)/unit)-z[0];if(d.v<0)d=-d;
      const auto w=d.v==0?accuracy_detail::Jet(0):accuracy_detail::exp(p*(accuracy_detail::log(d)-z[1]-llambda))/accuracy_detail::Jet(2);
      f.add(normalizer-w);}
    return f.value;
  };
  if(!accuracy_detail::polish(q,free,tol,maxit,objective,r.stop_detail))return false;
  if(free[0]) {
    const double location=static_cast<double>(static_cast<ExtendedReal>(center)+unit*static_cast<ExtendedReal>(q[0]));
    const double scale=unit*std::exp(q[1]);
    const double sensitivity=unit/std::max(std::abs(location),scale);
    if(sensitivity>4&&!accuracy_detail::polish(q,free,tol*4/sensitivity,maxit,objective,r.stop_detail))return false;
  }
  if(knot&&std::isnan(nu)) {
    std::array<accuracy_detail::Jet,3> z;for(int i=0;i<3;++i)z[i]=accuracy_detail::variable(q[i],i,i==2);
    const auto boundary=objective(z);
    const double margin=512*std::numeric_limits<double>::epsilon()*x.size();
    if(boundary.g[2]>margin)return false;
  }
  if(free[0])r.mu=static_cast<double>(static_cast<ExtendedReal>(center)+unit*static_cast<ExtendedReal>(q[0]));
  if(free[1])r.sigma=unit*std::exp(q[1]);
  if(free[2])r.nu=std::exp(q[2]);
  return (!free[0]||accuracy_detail::location_projection(r.mu,center,unit,q[0],r.sigma,tol))&&
    (!free[1]||accuracy_detail::scale_projection(r.sigma,unit,q[1],tol));
}
}
