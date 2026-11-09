#include "cppunivariate/distributions/tlambda/fit_inverse.hpp"
#include <iostream>
#include <iomanip>
#include <chrono>
int main(){
 std::cout<<std::setprecision(17);
 for(double l:{-32.,-2.,-1.,-.999999,-.7,-1e-12,0.,1e-12,.14,.5,.9,1-1e-12,1.,1+1e-12,1.5,2.,3.,16.}) {
  for(double az:{0.,1e-20,1e-10,.1,1.,10.,1e10,1e100,1e300}) {
   if(l>0 && l*az>=1)continue;
   double ld=l>0?std::log(std::fma(-l,az,1.)):NAN;
   auto r=cppunivariate::tlambda_fit_detail::inverse(az,l,ld);
   std::cout<<l<<' '<<az<<' '<<ld<<' '<<r.ok<<' '<<r.logp<<' '<<r.logd<<' '<<cppunivariate::distributions::dtlambda(az,0,1,l,true)<<'\n';
  }
  if(l>0)for(double deficit:{.1,1e-5,1e-15,1e-100,1e-300,std::numeric_limits<double>::denorm_min()}) {
   double ld=std::log(deficit),az=-std::expm1(ld)/l;
   auto r=cppunivariate::tlambda_fit_detail::inverse(az,l,ld);
   std::cout<<l<<' '<<az<<' '<<ld<<' '<<r.ok<<' '<<r.logp<<' '<<r.logd<<" nan\n";
  }
 }
}
