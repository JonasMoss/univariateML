#include "cppunivariate/distributions/tlambda/mle.hpp"
#include <iostream>
#include <iomanip>
int main(){
 const double tiny=std::numeric_limits<double>::denorm_min(),mu=2*tiny;
 const double gap=cppunivariate::tlambda_detail::slack(2,mu,1,.5);
 const double fast=cppunivariate::tlambda_fit_detail::logdensity(2,mu,1,.5);
 const double old=cppunivariate::distributions::dtlambda(2,mu,1,.5,true);
 if(gap!=tiny || !std::isfinite(fast) || std::abs(fast-std::log(tiny))>2e-12)return 1;
 std::cout<<std::setprecision(17)<<"gap="<<gap<<" fast="<<fast<<" dtlambda="<<old<<" reference="<<std::log(tiny)<<'\n';
 const double l=std::nextafter(1.,2.);
 const double inner=cppunivariate::tlambda_fit_detail::logdensity(1,tiny,l,l);
 const double alias=cppunivariate::distributions::dtlambda(1,tiny,l,l,true);
 if(!std::isfinite(inner) || std::abs(inner-alias)<.5)return 1;
 std::cout<<"alias lambda="<<l<<" fast="<<inner<<" public="<<alias<<'\n';
}
