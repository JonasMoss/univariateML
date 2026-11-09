#include "cppunivariate/distributions/tlambda/fit_inverse.hpp"
#include <chrono>
#include <iostream>
#include <iomanip>
int main(){std::cout<<std::setprecision(10);volatile double sum=0;
for(double l:{-2.,-1.,-.7,-.05,0.,.14,.7,.99,1.5,3.}){
 std::array<double,10001>x{};for(int i=0;i<10001;++i)x[i]=cppunivariate::distributions::qtlambda((i+.5)/10001,0,1,l);
 auto now=[](){return std::chrono::steady_clock::now();};
 auto start=now();for(int k=0;k<5;++k)for(double v:x)sum+=cppunivariate::distributions::dtlambda(v,0,1,l,true);
 auto mid=now();for(int k=0;k<5;++k)for(double v:x)sum+=cppunivariate::tlambda_fit_detail::logdensity(v,0,1,l);
 auto end=now();double error=0;int failed=0;
 for(double v:x){double a=cppunivariate::distributions::dtlambda(v,0,1,l,true),b=cppunivariate::tlambda_fit_detail::logdensity(v,0,1,l);if(!std::isfinite(b))++failed;error=std::max(error,std::abs(a-b)/(1+std::abs(a)));}
 std::cout<<l<<','<<std::chrono::duration<double,std::nano>(mid-start).count()/50005<<','<<std::chrono::duration<double,std::nano>(end-mid).count()/50005<<','<<error<<','<<failed<<'\n';
}}
