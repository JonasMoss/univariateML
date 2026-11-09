#include <cppunivariate/distributions/std/dpqr.hpp>
#include <cppunivariate/distributions/lst/dpqr.hpp>
#include <iostream>
#include <iomanip>
int main(){const double nu=std::numeric_limits<double>::max();std::cout<<std::setprecision(17);
for(double x:{std::nextafter(std::sqrt(nu),INFINITY),1.4e154})
std::cout<<nu<<","<<x<<","<<cppunivariate::distributions::dstd(x,0,1,nu,true)<<","<<cppunivariate::distributions::dlst(x,0,1,nu,true)<<","<<cppunivariate::distributions::dstd(x,0,1,nu,false)<<"\n";
}
