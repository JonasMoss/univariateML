#include <cppunivariate/distributions/ged/dpqr.hpp>
#include <iostream>
#include <iomanip>
int main(){std::cout<<std::setprecision(17);
for(double nu:{1e8,1e100,std::numeric_limits<double>::max()})
for(double x:{std::nextafter(std::sqrt(3.),0.),std::sqrt(3.),std::nextafter(std::sqrt(3.),INFINITY)})
std::cout<<nu<<","<<x<<","<<cppunivariate::distributions::dged(x,0,1,nu,true)<<","<<cppunivariate::distributions::pged(x,0,1,nu,false,true)<<"\n";
}
