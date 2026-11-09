#include <cppunivariate/distributions/sged/dpqr.hpp>
#include <cppunivariate/distributions/sstd/dpqr.hpp>
#include <cppunivariate/distributions/lst/dpqr.hpp>
#include <iostream>
#include <iomanip>
int main() {
  std::cout<<std::setprecision(17);
  for(double nu:{1e8,1e100,std::numeric_limits<double>::max()}) {
    for(double p:{.01,.25,.75,.99})
      std::cout<<nu<<",ged,"<<p<<","<<cppunivariate::distributions::qged(p,0,1,nu,true,false)<<"\n";
    std::cout<<nu<<",sged,0.25,"<<cppunivariate::distributions::qsged(.25,0,1,nu,2)<<"\n";
    std::cout<<nu<<",sstd,0.25,"<<cppunivariate::distributions::qsstd(.25,0,1,nu,2)<<"\n";
    for(double x:{1.,2.})
      std::cout<<nu<<",gamma-upper,"<<x<<","<<cppunivariate::ged_detail::gamma_power_log(std::log(x)/nu,nu,false)<<"\n";
    std::cout<<nu<<",lst-density,1,"<<cppunivariate::distributions::dlst(1,0,1,nu,false)<<"\n";
  }
}
