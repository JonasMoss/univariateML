#include "cppunivariate/special/skewed_densities.hpp"
#include <chrono>
#include <iomanip>
#include <iostream>
#include <cmath>
// Retain the old formulas explicitly so the comparison is reproducible.
double old_t(double n) {
 return std::exp(std::lgamma(.5*(n-1))-std::lgamma(.5*n))*std::sqrt((n-2)/cppunivariate::pi);
}
double old_g(double n) {
 const double lam=std::sqrt(std::pow(2.,-2/n)*std::tgamma(1/n)/std::tgamma(3/n));
 return std::pow(2.,1/n)*lam*std::tgamma(2/n)/std::tgamma(1/n);
}
double old_const(double n) {
 const double t=1/n;
 const double r=n>=128 ? .5*std::log(.5*n)-.25*t+t*t*t/24-std::pow(t,5)/20
   : std::lgamma(.5*(n+1))-std::lgamma(.5*n);
 return r-.5*std::log((n-2)*cppunivariate::pi);
}
template<class F> double timing(F f,double n) {
 volatile double sink=0;
 const auto start=std::chrono::steady_clock::now();
 for(int i=0;i<200000;++i) sink+=f(n+(i%17)*n*1e-6);
 return std::chrono::duration<double,std::nano>(std::chrono::steady_clock::now()-start).count()/200000;
}
int main(){
 std::cout<<std::setprecision(6)<<"shape,old_t_ns,new_t_ns,old_ged_ns,new_ged_ns,old_const_ns,new_const_ns\n";
 for(double n:{5.,127.,128.,129.,1e4,1e12})
  std::cout<<n<<','<<timing(old_t,n)<<','<<timing(cppunivariate::dens::std_abs_moment,n)<<','
   <<timing(old_g,n)<<','<<timing(cppunivariate::dens::ged_abs_moment,n)<<','
   <<timing(old_const,n)<<','<<timing(cppunivariate::dens::log_dstd_unit_const,n)<<'\n';
}
