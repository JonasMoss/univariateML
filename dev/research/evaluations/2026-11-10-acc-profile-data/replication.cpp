#include <iostream>
#include <iomanip>
#include <vector>
#include <cmath>
#include "cppunivariate/distributions/genbeta/mle.hpp"
// Fixed shape2=2, power=1: psi(a+2)-psi(a)=1/a+1/(a+1).
// The unique shape maximizer is unchanged by replicating an observation.
int main(){
  const std::vector<double> x(10000000,.999999);
  const long double c=-std::log(static_cast<long double>(x[0]));
  const long double reference=(2-c+std::sqrt(c*c+4))/(2*c);
  const auto r=cppunivariate::genbeta_mle({x.data(),x.size()},1,1e-10,300,NAN,2,1);
  std::cout<<std::setprecision(17)<<"{\"n\":"<<x.size()<<",\"x\":"<<x[0]<<",\"status\":"<<static_cast<int>(r.status)<<",\"shape1\":"<<r.shape1<<",\"reference\":"<<static_cast<double>(reference)<<",\"relative_error\":"<<static_cast<double>(std::abs((r.shape1-reference)/reference))<<"}\n";
  return r.status==cppunivariate::GenbetaStatus::Ok&&std::abs((r.shape1-reference)/reference)<=1e-10?0:1;
}
