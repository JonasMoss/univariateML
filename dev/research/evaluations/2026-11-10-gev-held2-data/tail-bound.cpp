#include "cppunivariate/special/special.hpp"
#include "cppunivariate/distributions/gev/mle.hpp"
#include "cppunivariate/distributions/gev/dpqr.hpp"
#include <iomanip>
#include <iostream>
int main(int argc,char** argv) {
  std::vector<double> x(500);
  for(std::size_t i=0;i<x.size();++i)x[i]=static_cast<double>(i)/5000;
  const auto e=cppunivariate::gev_held_detail::gev_evaluate({x.data(),x.size()},{{-1e-16,0},{704,705}},false,1,100);
  double mean=0;for(double v:x)mean+=cppunivariate::distributions::dgev(v,704,1,0,true)/x.size();
  std::cout<<std::setprecision(17)<<(argc>1?argv[1]:"after")<<','<<e.upper<<','<<mean<<','<<(e.upper< -200)<<'\n';
}
