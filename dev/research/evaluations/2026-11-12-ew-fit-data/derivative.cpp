#include "cppunivariate/distributions/expweibull/mle.hpp"
#include <iostream>
#include <iomanip>
int main() {
 std::cout<<std::setprecision(17);
 std::vector<double> y{-.7,.1,.4,.9};
 for(bool profile:{false,true})for(double a:{-.5,0.,2.,20.})for(double b:{.5,1.5})for(double c:{.2,2.5}) {
  const std::array<double,3> p{a,std::log(b),std::log(c)};
  std::array<cppunivariate::accuracy_detail::Jet,3> v;
  for(int i=0;i<3;++i)v[i]=cppunivariate::accuracy_detail::variable(p[i],i,true);
  const cppunivariate::expweibull_fit_detail::Likelihood objective{{y.data(),y.size()},1,profile};
  const auto f=objective(v);
  std::cout<<profile<<","<<p[0]<<","<<p[1]<<","<<p[2]<<","<<double(f.v);
  for(int i=0;i<3;++i)std::cout<<","<<double(f.g[i]);
  for(int i=0;i<3;++i)for(int j=0;j<3;++j)std::cout<<","<<double(f.h[i][j]);
  std::cout<<"\n";
 }
}
