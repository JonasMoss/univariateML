#include "cppunivariate/distributions/bhatt/normal_accuracy.hpp"
#include <iostream>
#include <vector>
inline double read_binary() {std::uint64_t m;int exponent,negative;std::cin>>m>>exponent>>negative;return (negative?-1:1)*std::ldexp(static_cast<double>(m),exponent);}
int main(){int n;while(std::cin>>n){const double center=read_binary(),scale=read_binary();std::vector<double> x(n);for(double& v:x)v=read_binary();const cppunivariate::span data{x.data(),x.size()};std::cout<<cppunivariate::bhatt_detail::normal_stationary_exact(data,center,scale,true,true)<<' '<<cppunivariate::bhatt_detail::normal_width_score_sign(data,center,scale,false)<<' '<<cppunivariate::bhatt_detail::normal_width_score_sign(data,center,scale,true)<<' '<<cppunivariate::bhatt_detail::normal_symmetric_exact(x.front(),x.back(),center)<<'\n';}}
