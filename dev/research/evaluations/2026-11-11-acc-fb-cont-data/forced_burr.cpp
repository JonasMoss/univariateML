#include "cppunivariate/distributions/burr/mle.hpp"
#include <iostream>
#include <iomanip>
int main(){const std::vector<double> x{.2,.5,1,2,4};const cppunivariate::span data{x.data(),x.size()};const auto p=cppunivariate::burr_mle_legacy(data,1e-8,1000,NAN,NAN,NAN,1e-12,true);const auto f=cppunivariate::burr_mle_certified(data,1e-8,1000,NAN,NAN,NAN,1e-12,0);std::cout<<std::setprecision(17)<<"{\"shape1\":"<<p.shape1<<",\"shape2\":"<<p.shape2<<",\"scale\":"<<p.scale<<",\"logLik\":"<<p.logLik<<",\"proposal_status\":"<<static_cast<int>(p.status)<<",\"final_status\":"<<static_cast<int>(f.status)<<",\"witnesses\":"<<f.stop_detail.candidates.size()<<",\"box_below_minimum\":"<<std::boolalpha<<(p.scale/(1-1e-8)<x.front())<<"}\n";}
