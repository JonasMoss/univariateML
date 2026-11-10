#include "cppunivariate/special/special.hpp"
#include "cppunivariate/distributions/emg/mle.hpp"
#include "cppunivariate/distributions/triang/mle.hpp"
#include "cppunivariate/distributions/gev/mle.hpp"
#include <iostream>
#include <iomanip>
#include <cstdlib>
int main(){const bool defaults=std::getenv("AUDIT_SUPPORT_DEFAULTS")!=nullptr;std::cout<<std::setprecision(17);std::string f;int id,n;double h[3];while(std::cin>>f>>id>>n){for(double&v:h){std::string s;std::cin>>s;v=std::stod(s);}std::vector<double>x(n);for(double&v:x)std::cin>>v;cppunivariate::span data{x.data(),x.size()};std::cout<<id<<","<<f<<",";
if(f=="emg"){auto r=cppunivariate::emg_mle(data,defaults?1e-8:1e-10,defaults?500:100,h[0],h[1],h[2]);std::cout<<int(r.status)<<","<<r.logLik<<","<<r.mu<<","<<r.sigma<<","<<r.lambda;}
if(f=="triang"){auto r=cppunivariate::triang_mle(data,defaults?1e-12:1e-10,100,h[0],h[1],h[2],std::getenv("AUDIT_SUPPORT_EXHAUSTIVE")==nullptr);std::cout<<int(r.status)<<","<<r.logLik<<","<<r.a<<","<<r.b<<","<<r.c;}
if(f=="gev"){auto r=cppunivariate::gev_mle(data,1e-10,100,h[2],h[0],h[1]);std::cout<<int(r.status)<<","<<r.logLik<<","<<r.mu<<","<<r.sigma<<","<<r.xi;}
std::cout<<std::endl;}}
