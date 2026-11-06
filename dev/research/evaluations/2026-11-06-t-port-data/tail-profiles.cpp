#include "cppunivariate/distributions/lst/certificate.hpp"
#include <iostream>
#include <iomanip>
int main(){std::size_t n;std::cout<<std::setprecision(17);while(std::cin>>n){std::vector<double>x(n);for(double&v:x)std::cin>>v;cppunivariate::t_cert_detail::Data d;cppunivariate::t_cert_detail::data({x.data(),x.size()},d);cppunivariate::t_cert_detail::Engine e(d,1,4096,200);for(double v:{1.,1.1,2.,10.,100.,1000.,1e4,1e6,1e8,1e10,1e12,1e14}){auto p=e.at(v);std::cout<<v<<" "<<p.m<<" "<<p.t<<" "<<p.delta.lo<<" "<<p.delta.hi<<" "<<e.failure<<std::endl;}cppunivariate::t_cert_detail::Tail tail;auto p=e.at(1.);bool ok=cppunivariate::t_cert_detail::tail(e,p.delta.lo,tail);std::cout<<"TAIL "<<ok<<" "<<tail.N<<" "<<tail.upper<<std::endl;}}
