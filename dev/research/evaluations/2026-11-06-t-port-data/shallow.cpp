#include "cppunivariate/distributions/lst/certificate.hpp"
#include "cppunivariate/special/special.hpp"
#include <iostream>
#include <iomanip>
int main(){std::cout<<std::setprecision(17);std::vector<double>x;for(int i=0;i<4000;++i)x.push_back(cppunivariate::special::qt((i+.5)/4000,300,true,false));cppunivariate::t_cert_detail::Data d;cppunivariate::t_cert_detail::data({x.data(),x.size()},d);for(double floor:{1.,2.}){cppunivariate::t_cert_detail::Engine e(d,floor,4096,500);cppunivariate::t_cert_detail::Profile best;best.delta={-INFINITY,-INFINITY};for(int j=0;j<=32;++j){auto p=e.at(floor*std::exp(std::log(1000/floor)*j/32));if(p.delta.lo>best.delta.lo)best=p;}cppunivariate::t_cert_detail::Isolation iso;bool ok=cppunivariate::t_cert_detail::isolate(e,best,iso);std::cout<<floor<<" "<<best.v<<" "<<ok<<" "<<iso.stage<<" "<<iso.v0;for(double p:iso.p)std::cout<<" "<<p;std::cout<<std::endl;}}
