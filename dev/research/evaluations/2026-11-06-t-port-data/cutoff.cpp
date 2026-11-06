#include "cppunivariate/distributions/lst/certificate.hpp"
#include <iostream>
#include <iomanip>
int main(){std::cout<<std::setprecision(17);for(double u:{1e-8,1e-4,std::nextafter(.01,0.),.01,std::nextafter(.01,1.),.02,1.,10.}){auto r=cppunivariate::t_cert_detail::lr(cppunivariate::t_cert_detail::I(u));auto p=cppunivariate::t_cert_detail::nl(cppunivariate::t_cert_detail::I(u));auto n=cppunivariate::t_cert_detail::nl(cppunivariate::t_cert_detail::I(-u));std::cout<<u<<" "<<r.lo<<" "<<r.hi<<" "<<p.lo<<" "<<p.hi<<" "<<n.lo<<" "<<n.hi<<std::endl;}}
