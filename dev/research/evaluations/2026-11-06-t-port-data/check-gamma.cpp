#include "cppunivariate/distributions/lst/certificate.hpp"
#include <iostream>
#include <iomanip>
int main(){std::cout<<std::setprecision(17);for(double v:{1.,1.001,2.,3.,10.,31.999,32.,50.,128.,1e3,1e6,1e12})for(int k=0;k<4;++k){auto q=cppunivariate::t_cert_detail::gamma(v,k);std::cout<<v<<" "<<k<<" "<<q.lo<<" "<<q.hi<<std::endl;}}
