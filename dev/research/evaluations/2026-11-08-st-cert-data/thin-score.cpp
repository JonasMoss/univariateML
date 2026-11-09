#include "cppunivariate/distributions/st/mle.hpp"
#include <iostream>
#include <iomanip>
int main(){std::cout<<std::setprecision(17);
 for(double nu:{1000.,1e6,1e12,1e20})for(double a:{1000.,1e6,1e12,1e30})for(double z:{-1.,-10.,-1000.}) {
  const auto r=cppunivariate::st_cert_detail::location_term(z,z,a,nu);
  std::cout<<nu<<' '<<a<<' '<<z<<' '<<r.lo<<' '<<r.hi<<'\n';
 }
}
