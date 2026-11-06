#include "cppunivariate/distributions/lst/certificate.hpp"
#include <iostream>
#include <iomanip>
int main(){std::cout<<std::setprecision(17);double v,m,t,width;std::size_t n;
while(std::cin>>n>>v>>m>>t>>width){std::vector<double>x(n);for(double&z:x)std::cin>>z;
cppunivariate::t_cert_detail::Data d;if(!cppunivariate::t_cert_detail::data({x.data(),x.size()},d))return 1;
auto h=cppunivariate::t_cert_detail::ordinary(d,{{{m-width,m+width},{t-width,t+width},{1-width,1+width}}},v);
auto g=cppunivariate::t_cert_detail::gamma(v,4);
std::cout<<d.center<<" "<<d.unit<<" "<<g.lo<<" "<<g.hi;
for(auto z:h.g)std::cout<<" "<<z.lo<<" "<<z.hi;
for(auto row:h.h)for(auto z:row)std::cout<<" "<<z.lo<<" "<<z.hi;
std::cout<<std::endl;}}
