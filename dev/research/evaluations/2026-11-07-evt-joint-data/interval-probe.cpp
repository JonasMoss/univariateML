#include "cppunivariate/distributions/gev/mle.hpp"
#include <iomanip>
#include <iostream>
int main() {
 const std::vector<double> x{-.7,-.2,0,.4,.8,1.5,3};
 std::vector<double> z;for(double v:x)z.push_back((v-.1)/1.3);
 std::cout<<std::setprecision(17);
 for(int j=0;j<100;++j) {
   const double lo=-.4+j*.0135,hi=lo+.01;
   bool ok=true;const auto b=cppunivariate::gev_shape_detail::likelihood({z.data(),z.size()},lo,hi,1000,ok);
   if(!ok)return 1;
   std::cout<<lo<<" "<<hi<<" "<<b.v.lo<<" "<<b.v.hi<<" "<<b.d.lo<<" "<<b.d.hi<<" "<<b.dd.lo<<" "<<b.dd.hi<<"\n";
 }
}
