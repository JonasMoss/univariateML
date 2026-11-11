#include "cppunivariate/distributions/ztpois/profile.hpp"
#include <iostream>
#include <iomanip>
int main() {
  int x; double n,p;
  while(std::cin>>x>>n>>p)
    std::cout<<std::setprecision(17)<<cppunivariate::zero_detail::bin_logpmf(x,n,p)<<'\n';
}
