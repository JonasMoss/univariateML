#include <cppunivariate/distributions/gengamma/dpqr.hpp>
#include <cmath>
#include <iomanip>
#include <iostream>
int main() {
  std::cout<<std::setprecision(17);
  for (int k=0;k<=300;++k) {
    const double lx=-35.9+k*0.16;
    std::cout<<lx<<' '<<cppunivariate::distributions::gengamma_detail::log_e1(lx)<<'\n';
  }
}
