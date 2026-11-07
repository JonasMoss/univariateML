#include "cppunivariate/distributions/zip/mle.hpp"
#include <iomanip>
#include <iostream>
#include <vector>
int main() {
  int z,k,count;double p;
  std::cout<<std::setprecision(17);
  while(std::cin>>z>>k>>count>>p) {
    std::vector<int>x(z,0);x.insert(x.end(),k,count);
    const auto r=cppunivariate::zip_mle({x.data(),x.size()},NAN,p);
    std::cout<<int(r.status)<<" "<<r.lambda<<" "<<r.logLik<<"\n";
  }
}
