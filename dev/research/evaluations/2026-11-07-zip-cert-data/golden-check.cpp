#include "cppunivariate/distributions/zip/mle.hpp"
#include <iostream>
#include <iomanip>
#include <vector>
int main() {
  int n;std::cin>>n;std::vector<int>x(n);for(auto& v:x)std::cin>>v;
  double lambda,pi;std::cin>>lambda>>pi;
  std::cout<<std::setprecision(17);
  for(unsigned mask=0;mask<4;++mask) {
    const auto r=cppunivariate::zip_mle({x.data(),x.size()},mask&1?lambda:NAN,mask&2?pi:NAN);
    std::cout<<mask<<" "<<int(r.status)<<" "<<r.lambda<<" "<<r.pi<<" "<<r.logLik<<"\n";
  }
}
