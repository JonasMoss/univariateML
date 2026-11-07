#ifdef ZIP_BASELINE_HEADER
#include ZIP_BASELINE_HEADER
#else
#include "cppunivariate/distributions/zip/mle.hpp"
#endif
#include <chrono>
#include <iostream>
#include <vector>
int main() {
  std::vector<int> x(100,0); x.back()=10;
  volatile double sink=0;
  for(double p:{double(NAN),.001,.1,.9}) {
    const auto begin=std::chrono::steady_clock::now();
    for(int j=0;j<10000;++j) {auto r=cppunivariate::zip_mle({x.data(),x.size()},NAN,p);sink+=r.lambda;}
    std::cout<<p<<" "<<std::chrono::duration<double,std::micro>(std::chrono::steady_clock::now()-begin).count()/10000<<" us/fit\n";
  }
  return sink==0;
}
