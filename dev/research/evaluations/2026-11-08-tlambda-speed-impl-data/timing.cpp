#include "cppunivariate/distributions/tlambda/mle.hpp"
#include <chrono>
#include <iostream>
#include <iomanip>
#include <vector>
int main(int argc,char**) {
  std::cout<<std::setprecision(17);
  for(int n: {101,500,1000,10000,100000}) {
    std::vector<double> x;
    for(int i=0;i<n;++i)x.push_back(cppunivariate::distributions::qtlambda((i+.5)/n,0,1,.14));
    for(int mask: {0,1,2,3,4,5,6,7}) {
      if(n>=1000 && mask!=0)continue;
      if(argc>1 && n==100000)continue;
      const double nan=NAN;
      const auto begin=std::chrono::steady_clock::now();
      const auto f=cppunivariate::tlambda_mle({x.data(),x.size()},1e-7,1000,mask&4?.14:nan,mask&1?0:nan,mask&2?1:nan);
      const double seconds=std::chrono::duration<double>(std::chrono::steady_clock::now()-begin).count();
      std::cout<<n<<','<<mask<<','<<seconds<<','<<int(f.status)<<','<<f.mu<<','<<f.sigma<<','<<f.lambda<<','<<f.logLik<<'\n'<<std::flush;
    }
  }
}
