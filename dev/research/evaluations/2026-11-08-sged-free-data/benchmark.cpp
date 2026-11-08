// Compile before and after the change; frozen input is supplied on stdin.
#include "cppunivariate/distributions/sged/mle.hpp"
#include <chrono>
#include <iostream>
#include <iomanip>
#include <vector>
int main() {
  std::vector<double> x; double value;
  while(std::cin>>value)x.push_back(value);
  std::cout<<std::setprecision(17)<<"nu,xi,held_mask,ms,status,certified,mean,sd,shape,skew,loglik\n";
  for(double nu:{double(NAN),.7,1.,1.7,2.,4.}) {
    for(double xi:{1.,1.4,double(NAN)}) {
      if(!std::isnan(nu) && !std::isnan(xi))continue;
      for(int mask=0;mask<4;++mask) {
        const auto start=std::chrono::steady_clock::now();
        const auto r=cppunivariate::sged_mle({x.data(),x.size()},1.220703e-4,200,
          mask&1?.2:NAN,mask&2?1.4:NAN,nu,xi);
        const double ms=std::chrono::duration<double,std::milli>(std::chrono::steady_clock::now()-start).count();
        std::cout<<nu<<','<<xi<<','<<mask<<','<<ms<<','<<int(r.status)<<','<<r.certified<<','<<r.mu<<','<<r.sigma<<','<<r.nu<<','<<r.xi<<','<<r.logLik<<std::endl;
      }
    }
  }
}
