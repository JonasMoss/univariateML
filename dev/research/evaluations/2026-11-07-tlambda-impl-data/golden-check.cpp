#include "cppunivariate/distributions/tlambda/mle.hpp"
#include <iostream>
#include <iomanip>
#include <chrono>
int main() {
  int n;double mu,sigma,lambda;std::cin>>n>>mu>>sigma>>lambda;
  std::vector<double>x(n);for(double& v:x)std::cin>>v;
  std::cout<<std::setprecision(17);
  for(int mask:{0,3,4,5,6,7}) {
    const auto begin=std::chrono::steady_clock::now();
    const auto f=cppunivariate::tlambda_mle({x.data(),x.size()},1e-8,2000,mask&4?lambda:NAN,mask&1?mu:NAN,mask&2?sigma:NAN);
    std::cout<<mask<<' '<<int(f.status)<<' '<<f.certified<<' '<<f.mu<<' '<<f.sigma<<' '<<f.lambda<<' '<<f.logLik<<' '<<f.iterations<<' '<<std::chrono::duration<double>(std::chrono::steady_clock::now()-begin).count()<<'\n'<<std::flush;
  }
}
