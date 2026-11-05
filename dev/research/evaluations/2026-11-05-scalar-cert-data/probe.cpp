#include "cppunivariate/distributions/nbinom/mle.hpp"
#include "cppunivariate/distributions/kumar/mle.hpp"
#include <iostream>
#include <iomanip>
#include <vector>
#include <string>
int main() {
  std::string family;int n;
  std::cout<<std::setprecision(17);
  while(std::cin>>family>>n) {
    if(family=="nbinom") {
      std::vector<int> x(n);for(auto& v:x)std::cin>>v;
      auto r=cppunivariate::nbinom_mle({x.data(),x.size()},1e-10,200);
      std::cout<<int(r.status)<<' '<<r.size<<' '<<r.prob<<' '<<r.logLik<<' '<<r.iterations<<'\n';
    } else {
      std::vector<double> x(n);for(auto& v:x)std::cin>>v;
      auto r=cppunivariate::kumar_mle({x.data(),x.size()},1e-10,200);
      std::cout<<int(r.status)<<' '<<r.a<<' '<<r.b<<' '<<r.logLik<<' '<<r.iterations<<'\n';
    }
  }
}
