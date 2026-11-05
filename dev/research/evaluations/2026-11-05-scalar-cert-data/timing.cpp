#include "cppunivariate/distributions/nbinom/mle.hpp"
#include "cppunivariate/distributions/kumar/mle.hpp"
#include <chrono>
#include <iostream>
#include <string>
#include <vector>
int main() {
  std::string family;int n;
  volatile double sink=0;
  while(std::cin>>family>>n) {
    std::vector<double> x(n);std::vector<int> ix(n);
    for(int j=0;j<n;++j){std::cin>>x[j];ix[j]=int(x[j]);}
    for(bool certified:{false,true}) {
      auto begin=std::chrono::steady_clock::now();
      const int repeats=100;
      for(int j=0;j<repeats;++j) {
        if(family=="nbinom") {
          const auto r=certified?cppunivariate::nbinom_mle({ix.data(),ix.size()},1.220703e-4,100):cppunivariate::nbinom_candidate_mle({ix.data(),ix.size()},1.220703e-4,100);
          sink+=r.logLik;
        } else {
          const auto r=certified?cppunivariate::kumar_mle({x.data(),x.size()},1.220703e-4,100):cppunivariate::kumar_candidate_mle({x.data(),x.size()},1.220703e-4,100);
          sink+=r.logLik;
        }
      }
      std::cout<<family<<' '<<(certified?"certified":"proposal")<<' '<<n<<' '<<std::chrono::duration<double,std::micro>(std::chrono::steady_clock::now()-begin).count()/repeats<<" us/fit\n";
    }
  }
}
