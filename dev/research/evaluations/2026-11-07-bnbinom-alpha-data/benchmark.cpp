#include "cppunivariate/distributions/bnbinom/mle.hpp"
#include <chrono>
#include <fstream>
#include <iostream>
#include <iomanip>
int main(int argc, char **argv) {
  if(argc!=2) return 1;
  std::ifstream input(argv[1]); std::vector<int> x; int value;
  while(input>>value)x.push_back(value);
  if(x.empty())return 2;
  std::cout<<std::setprecision(17);
  const std::vector<double> p{1.0672234787479615,2.1419791546447171,5.3647338351087397};
  for(int mask=0;mask<8;++mask) {
    const auto begin=std::chrono::steady_clock::now();
    cppunivariate::BnbinomResult f{};
    for(int repeat=0;repeat<5;++repeat)
      f=cppunivariate::bnbinom_mle({x.data(),x.size()},1e-8,600,
        mask&1?p[0]:NAN,mask&2?p[1]:NAN,mask&4?p[2]:NAN);
    const double ms=std::chrono::duration<double,std::milli>(std::chrono::steady_clock::now()-begin).count()/5;
    std::cout<<mask<<','<<static_cast<int>(f.status)<<','<<f.size<<','<<f.alpha<<','<<f.beta<<','<<f.logLik<<','<<ms<<'\n';
  }
}
