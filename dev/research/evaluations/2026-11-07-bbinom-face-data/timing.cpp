#include "cppunivariate/distributions/bbinom/mle.hpp"
#include <chrono>
#include <iostream>
#include <vector>
int main() {
  const std::vector<std::vector<int>> samples{
    {0,1,1,2,2,3,4,5,7,10}, {4,5,5,6,5,4,6,5,5,5}, {0,1,1,2}};
  for(std::size_t k=0;k<samples.size();++k) {
    const auto& x=samples[k];
    const auto start=std::chrono::steady_clock::now();
    cppunivariate::BbinomResult result{};
    for(int i=0;i<100;++i)
      result=cppunivariate::bbinom_mle({x.data(),x.size()},k==2?2:10);
    std::cout<<k<<" "<<int(result.status)<<" "<<result.alpha<<" "<<result.beta<<" "
             <<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\n";
  }
}
