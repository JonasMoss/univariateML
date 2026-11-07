#include "cppunivariate/distributions/pig/mle.hpp"
#include <fstream>
#include <iomanip>
#include <iostream>
int main() {
 std::ifstream input("dev/research/evaluations/2026-11-06-pig-cert-data/shape-oracle-input.txt");
 double held;int cells,index=0;
 while(input>>held>>cells) {
  std::vector<int> x;
  for(int i=0;i<cells;++i){int k,count;input>>k>>count;x.insert(x.end(),count,k);}
  const auto r=cppunivariate::pig_mle({x.data(),x.size()},1e-9,200,held==0?NAN:held,NAN);
  std::cout<<index++<<","<<std::setprecision(17)<<r.mean<<","<<r.shape<<","<<r.logLik<<","<<int(r.status)<<","<<r.iterations<<"\n";
 }
}
