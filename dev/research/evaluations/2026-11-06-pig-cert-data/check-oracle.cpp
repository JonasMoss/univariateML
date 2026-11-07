#include "cppunivariate/distributions/pig/mle.hpp"
#include <iomanip>
#include <iostream>
int main() {
 int index=0;
 for(const std::vector<int> x : {std::vector<int>{1}, {0,0,0,0,10}, {1,2,3,5}, {0,1,2,3}})
 for(double s : {1e-20,.01,3.,1e20}) {
  const auto r=cppunivariate::pig_mle({x.data(),x.size()},1e-10,200,NAN,s);
  std::cout<<index++<<","<<std::setprecision(17)<<r.mean<<","<<r.logLik<<","<<int(r.status)<<"\n";
 }
}
