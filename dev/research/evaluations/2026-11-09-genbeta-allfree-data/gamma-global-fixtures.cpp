#include "cppunivariate/distributions/genbeta/mle.hpp"
#include <iostream>
int main(){for(int n:{100,300})for(double floor:{.001,.05}){
 std::vector<double>x{std::exp(-floor)};
 for(int i=1;i<n;++i)x.push_back(std::exp(-(.5+(i-.5)/(n-1))));
 const auto r=cppunivariate::genbeta_mle({x.data(),x.size()},1);
 std::cout<<n<<' '<<floor<<' '<<int(r.status)<<' '<<r.allfree_certified<<' '<<int(r.certificate_failure)<<' '<<r.iterations<<' '<<r.logLik<<std::endl;
}}
