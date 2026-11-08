#include "cppunivariate/distributions/gb2/mle.hpp"
#include <iostream>
#include <iomanip>
int main() {
 std::cout<<std::setprecision(17);
 for(double t:{5.6463,5.646438395028704,5.6466})for(bool inverse:{false,true}) {
  std::vector<double>x{1.9901008294052028e-5,2.209857117039297e-5,.0002450722434289249,12.288342947780086,942.976056905759,16.239512580216314,2.898690421966857,1.2684036923492177,400970.63293690106*std::exp(t)};
  for(double& v:x)v=std::pow(v,inverse?-.5:.5);
  const auto r=cppunivariate::gb2_mle({x.data(),x.size()},1e-7,1000,inverse?.22690393135372058:NAN,2,inverse?NAN:.22690393135372058,NAN);
  std::cout<<t<<" "<<inverse<<" "<<int(r.status)<<" "<<r.shape1<<" "<<r.shape3<<" "<<r.scale<<" "<<r.logLik<<" "<<r.iterations<<" "<<r.stop_detail.comparison_tie<<" "<<r.stop_detail.candidates.size()<<"\n";
 }
}
