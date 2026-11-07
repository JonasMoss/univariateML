#include "cppunivariate/distributions/betapr/mle.hpp"
#include <iostream>
#include <iomanip>
int main(){
  std::cout<<std::setprecision(17);
  for(double t:{0.,5.6463,5.646438395028704,5.6466}) {
    std::vector<double> x{1.9901008294052028e-5,2.209857117039297e-5,
      .0002450722434289249,12.288342947780086,942.976056905759,16.239512580216314,
      2.898690421966857,1.2684036923492177,400970.63293690106*std::exp(t)};
    const auto r=cppunivariate::betapr_mle_holds({x.data(),x.size()},.22690393135372058,NAN,NAN);
    std::cout<<t<<" "<<int(r.status)<<" "<<r.beta<<" "<<r.scale<<" "<<r.logLik<<" "
      <<r.stop_detail.comparison_tie<<" "<<r.iterations<<" "<<r.stop_detail.candidates.size()<<"\n";
  }
}
