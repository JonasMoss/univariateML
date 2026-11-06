#include <cppunivariate/distributions/sged/mle.hpp>
#include <iostream>
#include <iomanip>
int main() {
  const std::vector<double> x{-3,-1,-.2,0,.3,.4,2};
  std::cout<<std::setprecision(17);
  for(double nu:{.4,.6,1.,1.001,1.5,2.,4.,20.,100.})
    for(double xi:{.1,.3,1.,3.,10.})for(bool hs:{false,true}) {
      const auto r=cppunivariate::sged_mle({x.data(),x.size()},1e-9,300,NAN,hs?1.7:NAN,nu,xi);
      std::cout<<nu<<' '<<xi<<' '<<hs<<' '<<int(r.status)<<' '<<r.certified<<' '<<r.mu<<' '<<r.sigma<<' '<<r.logLik<<'\n';
    }
}
