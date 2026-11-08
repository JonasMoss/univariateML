// End-to-end common-binding budget probe; no optimizer result is accepted on
// a stopped certificate. Run from the repository root after compiling with
// -Ibindings -IunivariateML-cpp/include and the installed Boost/BH include.
#include "common/distributions/ged/fit.hpp"
#include <iomanip>
#include <iostream>
#include <vector>
int main() {
  std::cout<<std::setprecision(17);
  for(int n:{40,400,4000,40000}) {
    std::vector<double>x;for(int j=0;j<n;++j)x.push_back(-1.+2.*j/(n-1.));
    for(int budget:{1,2,10}) {
      const auto r=umvl_binding::fit_ged({x.data(),x.size()},{},{},{},1e-6,budget);
      std::cout<<n<<" "<<budget<<" "<<r.error_kind<<" "<<r.candidates.size()<<" "
               <<r.loglik_lower<<" "<<r.loglik_upper<<"\n";
      if(r.error_kind=="budget" && (r.candidates.empty() || !(r.loglik_lower<=r.loglik_upper)))return 1;
      if(r.error_kind=="limit" && !(r.loglik_lower<=-n*std::log(2.) && -n*std::log(2.)<=r.loglik_upper))return 2;
    }
  }
}
