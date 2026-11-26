#include "bindings/common/distributions/nsbeta/fit.hpp"
#include <iostream>
#include <iomanip>
int main() {
  std::cout << std::setprecision(17);
  for (int k : {45, 50, 53, 60}) for (double gap : {.125,.25,.5}) for (bool reflected : {false,true}) {
    const double d=std::ldexp(1.,k);
    const double x[]{0,reflected?-gap:gap};
    const double lo=reflected?-1:-d, hi=reflected?d:1;
    for(int mask=0;mask<5;++mask) {
      const double a=mask==4 ? 2 : (mask&1 ? 1 : NAN);
      const double b=mask==4 ? 3 : (mask&2 ? 1 : NAN);
      const auto r=cppunivariate::nsbeta_mle({x,2},lo,hi,1e-10,1000,a,b);
      std::cout<<k<<','<<gap<<','<<reflected<<','<<mask<<','<<int(r.status)<<','<<r.shape1<<','<<r.shape2<<','<<r.logLik<<'\n';
    }
  }
}
