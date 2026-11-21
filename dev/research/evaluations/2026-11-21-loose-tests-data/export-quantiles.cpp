#include "cppunivariate/special/special.hpp"
#include "cppunivariate/distributions/norm/tnorm_dpqr.hpp"
#include "cppunivariate/distributions/huber/dpqr.hpp"
#include <iostream>
#include <cmath>
int main() {
  namespace d = cppunivariate::distributions;
  std::cout << std::hexfloat << "std";
  for (int i=0; i<4000; ++i)
    std::cout << ',' << cppunivariate::special::qt((i+.5)/4000,300,true,false);
  std::cout << '\n' << "huber";
  for (int i=0; i<2000; ++i)
    std::cout << ',' << d::qhuber((i+.5)/2000,2,3,.8,true,false);
  struct Case { double mean, sd, a, b; };
  unsigned index=0;
  for (const auto c : {Case{.3,.25,0,1},Case{0,1,-1,2},Case{2,1,0,INFINITY},
                       Case{0,1,1,INFINITY},Case{0,1,-INFINITY,-.5},
                       Case{5,3,-INFINITY,4},Case{-3,2,-4,10}}) {
    std::cout << '\n' << "tnorm/" << index++ << ',' << c.a << ',' << c.b;
    for (int i=0; i<300; ++i)
      std::cout << ',' << d::qtnorm((i+.5)/300,c.mean,c.sd,c.a,c.b,true,false);
  }
  std::cout << '\n';
}
