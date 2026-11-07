#include "cppunivariate/distributions/sged/mle.hpp"
#include <chrono>
#include <iomanip>
#include <iostream>

// The before route is the unchanged original conditional search kernel.
// Face payload reconstruction is excluded from its timing. The after route
// uses the public SGED fitter. Five fits per row; times are illustrative.
int main() {
  std::cout << std::setprecision(17);
  const std::vector<double> x{-4,-1,0,0,0,0,0,0,.3,2};
  const cppunivariate::span data{x.data(),x.size()};
  for(double nu:{.5,1.,2.,3.})for(int route:{0,1}) {
    const auto start=std::chrono::steady_clock::now();
    double ll=0;int status=0;
    for(int i=0;i<5;++i) {
      if(route) {
        const auto r=cppunivariate::sged_mle(data,1e-6,200,NAN,NAN,nu);
        ll=r.logLik;status=static_cast<int>(r.status);
      } else {
        const auto r=cppunivariate::skew_hold_detail::fit(data,false,NAN,NAN,nu,NAN,1e-6,200);
        ll=-r.f;status=r.status;
      }
    }
    std::cout << nu << ' ' << (route?"after":"before") << ' ' << status << ' '
              << ll << ' ' << std::chrono::duration<double>(
                   std::chrono::steady_clock::now()-start).count()/5 << '\n';
  }
}
