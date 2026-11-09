#include <chrono>
#include <iomanip>
#include <iostream>
#include "json_loader.hpp"
#ifdef SGED_BASELINE
#include "sged-baseline.hpp"
#else
#include "cppunivariate/distributions/sged/mle.hpp"
#endif
int main(){
 const std::vector<double> x{-3,-1,-.2,0,.3,.4,2};
 std::cout<<std::setprecision(17)<<"shape\tstatus\tcertified\txi\tll\tms\twork\n";
 for(double p:{.5,1.,1.7,4.,10.}) {
  const auto start=std::chrono::steady_clock::now();
  const auto r=cppunivariate::sged_mle({x.data(),x.size()},1e-10,200,.1,1.4,p);
  std::cout<<p<<'\t'<<int(r.status)<<'\t'<<r.certified<<'\t'<<r.xi<<'\t'<<r.logLik<<'\t'<<std::chrono::duration<double,std::milli>(std::chrono::steady_clock::now()-start).count()<<'\t'<<r.iterations<<'\n';
 }
}
