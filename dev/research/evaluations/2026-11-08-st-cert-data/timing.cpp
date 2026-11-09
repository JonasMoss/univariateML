#include "cppunivariate/distributions/st/mle.hpp"
#include <chrono>
#include <iomanip>
#include <iostream>
int main() {
 std::cout<<std::setprecision(17);
 for(int n:{7,500}) {
  std::vector<double> x{-3,-1,-.2,.1,.8,2,4};
  if(n==500){x.clear();for(int i=0;i<n;++i){double z=std::tan(2.5*((i+.5)/n-.5));x.push_back(z>0?1.5*z:z);}}
  for(int mask=0;mask<16;++mask) {
   const auto start=std::chrono::steady_clock::now();
   const auto r=cppunivariate::st_mle({x.data(),x.size()},1e-6,200,mask&1?NAN:0,mask&2?NAN:1,mask&4?NAN:2,mask&8?NAN:4);
   std::cout<<n<<' '<<mask<<' '<<int(r.status)<<' '<<r.certified<<' '<<r.xi<<' '<<r.omega<<' '<<r.alpha<<' '<<r.nu<<' '<<r.logLik<<' '<<std::chrono::duration<double,std::milli>(std::chrono::steady_clock::now()-start).count()<<std::endl;
  }
 }
 for(double alpha:std::vector<double>{0.,NAN}) {
  const std::vector<double> x{0,1,2,4};const auto start=std::chrono::steady_clock::now();
  const auto r=cppunivariate::st_mle({x.data(),x.size()},1e-6,200,0,NAN,alpha,4);
  std::cout<<"one-side "<<alpha<<' '<<int(r.status)<<' '<<r.certified<<' '<<r.omega<<' '<<r.logLik<<' '<<std::chrono::duration<double,std::milli>(std::chrono::steady_clock::now()-start).count()<<std::endl;
 }
}
