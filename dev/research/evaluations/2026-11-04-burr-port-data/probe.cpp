// Optimized public-fit and compatibility-path comparison; no timing in tests.
#include "cppunivariate/distributions/burr/mle.hpp"
#include <chrono>
#include <iomanip>
#include <iostream>
#include <vector>
int main(){
  std::size_t n;
  std::cout<<std::setprecision(17);
  while(std::cin>>n){
    std::vector<double> x(n);for(double& v:x)std::cin>>v;
    const cppunivariate::span input{x.data(),x.size()};
    cppunivariate::BurrMLE fit{},old{};
    std::vector<double> timings,legacy;
    for(int rep=0;rep<11;++rep){
      auto start=std::chrono::steady_clock::now();
      fit=cppunivariate::burr_mle(input,1e-8,1000);
      double elapsed=std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count();
      if(rep)timings.push_back(elapsed);
      start=std::chrono::steady_clock::now();
      old=cppunivariate::burr_mle_legacy(input,1e-8,1000);
      elapsed=std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count();
      if(rep)legacy.push_back(elapsed);
    }
    std::sort(timings.begin(),timings.end());std::sort(legacy.begin(),legacy.end());
    std::cout<<static_cast<int>(fit.status)<<' '<<fit.shape1<<' '<<fit.shape2<<' '<<fit.scale<<' '<<fit.logLik<<' '<<fit.iterations<<' '
      <<(timings[4]+timings[5])/2<<' '<<(legacy[4]+legacy[5])/2<<' '<<static_cast<int>(old.status)<<' '<<old.logLik;
    cppunivariate::burr_cert_detail::Data data;
    cppunivariate::burr_cert_detail::Weibull face;
    if(!cppunivariate::burr_cert_detail::data(input,data)||!cppunivariate::burr_cert_detail::weibull(data,face))return 1;
    std::cout<<' '<<face.A.lo<<' '<<face.A.hi<<'\n';
  }
}
