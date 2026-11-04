// Microbenchmark the dominant fixed-k work; this is not a complexity proof.
// Compiles without Boost or R, and with -fno-exceptions.
#include "cppunivariate/distributions/burr/certificate.hpp"
#include <chrono>
#include <iomanip>
#include <iostream>
int main(){
  std::size_t n;std::cin>>n;std::vector<double>x(n);for(double& v:x)std::cin>>v;
  cppunivariate::burr_cert_detail::Data d;
  if(!cppunivariate::burr_cert_detail::data(cppunivariate::span{x.data(),x.size()},d))return 1;
  double profile_seconds=0,dual_seconds=0;double checksum=0;
  for(int rep=0;rep<10;++rep){
    auto start=std::chrono::steady_clock::now();
    for(int i=0;i<=40;++i){const double k=.002004008016032064*std::exp(std::log(4990000.)*i/40.);
      cppunivariate::burr_cert_detail::Proposal p;
      if(!cppunivariate::burr_cert_detail::profile(d.y,k,p))return 2;
      checksum+=p.value;
    }
    profile_seconds+=std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count();
    start=std::chrono::steady_clock::now();
    for(int i=0;i<=40;++i){const double k=.002004008016032064*std::exp(std::log(4990000.)*i/40.);
      cppunivariate::burr_cert_detail::Node p;
      if(!cppunivariate::burr_cert_detail::dual(d,k,p))return 3;
      checksum+=p.upper;
    }
    dual_seconds+=std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count();
  }
  std::cout<<std::setprecision(17)<<"{\"profiles_per_batch\":41,\"repetitions\":10,\"proposal_batch_seconds\":"<<profile_seconds/10
    <<",\"proposal_and_enclosure_batch_seconds\":"<<dual_seconds/10<<",\"checksum\":"<<checksum<<"}\n";
}
