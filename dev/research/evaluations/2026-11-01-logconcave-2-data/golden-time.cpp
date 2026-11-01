#include <cppunivariate/cppunivariate.hpp>
#include "../../../../univariateML-cpp/tests/json_loader.hpp"
#include <chrono>
#include <iostream>
#include <string>
int main(int argc,char**argv) {
  if(argc!=2)return 2;
  const std::string family=argv[1];
  const auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/"+family+"/golden.json");
  const cppunivariate::span x{g.data.data(),g.data.size()};
  const auto begin=std::chrono::steady_clock::now();
  int status=-1;double ll=0;
  if(family=="sn"){const auto r=cppunivariate::sn_mle(x,1e-8,1000);status=int(r.status);ll=r.logLik;}
  if(family=="ged"){const auto r=cppunivariate::ged_mle(x,1e-8,1000);status=int(r.status);ll=r.logLik;}
  if(family=="sged"){const auto r=cppunivariate::sged_mle(x,1e-8,1000);status=int(r.status);ll=r.logLik;}
  std::cout<<family<<","<<g.data.size()<<","<<status<<","<<ll<<","<<std::chrono::duration<double>(std::chrono::steady_clock::now()-begin).count()<<"\n";
}
