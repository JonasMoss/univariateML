#include "cppunivariate/distributions/snorm/mle.hpp"
#include "json_loader.hpp"
#include <iomanip>
#include <iostream>
int main(){
 const auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/snorm/golden.json");
 int failed=0;
 std::cout<<std::setprecision(17);
 for(int mask=0;mask<8;++mask){
  const auto r=cppunivariate::snorm_mle({g.data.data(),g.data.size()},1e-9,200,mask&1?g.estimates.at("mean"):NAN,mask&2?g.estimates.at("sd"):NAN,mask&4?g.estimates.at("xi"):NAN);
  if(r.status!=cppunivariate::SnormStatus::Ok ||
     std::abs(r.mu-g.estimates.at("mean"))>1e-10*(1+std::abs(g.estimates.at("mean"))) ||
     std::abs(r.sigma-g.estimates.at("sd"))>1e-10*(1+std::abs(g.estimates.at("sd"))) ||
     std::abs(r.xi-g.estimates.at("xi"))>1e-10*(1+std::abs(g.estimates.at("xi"))) ||
     std::abs(r.logLik-g.logLik)>1e-10*(1+std::abs(g.logLik)))failed=1;
  std::cout<<mask<<" status="<<int(r.status)<<" mean="<<r.mu<<" sd="<<r.sigma<<" xi="<<r.xi<<" logLik="<<r.logLik<<" relative_errors=";
  std::cout<<std::abs(r.mu-g.estimates.at("mean"))/(1+std::abs(g.estimates.at("mean")))<<","<<std::abs(r.sigma-g.estimates.at("sd"))/(1+std::abs(g.estimates.at("sd")))<<","<<std::abs(r.xi-g.estimates.at("xi"))/(1+std::abs(g.estimates.at("xi")))<<" tie="<<r.stop_detail.comparison_tie<<" budget="<<r.stop_detail.budget_exhausted<<"\n";
 }
 return failed;
}
