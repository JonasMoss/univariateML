#include "json_loader.hpp"
#include "common/distributions/skellam/fit.hpp"
#include "common/distributions/trgamma/fit.hpp"
#include "common/distributions/invtrgamma/fit.hpp"
#include <iostream>
int main(){
 for(const std::string f:{"skellam","trgamma","invtrgamma"}){
  auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/"+f+"/golden.json");
  cppunivariate::span x{g.data.data(),g.data.size()};
  for(int i:{1,2,5,10,20,40,60,100,150,200,300,500,1000}){
   auto r=f=="skellam"?umvl_binding::fit_skellam(x,{},{},1e-9,i):f=="trgamma"?umvl_binding::fit_trgamma(x,{},{},{},1e-10,i):umvl_binding::fit_invtrgamma(x,{},{},{},1e-10,i);
   std::cout<<f<<'\t'<<i<<'\t'<<r.error_kind<<'\t'<<r.error<<'\t'<<r.candidates.size()<<'\t'<<r.loglik_lower<<std::endl;
  }
 }
}
