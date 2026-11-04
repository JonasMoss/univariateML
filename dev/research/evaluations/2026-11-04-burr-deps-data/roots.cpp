#include "cppunivariate/distributions/invburr/mle.hpp"
#include "cppunivariate/distributions/pareto4/mle.hpp"
#include "json_loader.hpp"
#include <iomanip>
#include <iostream>
int main(){
  std::cout<<std::setprecision(17);
  for(const std::string name:{"invburr","pareto4"}) {
    const auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/"+name+"/golden.json");
    if(name=="invburr") {
      const auto r=cppunivariate::invburr_mle(cppunivariate::span{g.data.data(),g.data.size()},1e-8,1000);
      if(r.status!=cppunivariate::InvBurrStatus::Ok)return 1;
      std::cout<<name<<' '<<r.shape1<<' '<<r.shape2<<' '<<r.scale<<' '<<r.logLik<<'\n';
    } else {
      const auto r=cppunivariate::pareto4_mle_certified(cppunivariate::span{g.data.data(),g.data.size()},g.estimates.at("min"),1e-10,1000);
      if(r.status!=cppunivariate::BurrStatus::Ok)return 2;
      std::cout<<name<<' '<<r.shape1<<' '<<r.shape2<<' '<<r.scale<<' '<<r.logLik<<'\n';
    }
  }
}
