// Compile/run from the worktree root; see run.sh. Seven alternating pairs,
// after one warmup; input is the unchanged family golden data.
#include "cppunivariate/distributions/invburr/mle.hpp"
#include "cppunivariate/distributions/pareto4/mle.hpp"
#include <chrono>
#include <iostream>
#include <iomanip>
#include <string>
namespace cppunivariate {
#include "legacy-invburr.inc"
}
int main() {
  std::string name;std::size_t n;
  std::cout<<std::setprecision(17);
  while(std::cin>>name>>n) {
    std::vector<double> x(n);for(double& v:x)std::cin>>v;
    std::vector<double> before,after;
    for(int rep=-1;rep<7;++rep) for(int mode=0;mode<2;++mode) {
      const auto start=std::chrono::steady_clock::now();
      double ll=0;
      if(name=="invburr") {
        const auto r=mode?cppunivariate::invburr_mle(cppunivariate::span{x.data(),n},1e-8,1000):cppunivariate::invburr_before(cppunivariate::span{x.data(),n},1e-8,1000);
        if(r.status!=cppunivariate::InvBurrStatus::Ok)return 1;
        ll=r.logLik;
      } else {
        const auto r=mode?cppunivariate::pareto4_mle_certified(cppunivariate::span{x.data(),n},0,1e-10,1000):cppunivariate::pareto4_mle(cppunivariate::span{x.data(),n},0,1e-10,1000);
        if(r.status!=cppunivariate::BurrStatus::Ok)return 2;
        ll=r.logLik;
      }
      const double ms=std::chrono::duration<double,std::milli>(std::chrono::steady_clock::now()-start).count();
      if(rep>=0){(mode?after:before).push_back(ms);std::cout<<name<<' '<<rep<<' '<<mode<<' '<<ms<<' '<<ll<<'\n';}
    }
    std::sort(before.begin(),before.end());std::sort(after.begin(),after.end());
    std::cout<<name<<" median "<<before[3]<<' '<<after[3]<<' '<<after[3]/before[3]<<'\n';
  }
}
