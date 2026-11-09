#define CPPUNIV_SIZE_SEARCH_DIAGNOSTICS
#include "cppunivariate/distributions/tbinom/mle.hpp"
#include "cppunivariate/distributions/zib/mle.hpp"
#include <chrono>
#include <iomanip>
#include <iostream>
int main() {
  std::cout<<std::setprecision(17);
  for(const auto& count : {std::vector<std::uint64_t>{242187,60672,121344,1488980,406274},std::vector<std::uint64_t>{21251,11790,0,244158,168771}}) {
    std::vector<int>x;for(int k=0;k<5;++k)x.insert(x.end(),count[k],k);
    const auto start=std::chrono::steady_clock::now();
    const auto f=cppunivariate::tbinom_design_mle({x.data(),x.size()},-1,4,NAN);
    const auto diagnostic=cppunivariate::tbinom_detail::size_search_diagnostic;
    std::cout<<"n "<<x.size()<<" status "<<int(f.status)<<" size "<<f.size<<" prob "<<f.prob<<" logLik "<<f.logLik
      <<" upper "<<f.stop_detail.loglik_upper<<" comparison "<<f.stop_detail.comparison_tie<<" cutoff "<<diagnostic.cutoff<<" cells "<<diagnostic.work<<" budget "<<f.stop_detail.budget_exhausted
      <<" ms "<<std::chrono::duration<double,std::milli>(std::chrono::steady_clock::now()-start).count()<<"\n";
  }
  const std::vector<int>x{0,0,0,0,0,1,1,2,2,4};
  for(double pi : {.3772146069463776,.37721460694637765}) {
    const auto start=std::chrono::steady_clock::now();
    const auto f=cppunivariate::zib_mle({x.data(),x.size()},1e-10,200,NAN,NAN,pi);
    std::cout<<"pi "<<pi<<" status "<<int(f.status)<<" upper "<<f.stop_detail.loglik_upper<<" comparison "<<f.stop_detail.comparison_tie
      <<" ms "<<std::chrono::duration<double,std::milli>(std::chrono::steady_clock::now()-start).count()<<"\n";
  }
}
