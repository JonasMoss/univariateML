// Build/run commands are recorded in check.py and the final report.
#include "cppunivariate/distributions/burr/mle.hpp"
#include "cppunivariate/distributions/invburr/mle.hpp"
#include "json_loader.hpp"
#include <chrono>
#include <iomanip>
#include <iostream>
int main(int argc,char** argv) {
  const auto fixture=mltest::load_golden(argv[1]);
  std::vector<std::vector<double>> samples{fixture.data,{.1,.2,.4,.8},{.5,2,3,4,5},
    {.01,.02,.1,1,2,100},{.9,.95,1,1.01},{1e-100,1e-50,1.,1e50,1e100},
    {1,1,2,4},{.5,.5,.5},{2,2,2},{.5,std::nextafter(.5,1.)}};
  std::cout<<std::setprecision(17)<<"[";
  for(std::size_t j=0;j<samples.size();++j) {
    if(j)std::cout<<",";
    const auto& x=samples[j];
    const auto start=std::chrono::steady_clock::now();
    const auto r=cppunivariate::burr_mle_fixed_scale(cppunivariate::span{x.data(),x.size()},1);
    const double us=std::chrono::duration<double,std::micro>(std::chrono::steady_clock::now()-start).count();
    std::cout<<"{\"id\":"<<j<<",\"x\":[";
    for(std::size_t i=0;i<x.size();++i){if(i)std::cout<<",";std::cout<<x[i];}
    std::cout<<"],\"status\":"<<static_cast<int>(r.status)<<",\"microseconds\":"<<us<<",\"evaluations\":"<<r.iterations;
    if(r.status==cppunivariate::BurrStatus::Ok)
      std::cout<<",\"k\":"<<r.shape1<<",\"c\":"<<r.shape2<<",\"logLik\":"<<r.logLik
       <<",\"lower\":"<<r.stop_detail.loglik_lower<<",\"upper\":"<<r.stop_detail.loglik_upper;
    std::cout<<"}";
  }
  std::cout<<"]\n";
}
