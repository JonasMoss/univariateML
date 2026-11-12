#include "cppunivariate/distributions/cmb/mle.hpp"
#include "cppunivariate/distributions/dgamma/mle.hpp"
#include <iostream>
#include <iomanip>
void detail(const cppunivariate::StopDetail& d) {
  std::cout<<"\"budget_exhausted\":"<<(d.budget_exhausted?"true":"false")<<",\"candidates\":[";
  bool first=true;
  for(const auto& c:d.candidates) {
    if(!first)std::cout<<',';first=false;
    std::cout<<"{\"coef\":[";
    for(std::size_t i=0;i<c.coef.size();++i){if(i)std::cout<<',';std::cout<<c.coef[i];}
    std::cout<<"],\"loglik_lower\":"<<c.loglik_lower<<",\"loglik_upper\":"<<c.loglik_upper<<'}';
  }
  std::cout<<']';
}
int main() {
  std::cout<<std::setprecision(17)<<'[';
  const std::vector<int>x{1000000,1000001,1000003,1000008,1000011};
  const cppunivariate::ispan data{x.data(),x.size()};
  for(int id:{837,915}) {
    const auto r=id==837?cppunivariate::cmb_size_free_nu_mle(data):cppunivariate::cmb_size_mle(data,.2777362750324292,1.683599466457963);
    std::cout<<"{\"index\":"<<id<<",\"status\":"<<int(r.fit.status)<<',';detail(r.fit.stop_detail);std::cout<<"},";
  }
  const std::vector<double>y(x.begin(),x.end());
  const auto r=cppunivariate::dgamma_mle({y.data(),y.size()});
  std::cout<<"{\"index\":997,\"status\":"<<int(r.status)<<",\"large_shape_iterations\":"<<r.large_shape_iterations
    <<",\"rate_score_roundoff\":"<<r.rate_score_roundoff<<",\"failed_shape\":"<<r.failed_shape<<",\"failed_rate\":"<<r.failed_rate<<',';
  detail(r.stop_detail);std::cout<<"}]\n";
}
