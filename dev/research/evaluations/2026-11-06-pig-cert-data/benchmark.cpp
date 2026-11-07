#include "cppunivariate/distributions/pig/mle.hpp"
#include <chrono>
#include <iostream>
#include <iomanip>
#include <vector>
#include <utility>
#include <string>
int main() {
#ifdef PIG_GOLDEN
#include "pig-golden-data.hpp"
#else
 std::vector<int> x{0,0,0,0,1,2,3,5,10};
#endif
 const double nan=std::numeric_limits<double>::quiet_NaN();
 const std::vector<std::pair<double,double>> holds{{nan,nan},{nan,3.},{nan,1e-20},{nan,1e20},{3.,nan},{3.,3.}};
 const std::vector<std::string> labels{"free","shape=3","shape=1e-20","shape=1e20","mean=3","mean=3;shape=3"};
 std::cout<<"case,mean,shape,logLik,status,microseconds_per_fit\n";
 for (std::size_t j=0;j<holds.size();++j) {
  const auto start=std::chrono::steady_clock::now();
  cppunivariate::PigResult r{};
  for(int i=0;i<1000;++i)r=cppunivariate::pig_mle({x.data(),x.size()},1e-9,200,holds[j].first,holds[j].second);
  std::cout<<labels[j]<<","<<std::setprecision(17)<<r.mean<<","<<r.shape<<","<<r.logLik<<","<<int(r.status)<<","<<std::chrono::duration<double,std::micro>(std::chrono::steady_clock::now()-start).count()/1000<<"\n";
 }

}
