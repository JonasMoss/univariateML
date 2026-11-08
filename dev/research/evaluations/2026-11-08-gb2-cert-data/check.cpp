#include "cppunivariate/distributions/gb2/mle.hpp"
#include "cppunivariate/distributions/fpareto/mle.hpp"
#include <chrono>
#include <iostream>
#include <iomanip>
int main() {
 std::cout<<std::setprecision(17);
 const std::vector<double> base{.03,.1,.3,.7,1,2,5,20};
 for(double a:{.7,1.,2.})for(bool reciprocal:{false,true}) {
  std::vector<double>x;for(double v:base)x.push_back(std::pow(v,reciprocal?-1/a:1/a));
  auto start=std::chrono::steady_clock::now();cppunivariate::Gb2Result r{};
  for(int j=0;j<30;++j)r=cppunivariate::gb2_mle({x.data(),x.size()},1e-7,1000,reciprocal?2:NAN,a,reciprocal?NAN:2,NAN);
  auto end=std::chrono::steady_clock::now();
  std::cout<<a<<" "<<reciprocal<<" "<<int(r.status)<<" "<<r.shape1<<" "<<r.shape2<<" "<<r.shape3<<" "<<r.scale<<" "<<r.logLik<<" "<<std::chrono::duration<double,std::milli>(end-start).count()/30<<"\n";
 }

 std::vector<double> large;
 for(int j=0;j<12500;++j)for(double v:base)large.push_back(std::sqrt(v));
 auto start=std::chrono::steady_clock::now();
 const auto r=cppunivariate::gb2_mle({large.data(),large.size()},1e-7,1000,NAN,2,2,NAN);
 auto end=std::chrono::steady_clock::now();
 std::cout<<"large "<<large.size()<<" "<<int(r.status)<<" "<<r.shape1<<" "<<r.shape2<<" "<<r.shape3<<" "<<r.scale<<" "<<r.logLik<<" "<<std::chrono::duration<double,std::milli>(end-start).count()<<"\n";

 for(bool big:{false,true}) {
  std::vector<double>x;
  for(int j=0;j<(big?12500:1);++j)for(double v:base)x.push_back(2+std::sqrt(v));
  auto start=std::chrono::steady_clock::now();cppunivariate::FparetoResult r{};
  for(int j=0;j<(big?1:30);++j)r=cppunivariate::fpareto_mle({x.data(),x.size()},2,1e-7,1000,NAN,2,2,NAN);
  auto end=std::chrono::steady_clock::now();
  std::cout<<"shifted "<<x.size()<<" "<<int(r.status)<<" "<<r.shape1<<" "<<r.shape2<<" "<<r.shape3<<" "<<r.scale<<" "<<r.logLik<<" "<<std::chrono::duration<double,std::milli>(end-start).count()/(big?1:30)<<"\n";
 }
}
