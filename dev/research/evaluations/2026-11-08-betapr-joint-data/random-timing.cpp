#define DOCTEST_CONFIG_DISABLE
#include "test_support.hpp"
#ifdef BENCH_BASELINE
#include "baseline-mle.hpp"
#else
#include "cppunivariate/distributions/betapr/mle.hpp"
#endif
#include <iostream>
#include <random>
int main(){
 const auto g=mltest::load_distribution_golden("betapr");
 std::vector<double> large;std::mt19937_64 rng(11646);std::gamma_distribution<double> numerator(2),denominator(3);for(int k=0;k<10000;++k){const double a=numerator(rng),b=denominator(rng);large.push_back(a/b);}
 for(const auto& x:std::vector<std::vector<double>>{g.data,large})for(int round=0;round<3;++round){
  const auto t=std::chrono::steady_clock::now();const auto r=cppunivariate::betapr_mle(mltest::as_span(x));
  std::cout<<std::setprecision(17)<<x.size()<<" "<<round<<" "<<std::chrono::duration<double,std::milli>(std::chrono::steady_clock::now()-t).count()<<" "<<r.alpha<<" "<<r.beta<<" "<<r.scale<<" "<<r.logLik<<" "<<int(r.status);
#ifndef BENCH_BASELINE
  std::cout<<" "<<r.joint_exterior.total_evaluations<<" "<<r.joint_exterior.cover_cells<<" "<<r.joint_exterior.profile_evaluations;
  for(double v:r.joint_exterior.coefficient_lower)std::cout<<" "<<v;
  for(double v:r.joint_exterior.coefficient_upper)std::cout<<" "<<v;
  if(r.status!=cppunivariate::BetaPrimeStatus::Ok||!r.joint_exterior.global_certified)return 2;
#endif
  std::cout<<"\n";
 }
}
