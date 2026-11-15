#include "cppunivariate/distributions/lgser/zmlogarithmic_mle.hpp"
#include "cppunivariate/distributions/pareto/censored.hpp"
#include "cppunivariate/distributions/weibull/censored.hpp"
#include <iostream>
#include <iomanip>
#include <vector>
int main() {
  std::cout<<std::hexfloat;
  const std::vector<int> x{2147483646,2147483647};
  for(double p0:{0.,.2}) {
    const auto r=cppunivariate::zmlogarithmic_mle({x.data(),x.size()},1e-10,1,NAN,p0==0?NAN:p0);
    std::cout<<"lgser "<<r.prob<<" "<<r.p0<<" "<<r.logLik<<" "<<cppunivariate::outcome(r).certified<<"\n";
  }
  const std::vector<double> l{.5,2},u{INFINITY,2};
  const auto p=cppunivariate::pareto_censored_mle({{l.data(),l.size()},{u.data(),u.size()}},NAN,1);
  std::cout<<"pareto "<<p.shape<<" "<<p.min<<" "<<p.logLik<<" "<<cppunivariate::outcome(p).certified<<"\n";
  const std::vector<double> right{INFINITY,INFINITY};
  for(const std::vector<double>& times:{std::vector<double>{.25,2},std::vector<double>{1.5,2./3}}) {
    const auto r=cppunivariate::weibull_censored_mle({{times.data(),times.size()},{right.data(),right.size()}},NAN,1);
    std::cout<<(times[0]==.25?"weibull":"thin-weibull")<<" "<<r.shape<<" "<<r.scale<<" "<<r.logLik<<" "<<cppunivariate::outcome(r).certified<<"\n";
  }
}
