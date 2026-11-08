#include "cppunivariate/distributions/bbinom/mle.hpp"
#include <cmath>
#include <iomanip>
#include <iostream>
#include <vector>
int main() {
  std::cout<<std::setprecision(17);
  for(int N:{2,3}) {
    std::vector<int>x;
    const std::vector<int> frequencies=N==2?std::vector<int>{25001,50000,25001}:std::vector<int>{12501,37499,37499,12501};
    for(int v=0;v<=N;++v)x.insert(x.end(),frequencies[v],v);
    const auto r=cppunivariate::bbinom_mle({x.data(),x.size()},N);
    const double exact=N==2?25000:18749.5;
    std::cout<<N<<" "<<int(r.status)<<" "<<r.alpha<<" "<<r.beta<<" "
      <<std::abs(r.alpha/exact-1)<<" "<<std::abs(r.beta/exact-1)<<"\n";
  }
  std::vector<int> near;
  const std::vector<int> frequencies{1000,3001,2999,1000};
  for(int v=0;v<4;++v)near.insert(near.end(),frequencies[v],v);
  const auto face=cppunivariate::bbinom_mle({near.data(),near.size()},3);
  std::cout<<"near-zero "<<int(face.status)<<"\n";
  if(face.status!=cppunivariate::BbinomStatus::BinomialLimit)return 1;
  for(int& value:near)value=3-value;
  const auto reflected=cppunivariate::bbinom_mle({near.data(),near.size()},3);
  std::cout<<"reflected-near-zero "<<int(reflected.status)<<"\n";
  if(reflected.status!=cppunivariate::BbinomStatus::BinomialLimit)return 1;

  const std::vector<int>x{0,1,1,2,2,3,4,5,7,10};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  cppunivariate::bbinom_detail::CellWork reference(1000000000);
  const auto fitted=cppunivariate::bbinom_mle_work({x.data(),x.size()},10,1e-10,100,nan,nan,reference);
  cppunivariate::bbinom_detail::CellWork limited(reference.used-1);
  const auto stopped=cppunivariate::bbinom_mle_work({x.data(),x.size()},10,1e-10,100,nan,nan,limited);
  std::cout<<"budget "<<reference.used<<" "<<int(stopped.status)<<" "<<stopped.stop_detail.candidates.size()<<"\n";
  return fitted.status!=cppunivariate::BbinomStatus::Ok ||
    stopped.status!=cppunivariate::BbinomStatus::SizeBudget ||
    !stopped.stop_detail.budget_exhausted || stopped.stop_detail.candidates.size()<2;

}
