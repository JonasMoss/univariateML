#include "cppunivariate/distributions/tbinom/face_certificate.hpp"
#include <iostream>
#include <vector>
int main() {
  std::vector<int>x;
  const int counts[]{242187,60672,121344,1488980,406274};
  for(int k=0;k<5;++k)x.insert(x.end(),counts[k],k);
  cppunivariate::binomial_certificate_detail::Work work{0,0,512};
  bool budget=false;double rate=NAN,ll=NAN;
  const double cutoff=cppunivariate::tbinom_detail::second_order_poisson_cutoff({x.data(),x.size()},4,work,budget,rate,ll);
  std::cout << "n=" << x.size() << " cutoff=" << cutoff << " budget=" << budget << " numerical=" << work.numerical << '\n';
  return std::isfinite(cutoff)||budget||work.numerical;
}
