#include "cppunivariate/distributions/gengamma/zero_certificate.hpp"
#include <cmath>
#include <iostream>
#include <vector>
int main() {
  std::vector<double> x,y;
  for(int k=-5;k<=5;++k){const double v=std::ldexp(1.,k);x.push_back(v);y.push_back(std::log(v));}
  const auto r=cppunivariate::gengamma_fit_detail::certify_zero({x.data(),x.size()},{y.data(),y.size()},100);
  std::cout << r.certified << ' ' << r.evaluations << '\n';
  return 0;
}
