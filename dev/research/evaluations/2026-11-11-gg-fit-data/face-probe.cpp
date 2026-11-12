#include "cppunivariate/distributions/gengamma/face_certificate.hpp"
#include <cmath>
#include <iostream>
#include <vector>
int main() {
  const std::vector<double> y{0.,std::log(2.),std::log(3.)};
  const std::vector<double> inverse{0.,-std::log(2.),-std::log(3.)};
  const std::vector<double> tie{-1.,1.};
  for (const auto& v:{y,inverse,tie}) {
    const auto r=cppunivariate::gengamma_fit_detail::strict_winning_face({v.data(),v.size()},100);
    std::cout << int(r.winner) << ' ' << r.evaluations << '\n';
  }
  return 0;
}
