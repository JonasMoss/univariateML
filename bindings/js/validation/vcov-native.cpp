#include "cppunivariate/core/inference_families.hpp"
#include <iostream>
#include <iomanip>
int main() {
  std::string family; unsigned count, mask, n;
  while(std::cin >> family >> count >> mask >> n) {
    std::array<double,cppunivariate::inference_max_parameters> p{};
    for(unsigned i=0;i<count;++i) std::cin >> p[i];
    std::vector<double> x(n); for(auto& z:x) std::cin >> z;
    auto r=cppunivariate::family_inference(cppunivariate::span{x.data(),x.size()},family,p,mask);
    for(auto kind:{cppunivariate::CovarianceKind::Sandwich,cppunivariate::CovarianceKind::Observed}) {
      auto c=cppunivariate::inference_covariance(r,kind);
      std::cout << static_cast<int>(c.status) << ' ' << c.dimension;
      for(std::size_t i=0;i<c.dimension*c.dimension;++i) std::cout << ' ' << std::setprecision(17) << c.matrix[i];
      std::cout << '\n';
    }
  }
}
