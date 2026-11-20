// Run from the repository root with the installed Boost.Math headers:
// c++ -std=c++17 -O2 -IunivariateML-cpp/include -I<Boost include> tight-controls.cpp
#include "cppunivariate/special/special.hpp"
#include "cppunivariate/distributions/dweibull/mle.hpp"
#include "cppunivariate/distributions/gpd/mle.hpp"
#include "cppunivariate/distributions/huber/mle.hpp"
#include "cppunivariate/distributions/snorm/mle.hpp"
#include "cppunivariate/distributions/triang/mle.hpp"
#include <array>
#include <iostream>
#include <string_view>

template<class Result>
bool check(std::string_view family, const Result& result, double tol) {
  const auto fit = cppunivariate::outcome(result);
  const auto& detail = result.stop_detail;
  std::cout << family << ' ' << cppunivariate::outcome_kind_name(fit.kind) << ' '
            << cppunivariate::outcome_reason_name(fit.reason) << ' '
            << detail.attainable_tol << ' ' << detail.candidates.size() << '\n';
  if (tol == 1e-10) return fit.kind == cppunivariate::OutcomeKind::Estimate;
  return fit.reason == cppunivariate::OutcomeReason::Tolerance &&
         detail.attainable_tol > tol && !detail.candidates.empty();
}

int main() {
  const std::array<double, 9> counts{{0, 0, 1, 1, 1, 2, 4, 5, 8}};
  const std::array<double, 10> positive{{.02, .1, .15, .22, .4, .6, 1.2, 1.8, 2.8, 5.7}};
  const std::array<double, 10> asym{{-3, -1, -.5, 0, .1, .2, .4, 1, 2, 4}};
  const cppunivariate::span count_data{counts.data(), counts.size()};
  const cppunivariate::span positive_data{positive.data(), positive.size()};
  const cppunivariate::span asymmetric_data{asym.data(), asym.size()};

  bool pass = true;
  std::cout << "Family kind reason attainable_tol candidates\n";
  for (double tol : {1e-10, 1e-24}) {
    std::cout << "tol " << tol << '\n';
    pass &= check("dweibull", cppunivariate::dweibull_mle(count_data, tol), tol);
    pass &= check("gpd", cppunivariate::gpd_mle(positive_data, tol), tol);
    pass &= check("huber", cppunivariate::huber_mle(asymmetric_data, tol), tol);
    pass &= check("snorm", cppunivariate::snorm_mle(asymmetric_data, tol), tol);
    pass &= check("triang", cppunivariate::triang_mle(positive_data, tol), tol);
  }
  return pass ? 0 : 1;
}
