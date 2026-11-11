#include <cppunivariate/special/special.hpp>
#include <cmath>
#include <iomanip>
#include <iostream>
int main() {
  std::cout << std::setprecision(17);
  std::cout << "Q,shape,argument,backend_lower,backend_upper,helper_lower\n";
  for (double q : {1e-2, 1e-4, 1e-6, 1e-8}) {
    const double a = 1.0 / (q*q);
    const double logu = std::log(a) + q;
    const double u = std::exp(logu);
    const auto t = cppunivariate::special::gamma_log_tails(logu, a);
    std::cout << q << ',' << a << ',' << u << ','
              << cppunivariate::special::gamma_p(a,u) << ','
              << cppunivariate::special::gamma_q(a,u) << ','
              << std::exp(t.lower) << '\n';
  }
}
