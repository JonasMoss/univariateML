// Compile once against each include tree, then compare stdout byte for byte.
// Example: c++ -O2 -std=c++17 -I<include-tree> -I<BH-include> this-file -o check
#include <cppunivariate/distributions/std/mle.hpp>
#include <iomanip>
#include <iostream>
#include <limits>
#include <random>
#include <vector>

int main() {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  std::mt19937_64 rng(13320261031ULL);
  std::normal_distribution<double> normal;
  std::student_t_distribution<double> student(4.0);
  std::cout << std::hexfloat;
  for (int sample=0; sample<200; ++sample) {
    const int n=sample%3==0?8:(sample%3==1?16:32);
    std::vector<double> x;
    for (int i=0; i<n; ++i) {
      const double v=sample%2 ? normal(rng) : student(rng);
      x.push_back(v+(sample%5==0 ? 0.3*v*v : 0.0));
    }
    for (unsigned held=1; held<8; ++held) {
      const double mu=held&1?0.0:nan, scale=held&2?1.0:nan;
      const double df=held&4?4.0:nan;
      const cppunivariate::span data{x.data(),x.size()};
      const auto a=cppunivariate::student_t_mle(data,1e-8,200,mu,scale,df);
      const auto b=cppunivariate::lst_mle(data,1e-8,200,mu,scale,df);
      std::cout << sample << ' ' << held << ' '
                << static_cast<int>(a.status) << ' ' << a.mu << ' ' << a.sigma
                << ' ' << a.nu << ' ' << a.logLik << ' ' << a.iterations << '\n'
                << static_cast<int>(b.status) << ' ' << b.mu << ' ' << b.sigma
                << ' ' << b.df << ' ' << b.logLik << ' ' << b.iterations
                << ' ' << b.restricted << '\n';
    }
  }
}
