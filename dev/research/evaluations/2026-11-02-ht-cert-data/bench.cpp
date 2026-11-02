#include "cppunivariate/core/span.hpp"
#include "cppunivariate/special/special.hpp"
namespace legacy_cppunivariate { typedef cppunivariate::span span; }
#define cppunivariate legacy_cppunivariate
#include "ht-old.hpp"
#undef cppunivariate
#include "cppunivariate/distributions/ht/mle.hpp"
#include <chrono>
#include <cstdio>
#include <random>
#include <vector>
int main() {
  std::mt19937_64 rng(116500); std::student_t_distribution<double> td(3.0);
  std::vector<double> x(500); for(auto &v:x) v=std::abs(td(rng));
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(int i=0;i<5;++i) {
    const auto s=std::chrono::steady_clock::now();
    const auto a=legacy_cppunivariate::ht_mle(cppunivariate::span{x.data(),x.size()},0,nan,nan);
    const auto m=std::chrono::steady_clock::now();
    const auto b=cppunivariate::ht_mle(cppunivariate::span{x.data(),x.size()},0,nan,nan);
    const auto e=std::chrono::steady_clock::now();
    std::printf("%d %.9g %.9g %d %d %d %.17g\n",i,std::chrono::duration<double>(m-s).count(),std::chrono::duration<double>(e-m).count(),int(a.status),int(b.status),b.iterations,b.logLik-a.logLik);
    std::fflush(stdout);
  }
}
