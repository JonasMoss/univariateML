// Direct microbenchmark of bhatt location()'s inner per-point loop body,
// old vs new (hoisted log2b/logs/s2), on identical synthetic inputs, with a
// fixed repeat count -- isolates the hoist's effect from Nelder-Mead noise.
#include "cppunivariate/distributions/bhatt/dpqr.hpp"
#include <chrono>
#include <cstdio>
#include <vector>
#include <random>

using namespace cppunivariate;
using namespace cppunivariate::bhatt_detail;

double old_pass(const std::vector<double>& x, double m, double s, double b) {
  long double score=0,deriv=0;
  for(double z:x) {
    const long double u=(static_cast<long double>(z)-m-b)/s,v=(static_cast<long double>(z)-m+b)/s;
    const long double lu=logphi(u),lv=logphi(v);
    const long double ld=distributions::dbhatt(z,m,s,b,true)+std::log(2*static_cast<long double>(b));
    const long double A=std::exp(lu-ld),B=std::exp(lv-ld);
    long double g=0;
    if(lu!=lv) {
      const long double ln=std::max(lu,lv)+std::log(-std::expm1(-std::abs(lu-lv)));
      g=(lu>lv?1:-1)*std::exp(ln-ld-std::log(static_cast<long double>(s)));
    }
    score+=g;deriv+=(u*A-v*B)/(static_cast<long double>(s)*s)-g*g;
  }
  return static_cast<double>(score)+static_cast<double>(deriv);
}

double new_pass(const std::vector<double>& x, double m, double s, double b) {
  const long double log2b=std::log(2*static_cast<long double>(b));
  const long double logs=std::log(static_cast<long double>(s));
  const long double s2=static_cast<long double>(s)*s;
  long double score=0,deriv=0;
  for(double z:x) {
    const long double u=(static_cast<long double>(z)-m-b)/s,v=(static_cast<long double>(z)-m+b)/s;
    const long double lu=logphi(u),lv=logphi(v);
    const long double ld=distributions::dbhatt(z,m,s,b,true)+log2b;
    const long double A=std::exp(lu-ld),B=std::exp(lv-ld);
    long double g=0;
    if(lu!=lv) {
      const long double ln=std::max(lu,lv)+std::log(-std::expm1(-std::abs(lu-lv)));
      g=(lu>lv?1:-1)*std::exp(ln-ld-logs);
    }
    score+=g;deriv+=(u*A-v*B)/s2-g*g;
  }
  return static_cast<double>(score)+static_cast<double>(deriv);
}

int main() {
  const int n = 1000;
  std::mt19937_64 rng(42);
  std::normal_distribution<> nd(0,1);
  std::vector<double> x(n);
  for (auto &v : x) v = nd(rng);
  const int reps = 2000;
  double sink = 0;

  auto t0 = std::chrono::steady_clock::now();
  for (int r = 0; r < reps; ++r) sink += old_pass(x, 0.01*r, 1.0, 2.0);
  auto t1 = std::chrono::steady_clock::now();
  for (int r = 0; r < reps; ++r) sink += new_pass(x, 0.01*r, 1.0, 2.0);
  auto t2 = std::chrono::steady_clock::now();

  double t_old = std::chrono::duration<double>(t1-t0).count();
  double t_new = std::chrono::duration<double>(t2-t1).count();
  std::printf("old: %.4fs (%.1f ns/point-pass)\n", t_old, t_old/(reps*(double)n)*1e9);
  std::printf("new: %.4fs (%.1f ns/point-pass)\n", t_new, t_new/(reps*(double)n)*1e9);
  std::printf("speedup: %.3fx  sink=%.6g\n", t_old/t_new, sink);
  return 0;
}
