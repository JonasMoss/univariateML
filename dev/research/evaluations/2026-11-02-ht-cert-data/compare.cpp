#include "cppunivariate/core/span.hpp"
#include "cppunivariate/special/special.hpp"
namespace legacy_cppunivariate { typedef cppunivariate::span span; }
#define cppunivariate legacy_cppunivariate
#include "ht-old.hpp"
#undef cppunivariate
#include "cppunivariate/distributions/ht/mle.hpp"
#include <chrono>
#include <cmath>
#include <cstdio>
#include <random>
#include <vector>
int main() {
  std::mt19937_64 rng(1162026);
  const double nan=std::numeric_limits<double>::quiet_NaN();
  int both=0, limits=0, changes=0, failures=0;
  double worst=0, t_old=0, t_new=0;
  for(int i=0;i<600;++i) {
    const int group=i/200;
    std::student_t_distribution<double> td(group==0?.4:group==1?80.:3.);
    std::vector<double> x(24);
    for(auto& v:x) v=std::abs(td(rng));
    double mu=0;
    if(group==2) {x[0]=0; x[1]=0; mu=nan;}
    auto s=std::chrono::steady_clock::now();
    const auto a=legacy_cppunivariate::ht_mle(cppunivariate::span{x.data(),x.size()},mu,nan,nan);
    auto m=std::chrono::steady_clock::now();
    const auto b=cppunivariate::ht_mle(cppunivariate::span{x.data(),x.size()},mu,nan,nan);
    auto e=std::chrono::steady_clock::now();
    t_old+=std::chrono::duration<double>(m-s).count(); t_new+=std::chrono::duration<double>(e-m).count();
    if(b.status==cppunivariate::HtStatus::IterationFailure || b.status==cppunivariate::HtStatus::SearchFailure) ++failures;
    if(a.status==legacy_cppunivariate::HtStatus::Ok && b.status==cppunivariate::HtStatus::Ok) {
      ++both; worst=std::max(worst,a.logLik-b.logLik);
      if(b.logLik+1e-8*x.size()<a.logLik) {++changes; std::printf("loss %d %.12g\n",i,b.logLik-a.logLik);}
    } else if(static_cast<int>(a.status)==static_cast<int>(b.status)) ++limits;
    else {++changes; std::printf("status %d old %d new %d\n",i,int(a.status),int(b.status));}
  }
  std::printf("seed 1162026: both %d same-limit %d status-or-loss %d failures %d max-loss %.12g old %.3fs new %.3fs\n",both,limits,changes,failures,worst,t_old,t_new);
  return failures||changes ? 1:0;
}
