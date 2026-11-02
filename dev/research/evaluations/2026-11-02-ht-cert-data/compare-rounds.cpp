#include "cppunivariate/core/span.hpp"
#include "cppunivariate/special/special.hpp"
namespace round1_cppunivariate { typedef cppunivariate::span span; namespace special = cppunivariate::special; }
#define cppunivariate round1_cppunivariate
#include "ht-round1/cppunivariate/distributions/ht/mle.hpp"
#undef cppunivariate
#include "cppunivariate/distributions/ht/mle.hpp"
#include <algorithm>
#include <chrono>
#include <cmath>
#include <cstdio>
#include <random>
#include <vector>
int main() {
  std::mt19937_64 rng(1162026);
  const double nan=std::numeric_limits<double>::quiet_NaN();
  int finite=0,limits=0,changed=0; double maxll=0,maxnu=0,maxscale=0;
  double oldtime=0,newtime=0;
  for(int i=0;i<600;++i) {
    const int group=i/200;
    std::student_t_distribution<double> td(group==0?.4:group==1?80.:3.);
    std::vector<double> x(24); for(auto& v:x) v=std::abs(td(rng));
    double mu=0; if(group==2) {x[0]=0; x[1]=0; mu=nan;}
    auto start=std::chrono::steady_clock::now();
    const auto a=round1_cppunivariate::ht_mle(cppunivariate::span{x.data(),x.size()},mu,nan,nan);
    auto middle=std::chrono::steady_clock::now();
    const auto b=cppunivariate::ht_mle(cppunivariate::span{x.data(),x.size()},mu,nan,nan);
    auto end=std::chrono::steady_clock::now();
    oldtime+=std::chrono::duration<double>(middle-start).count();
    newtime+=std::chrono::duration<double>(end-middle).count();
    if(int(a.status)!=int(b.status)) {++changed; std::printf("status sample %d: %d %d\n",i,int(a.status),int(b.status));}
    else if(a.status==round1_cppunivariate::HtStatus::Ok) {
      ++finite; const double dl=std::abs(a.logLik-b.logLik);
      const double dn=std::abs(std::log(a.nu/b.nu)), ds=std::abs(std::log(a.sigma/b.sigma));
      maxll=std::max(maxll,dl); maxnu=std::max(maxnu,dn); maxscale=std::max(maxscale,ds);
      if(dl>1e-8*x.size()||dn>1e-7||ds>1e-7||a.mu!=b.mu) {++changed; std::printf("fit sample %d: %.17g %.17g %.17g\n",i,dl,dn,ds);}
    } else ++limits;
  }
  std::printf("600 cases: finite %d matching limits %d changed %d; max abs logLik %.17g log-nu %.17g log-scale %.17g; seconds round1 %.3f round2 %.3f\n",finite,limits,changed,maxll,maxnu,maxscale,oldtime,newtime);
  return changed?1:0;
}
