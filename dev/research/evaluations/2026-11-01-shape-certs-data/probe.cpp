#include "cppunivariate/distributions/ht/mle.hpp"
#include "cppunivariate/distributions/dgamma/mle.hpp"
#include "cppunivariate/distributions/slash/mle.hpp"
#include <random>
#include <iostream>
#include <iomanip>
#include <chrono>
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>
int main() {
  std::mt19937_64 rng(11620261101ULL);
  std::normal_distribution<double> normal;
  std::uniform_real_distribution<double> uniform;
  const double nan=std::numeric_limits<double>::quiet_NaN();
  std::cout<<"family,case,n,generator,status,fit_ll,probe_ll,fit_ms\n"<<std::setprecision(17);
  for(int family=0;family<3;++family) for(int j=0;j<600;++j) {
    const int ns[]={10,30,80}; int n=ns[j%3];
    const double shapes[]={0.5,1.0,3.0,10.0}; double a=shapes[(j/3)%4];
    std::vector<double> x(n);
    std::gamma_distribution<double> gam(a,1.0), chi(a/2,2.0);
    for(double &v:x) {
      if(family==0) v=std::abs(normal(rng)/std::sqrt(chi(rng)/a));
      else if(family==1) v=std::floor(gam(rng));
      else { double u=uniform(rng); v=normal(rng)/std::max(u,1e-15); }
    }
    cppunivariate::span xs{x.data(),x.size()};
    double ll=nan,probe=-std::numeric_limits<double>::infinity(); int status=0;
    auto start=std::chrono::steady_clock::now();
    if(family==0) { auto r=cppunivariate::ht_mle(xs,0,nan,nan); ll=r.logLik; status=(int)r.status; }
    if(family==1) { auto r=cppunivariate::dgamma_mle(xs); ll=r.logLik; status=(int)r.status; }
    if(family==2) { auto r=cppunivariate::slash_mle(xs); ll=r.logLik; status=(int)r.status; }
    auto stop=std::chrono::steady_clock::now();
    if(family==0 && status==0) {
      for(int k=0;k<=160;++k) { double ls=0; double nu=std::exp(-5.0+k*0.075); probe=std::max(probe,cppunivariate::ht_detail::profile(xs,nu,ls,false)); }
    } else if(family==1 && status==0) {
      cppunivariate::dnorm_detail::Cells cells; cells.init(xs);
      cppunivariate::dgamma_detail::Cost cost{cells,false,nan,400};
      for(int k=0;k<=120;++k) { double t=-4.0+k*0.08; double f=cost(std::vector<double>{t}); if(std::isfinite(f)) probe=std::max(probe,-cells.n*f); }
    } else if(family==2 && status==0) {
      std::sort(x.begin(),x.end()); cppunivariate::slash_detail::Profile p{x,nan};
      for(int k=0;k<=128;++k) { double m=x.front()+(x.back()-x.front())*k/128.0; probe=std::max(probe,p(m).logLik); }
    }
    std::cout<<(family==0?"ht":family==1?"dgamma":"slash")<<','<<j<<','<<n<<','<<(family==2?1:a)<<','<<status<<','<<ll<<','<<probe<<','<<std::chrono::duration<double,std::milli>(stop-start).count()<<'\n';
  }
}
