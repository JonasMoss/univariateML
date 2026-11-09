#ifdef BEFORE_G1
#include "g1-before/frechet/mle.hpp"
#else
#include "cppunivariate/distributions/frechet/mle.hpp"
#endif
#include "evt-joint-golden.hpp"
#include <chrono>
#include <iostream>
#include <iomanip>
#include <random>
int main() {
  std::mt19937_64 engine(11651);
  std::vector<double> gx,fx;
  for(int i=0;i<10000;++i) {
    const double u=(static_cast<double>(engine()>>11)+.5)/9007199254740992.;
    gx.push_back(std::expm1(-.3*std::log(-std::log(u)))/.3);
    fx.push_back(std::exp(-std::log(-std::log(u))/1.5));
  }
  std::cout<<std::setprecision(17);
  for(int job=0;job<4;++job) {
    const auto& x=job==0?gev_golden:job==1?frechet_golden:job==2?gx:fx;
    const int repeats=job<2?3:1;
    double a=0,b=0,c=0,ll=0;int status=0,work=0;bool budget=false;
    const auto start=std::chrono::steady_clock::now();
    for(int j=0;j<repeats;++j) {
      if(job%2==0) {
        const auto f=cppunivariate::gev_mle({x.data(),x.size()});
        a=f.mu;b=f.sigma;c=f.xi;ll=f.logLik;status=int(f.status);work=f.iterations;budget=f.stop_detail.budget_exhausted;
      } else {
        const auto f=cppunivariate::frechet_mle({x.data(),x.size()});
        a=f.lambda;b=f.mu;c=f.sigma;ll=f.logLik;status=int(f.status);work=f.iterations;budget=f.stop_detail.budget_exhausted;
      }
    }
    std::cout<<"{\"case\":\""<<(job%2==0?"gev":"frechet")<<"\",\"n\":"<<x.size()<<",\"iterlim\":100,\"status\":"<<status
        <<",\"coef\":["<<a<<","<<b<<","<<c<<"],\"logLik\":"<<ll<<",\"evaluations_or_iterations\":"<<work
        <<",\"budget\":"<<(budget?"true":"false")<<",\"microseconds\":"
        <<std::chrono::duration<double,std::micro>(std::chrono::steady_clock::now()-start).count()/repeats<<"}\n"<<std::flush;
  }
}
