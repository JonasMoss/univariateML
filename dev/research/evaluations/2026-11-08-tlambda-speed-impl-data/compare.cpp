#include "cppunivariate/distributions/tlambda/mle.hpp"
#include <iostream>
#include <iomanip>
#include <chrono>
#include <cstdio>
#include <random>
#include <functional>
static double now(){return std::chrono::duration<double>(std::chrono::steady_clock::now().time_since_epoch()).count();}
int main(int argc,char**argv){
  setvbuf(stdout,nullptr,_IONBF,0);
  const int n=std::atoi(argv[1]); const int seeds=std::atoi(argv[2]);
  std::mt19937_64 rng(777+n);
  std::normal_distribution<double> N01; std::cauchy_distribution<double> C01; std::student_t_distribution<double> T3(3), T1_5(1.5);
  std::uniform_real_distribution<double> U(0,1); std::exponential_distribution<double> E1(1);
  std::vector<std::pair<const char*,std::function<double()>>> gens{
    {"normal",[&]{return N01(rng);}},{"t3",[&]{return T3(rng);}},{"t1.5",[&]{return T1_5(rng);}},{"cauchy",[&]{return C01(rng);}},
    {"uniform",[&]{return U(rng);}},{"exp",[&]{return E1(rng);}},{"contam",[&]{return U(rng)<.1?10*N01(rng):N01(rng);}},
    {"bimodal",[&]{return (U(rng)<.5?-3:3)+N01(rng);}},{"tl-0.8",[&]{return cppunivariate::distributions::qtlambda(U(rng),0,1,-0.8);}},
    {"tl0.9",[&]{return cppunivariate::distributions::qtlambda(U(rng),0,1,0.9);}},{"rounded",[&]{return std::round(4*N01(rng))/4;}}};
  std::cout<<std::setprecision(17);
  for(auto& g:gens) for(int seed=0;seed<seeds;++seed){
    std::vector<double> x(n); for(auto& v:x)v=g.second();
    for(int route=0;route<4;++route){
      auto t=now();
      auto f=cppunivariate::tlambda_mle({x.data(),x.size()},1e-10,2000,route==3?-.5:NAN,route==1?.123:NAN,route==2?1.07:NAN);
      std::cout<<g.first<<' '<<seed<<' '<<route<<' '<<int(f.status)<<' '<<f.mu<<' '<<f.sigma<<' '<<f.lambda<<' '<<f.logLik<<' '<<now()-t<<'\n'<<std::flush;
    }
  }
}
