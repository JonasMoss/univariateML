#include "proto.hpp"
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
  double tl=0,tp=0; int worse=0,better=0,same=0,total=0,lfail=0,pfail=0;
  for(auto& g:gens) for(int s=0;s<seeds;++s){
    std::vector<double> x(n); for(auto& v:x) v=g.second();
    cppunivariate::span sp{x.data(),x.size()};
    double t0=now(); auto L=cppunivariate::tlambda_mle(sp,1e-7,1000); double t1=now();
    auto P=proto::fit(x); double t2=now();
    tl+=t1-t0; tp+=t2-t1; ++total;
    const bool lok=L.status==cppunivariate::TlambdaStatus::Ok;
    const double llP=P.ok?cppunivariate::tlambda_detail::likelihood(sp,P.mu,P.sigma,P.lambda):NAN;
    if(!lok) ++lfail; if(!std::isfinite(llP)) ++pfail;
    const double d=llP-L.logLik;
    if(!lok || !std::isfinite(llP) || !(std::abs(d)<1e-7))
      std::printf("%-8s s=%d legacy st=%d ll=%.10f lam=%.8f | proto ll=%.10f lam=%.8f passes=%ld diff=%.3g\n",g.first,s,(int)L.status,L.logLik,L.lambda,llP,P.lambda,P.passes,d);
    if(lok && std::isfinite(llP)){ if(d< -1e-7)++worse; else if(d>1e-7)++better; else ++same; }
  }
  std::printf("n=%d fits=%d legacy %.2fs proto %.2fs (x%.1f) same=%d better=%d worse=%d legacy-fail=%d proto-fail=%d\n",n,total,tl,tp,tl/tp,same,better,worse,lfail,pfail);
}
