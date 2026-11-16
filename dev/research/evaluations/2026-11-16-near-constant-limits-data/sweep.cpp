#include "cppunivariate/distributions/betapr/mle.hpp"
#include "cppunivariate/distributions/genbeta/mle.hpp"
#include <cstdio>
#include <random>
using namespace cppunivariate;
int main(){
  std::mt19937_64 g(7);
  std::uniform_real_distribution<double> U(0,1);
  double bases[]={0.612107714198942,1.7,0.3};
  int ks[]={0,0,-1};
  for(int trial=0;trial<400;++trial){
    int n=3+g()%4; double base=bases[g()%3]; double rho=std::pow(10.0,-16+10*U(g)); if(g()%3==0)rho=2.3e-16*(1+g()%4);
    std::vector<double> x(n); for(auto&v:x)v=base*(1+rho*(U(g)<0.5?0:U(g)<0.8?1:U(g)));
    // genbeta needs x<1
    std::vector<double> xb=x; std::vector<double> xg; for(double v:x)xg.push_back(v>=1?v/2:v);
    // betapr
    {auto old=betapr_detail::accurate_fallback(betapr_joint_cert::fit(span{xb.data(),xb.size()},1e-6,2000,0,0,0,1e-13,1e-10),span{xb.data(),xb.size()},NAN,NAN,NAN,1e-6,2000,0,0,0);
     auto nw=betapr_mle(span{xb.data(),xb.size()});
     printf("B n=%d base=%g rho=%.3g old=%d new=%d oldll=%.15g newll=%.15g model=%d x=",n,base,rho,(int)old.status,(int)nw.status,old.logLik,nw.logLik,(int)nw.stop_detail.limit_model);
     for(double v:xb)printf("%a,",v);printf("\n");}
    {auto old=genbeta_accuracy_search(span{xg.data(),xg.size()},1.0,1e-7,300);
     auto nw=genbeta_mle(span{xg.data(),xg.size()},1.0);
     printf("G n=%d base=%g rho=%.3g old=%d new=%d newll=%.15g model=%d x=",n,base,rho,(int)old.status,(int)nw.status,nw.stop_detail.limit_loglik,(int)nw.stop_detail.limit_model);
     for(double v:xg)printf("%a,",v);printf("\n");}
  }
}
