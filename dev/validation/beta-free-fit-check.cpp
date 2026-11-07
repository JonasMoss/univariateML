// Datasets and fits for dev/validation/beta-free-fit-check.py (TASK-148).
#include "cppunivariate/distributions/beta/mle.hpp"
#include <cstdio>
#include <vector>
#include <random>
int main(){
  std::vector<std::vector<double>> sets;
  for (double sc : {1e-6, 1e-10, 1e-13, 1e-17, 1e-200}) { std::vector<double> x; for(int i=1;i<=50;++i) x.push_back(sc*i*(1+0.01*(i%7))); sets.push_back(x);}
  { std::vector<double> x; for(int i=1;i<=50;++i) x.push_back(1-1e-12*i*(1+0.01*(i%7))); sets.push_back(x);}
  { std::vector<double> x; for(int i=1;i<=50;++i) x.push_back(0.3+1e-9*(i-25)); sets.push_back(x);}
  { std::vector<double> x; for(int i=1;i<=50;++i) x.push_back(0.3+1e-3*(i-25)); sets.push_back(x);}
  { std::vector<double> x; for(int i=1;i<=50;++i) x.push_back(i%2? 1e-30*i : 1-1e-12*i); sets.push_back(x);}
  { std::vector<double> x{.2,.2,.6,.8}; sets.push_back(x);}
  { std::vector<double> x{.1,.9}; sets.push_back(x);}
  std::mt19937 g(1);
  for (auto [p,q] : std::vector<std::pair<double,double>>{{2.5,.7},{.05,.08},{300,500},{1e4,2}}) {
    std::gamma_distribution<double> ga(p),gb(q); std::vector<double> x; for(int i=0;i<500;++i){double a=ga(g),b=gb(g); double v=a/(a+b); if(v>0&&v<1) x.push_back(v);} sets.push_back(x);}
  std::printf("[");
  for (size_t k=0;k<sets.size();++k){ auto&x=sets[k];
    auto f = cppunivariate::beta_mle(cppunivariate::span{x.data(),x.size()},1.490116e-08,100);
    std::printf("%s{\"status\":%d,\"alpha\":\"%a\",\"beta\":\"%a\",\"ll\":\"%a\",\"it\":%d,\"x\":[",k?",":"",(int)f.status,f.alpha,f.beta,f.logLik,f.iterations);
    for(size_t i=0;i<x.size();++i) std::printf("%s\"%a\"",i?",":"",x[i]);
    std::printf("]}");
  }
  std::printf("]\n");
}
