#include "cppunivariate/distributions/bnbinom/mle.hpp"
#include <fstream>
#include <iostream>
#include <iomanip>
int main(){
  std::ifstream input("build/scratch/bnbinom-allfree/counts.txt");std::vector<int>x;int v;
  std::map<int,double>counts;while(input>>v){x.push_back(v);counts[v]++;}
  const auto f=cppunivariate::bnbinom_mle({x.data(),x.size()});
  if(f.status!=cppunivariate::BnbinomStatus::Ok)return 1;
  cppunivariate::bnbinom_detail::Data d;d.n=x.size();
  for(const auto &p:counts){d.x.push_back(p.first);d.count.push_back(p.second);}
  std::cout<<std::setprecision(17)<<"{\"alpha\":"<<f.alpha<<",\"loglik\":"<<f.logLik<<",\"interval\":["<<f.profile_alpha[0]<<','<<f.profile_alpha[1]<<"],\"mode_box\":[";
  for(int j=0;j<4;++j){if(j)std::cout<<',';std::cout<<f.profile_mode[j];}
  std::cout<<"],\"slab_probes\":[";bool first=true;
  for(double width:{1e-6,1e-4,.01,.1,.25}) {
    const auto p=cppunivariate::bnbinom_detail::alpha_interval_profile(d,{f.size,f.alpha,f.beta},f.alpha*(1-width),f.alpha*(1+width),1e-8,600);
    if(!first)std::cout<<',';first=false;
    std::cout<<"{\"relative_alpha_radius\":"<<width<<",\"proved\":"<<(p.proved?"true":"false")<<",\"budget\":"<<(p.budget_exhausted?"true":"false")<<",\"cover_evaluations\":"<<p.evaluations<<'}';
  }
  std::cout<<"]}\n";
}
