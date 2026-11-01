#include "cppunivariate/distributions/slash/mle.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include "baseline.hpp"
#include "json_loader.hpp"
#include <chrono>
#include <iomanip>
#include <iostream>
int main(int argc,char**) {
  cppunivariate::distributions::StdRng<> rng(13320261101ULL);
  std::cout<<std::setprecision(17)<<"sample,n,mask,old_status,new_status,old_ll,new_ll,old_mu,new_mu,old_sigma,new_sigma,old_it,new_it,old_s,new_s\n";
  auto pair=[&](int id,const std::vector<double>& x,int mask,double mu,double sigma){
    const double nan=cppunivariate::special::kNaN;
    const auto start=std::chrono::steady_clock::now();
    const auto a=cppunivariate::slash_baseline_mle({x.data(),x.size()},1e-9,200,mask&2?sigma:nan,mask&1?mu:nan);
    const auto mid=std::chrono::steady_clock::now();
    const auto b=cppunivariate::slash_mle({x.data(),x.size()},1e-9,200,mask&2?sigma:nan,mask&1?mu:nan);
    const auto end=std::chrono::steady_clock::now();
    std::cout<<id<<','<<x.size()<<','<<mask<<','<<int(a.status)<<','<<int(b.status)<<','<<a.logLik<<','<<b.logLik<<','<<a.mu<<','<<b.mu<<','<<a.sigma<<','<<b.sigma<<','<<a.iterations<<','<<b.iterations<<','<<std::chrono::duration<double>(mid-start).count()<<','<<std::chrono::duration<double>(end-mid).count()<<'\n';
    if(a.status!=b.status || (std::isfinite(a.logLik)&&(a.logLik!=b.logLik||a.mu!=b.mu||a.sigma!=b.sigma||a.iterations!=b.iterations)))std::exit(1);
  };
  if(argc>1){const auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/slash/golden.json");for(int rep=0;rep<3;++rep)for(int mask=0;mask<4;++mask)pair(rep,g.data,mask,g.estimates.at("mu"),g.estimates.at("sigma"));return 0;}
  int id=0;
  for(int n:{8,32,128,500})for(int rep=0;rep<150;++rep){
    std::vector<double> x;for(int i=0;i<n;++i)x.push_back(cppunivariate::distributions::rslash(rng));
    // Include tied, separated-mode and differently scaled samples.
    if(rep%5==1)for(int i=0;i<n/4;++i)x[i]=0;
    if(rep%5==2)for(int i=0;i<n;++i)x[i]+=(i<n/2?-30:30);
    const double scale=rep%5==3?1e-5:rep%5==4?1e5:1;
    for(double& v:x)v=2+scale*v;
    for(int mask=0;mask<4;++mask)pair(id,x,mask,2,scale);
    ++id;
  }
}
