#include "baseline.hpp"
#include "cppunivariate/distributions/sn/mle.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include "../../../../univariateML-cpp/tests/json_loader.hpp"
#include <chrono>
#include <iostream>
#include <iomanip>
int main(int argc,char**) {
  std::cout<<std::setprecision(17)<<"sample,n,alpha,mask,old_status,new_status,old_ll,new_ll,old_seconds,new_seconds,old_evaluations,new_evaluations\n";
  cppunivariate::distributions::StdRng<> rng(13320261101ULL);
  const double nan=cppunivariate::special::kNaN;
  double held_xi=0,held_omega=1;
  const double tol=argc>2?1e-8:1e-9;
  const int maxit=argc>2?1000:100;
  const auto run=[&](int sample,const std::vector<double>& x,double a,int mask) {
    const cppunivariate::span data{x.data(),x.size()};
    const double xi=mask&1?held_xi:nan,w=mask&2?held_omega:nan,alpha=mask&4?a:nan;
    auto t=std::chrono::steady_clock::now();
    const auto old=cppunivariate::sn_baseline_mle(data,tol,maxit,xi,w,alpha);
    const double ot=std::chrono::duration<double>(std::chrono::steady_clock::now()-t).count();
    t=std::chrono::steady_clock::now();
    const auto now=cppunivariate::sn_mle(data,tol,maxit,xi,w,alpha);
    const double nt=std::chrono::duration<double>(std::chrono::steady_clock::now()-t).count();
    std::cout<<sample<<','<<x.size()<<','<<a<<','<<mask<<','<<int(old.status)<<','<<int(now.status)<<','<<old.logLik<<','<<now.logLik<<','<<ot<<','<<nt<<','<<old.iterations<<','<<now.iterations<<std::endl;
  };
  if(argc>1) {
    const auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/sn/golden.json");
    held_xi=g.estimates.at("xi"); held_omega=g.estimates.at("omega");
    for(int rep=0;rep<3;++rep) for(int mask=0;mask<8;++mask)run(-rep-1,g.data,g.estimates.at("alpha"),mask);
    return 0;
  }
  int sample=0;
  for(int n:{8,32,128,500}) for(double a:{-1000.,-30.,-8.,-2.,0.,2.,8.,30.,1000.}) for(int rep=0;rep<17;++rep) {
    std::vector<double> x;
    for(int i=0;i<n;++i)x.push_back(cppunivariate::distributions::rsn(rng,0,1,a));
    for(int mask=0;mask<8;++mask)run(sample,x,a,mask);
    ++sample;
  }
}
