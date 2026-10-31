#include "cppunivariate/distributions/tbinom/mle.hpp"
#include <algorithm>
#include <chrono>
#include <cmath>
#include <iomanip>
#include <iostream>
#include <limits>
#include <random>
#include <vector>
int main() {
  std::mt19937 rng(1152026);
  const double nan=std::numeric_limits<double>::quiet_NaN();
  std::cout<<"window,sample,n,status,size,prob,loglik,cutoff,microseconds,certified,estimated_work\n"<<std::setprecision(17);
  for(int window=0;window<4;++window)for(int i=0;i<220;++i) {
    int n=i<200 ? (i%3==0?8:i%3==1?16:32) : 200;
    int source=8+(i%5)*4;double p=.15+.15*(i%5);
    std::binomial_distribution<int> dist(source,p);
    std::vector<int> x;while(static_cast<int>(x.size())<n) {int v=dist(rng);if(v>=1 && v<=8)x.push_back(v);}
    double a=window==0||window==2?nan:0;
    double b=window<2?nan:(window==2?10:12);
    auto start=std::chrono::steady_clock::now();
    auto f=cppunivariate::tbinom_design_mle({x.data(),x.size()},a,b,nan);
    auto elapsed=std::chrono::duration_cast<std::chrono::microseconds>(std::chrono::steady_clock::now()-start).count();
    double lo=std::isnan(a)?*std::min_element(x.begin(),x.end()):std::max(0.,std::floor(a)+1);
    double upper=std::isnan(b)?*std::max_element(x.begin(),x.end()):std::floor(b);
    auto face=cppunivariate::tbinom_detail::profile({x.data(),x.size()},lo,upper,-1);
    double cutoff=nan;
    if(face.status==cppunivariate::truncated_detail::Status::Ok && f.logLik>face.logLik+1e-9*n) {
      auto corr=[&](double k){double z=0;for(int v:x)for(int j=v;j<upper;++j)z-=std::log1p(-j/k);return z;};
      double l=upper,h=upper;while(face.logLik+corr(h)>=f.logLik-1e-9*n && h<1e15)h*=2;
      if(h<1e15){while(h-l>1){double m=std::floor((l+h)/2);if(face.logLik+corr(m)<f.logLik-1e-9*n)h=m;else l=m;}cutoff=face.logLik+corr(l)<f.logLik-1e-9*n?l:h;}
    }
    std::cout<<window<<','<<i<<','<<n<<','<<static_cast<int>(f.status)<<','<<f.size<<','<<f.prob<<','<<f.logLik<<','<<cutoff<<','<<elapsed<<','<<cppunivariate::tbinom_detail::size_search_diagnostic.certified<<','<<cppunivariate::tbinom_detail::size_search_diagnostic.work<<'\n';
  }
}
