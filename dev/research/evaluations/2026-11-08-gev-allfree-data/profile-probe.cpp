#include "cppunivariate/distributions/frechet/mle.hpp"
#include "evt-joint-golden.hpp"
#include <iostream>
#include <iomanip>
int main(){
 std::cout<<std::setprecision(17);
 for(int family=0;family<2;++family) {
  auto x=family==0?gev_golden:frechet_golden;std::sort(x.begin(),x.end());const double origin=x.front(),range=x.back()-origin;
  for(double& v:x)v=(v-origin)/range;
  const cppunivariate::span d{x.data(),x.size()};const auto incumbent=cppunivariate::gev_mle(d);
  for(double xi:{-1.,-.99,-.5,-.1,-.01,0.,.01,.1,.3,.5,1.}) {
   const auto f=cppunivariate::gev_profile(d,xi,1e-12,1000);
   const double gap=(f.fit.logLik-incumbent.logLik)/x.size();
   double lower=NAN,upper=NAN;
   if(xi<=0 && gap>=0) {
    auto solve=[&](bool left){double lo=left?std::exp(-gap-2):1.,hi=left?1.:2*(gap+2);for(int j=0;j<100;++j){const double r=(lo+hi)/2;const bool inside=r-1-std::log(r)<=gap;if(inside==left)hi=r;else lo=r;}return (lo+hi)/2;};
    lower=solve(true);upper=solve(false);
   }
   std::cout<<(family==0?"gev":"frechet")<<" "<<xi<<" "<<int(f.fit.status)<<" "<<f.fit.sigma*range<<" "<<f.score<<" "<<f.curvature<<" "<<gap<<" "<<lower<<" "<<upper<<"\n";
  }
 }
}
