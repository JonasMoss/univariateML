#include "cppunivariate/distributions/sstd/mle.hpp"
#include "cppunivariate/distributions/sstd/dpqr.hpp"
#include "cppunivariate/distributions/sged/dpqr.hpp"
#include "cppunivariate/distributions/st/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <iostream>
#include <iomanip>
int main(){
 std::cout<<std::setprecision(17);
 std::cout<<"nu,t_cdf0,t_q25,ged_cdf0,ged_q25,ged_draw_min,ged_draw_max\n";
 for(double nu:{128.,1e4,1e8,1e12,1e16,1e100,std::numeric_limits<double>::max()}){
  cppunivariate::distributions::StdRng<> rng(157);
  double lo=INFINITY,hi=-INFINITY;
  for(int i=0;i<100;++i){const double v=cppunivariate::distributions::rsged(rng,0,1,nu,2);lo=std::min(lo,v);hi=std::max(hi,v);}
  std::cout<<nu<<','<<cppunivariate::distributions::psstd(0,0,1,nu,2,true,false)<<','
   <<cppunivariate::distributions::qsstd(.25,0,1,nu,2,true,false)<<','
   <<cppunivariate::distributions::psged(0,0,1,nu,2,true,false)<<','
   <<cppunivariate::distributions::qsged(.25,0,1,nu,2,true,false)<<','<<lo<<','<<hi<<'\n';
 }
}
