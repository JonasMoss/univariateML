#include "table2d.hpp"
#include "cppunivariate/distributions/tlambda/dpqr.hpp"
#include <cstdio>
#include <algorithm>
int main(){
  const auto T=t2d::build(33,65,0.25);
  std::vector<std::tuple<double,double,double,double>> v;
  for(int i=0;i<=40;++i){ const double l=-1+2.0*(i+0.37)/41; for(int k=0;k<4000;++k){ double z=std::abs(cppunivariate::distributions::qtlambda((k+.5)/4000,0,1.07,l)); if(!(z>0) || (l>0 && l*z>=1)) continue;
    double ld; const double eta=t2d::eta_of(z,l,ld); double D; t2d::halley(z,ld,l,t2d::guess(T,eta,l),D);
    const double a=-std::log(D), b=cppunivariate::distributions::dtlambda(z,0,1,l,true);
    v.push_back({std::abs(a-b)/(1+std::abs(a)),l,z,a}); v.back();
    std::get<3>(v.back())=a; }}
  std::sort(v.begin(),v.end(),[](auto&x,auto&y){return std::get<0>(x)>std::get<0>(y);});
  for(int i=0;i<12;++i){auto [e,l,z,a]=v[i]; std::printf("%.17g %.17g %.17g %.17g %.3e\n",l,z,cppunivariate::distributions::dtlambda(z,0,1,l,true),a,e);}
}
