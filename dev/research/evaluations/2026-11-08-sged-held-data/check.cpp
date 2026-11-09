#include <chrono>
#include "json_loader.hpp"
#include "cppunivariate/distributions/sged/mle.hpp"
#include <iomanip>
#include <iostream>
int main(){
 const std::vector<double>x{-3,-1,-.2,0,.3,.4,2};const cppunivariate::span data{x.data(),x.size()};
 std::cout<<std::setprecision(17);
 for(double p:{.5,1.,1.7,4.,10.}){
   const auto start=std::chrono::steady_clock::now();
   const auto r=cppunivariate::sged_mle(data,1e-10,200,.1,1.4,p);
   std::cout<<"fit\t"<<p<<'\t'<<int(r.status)<<'\t'<<r.certified<<'\t'<<r.xi<<'\t'<<r.logLik<<'\t'<<r.iterations<<'\t'<<std::chrono::duration<double,std::milli>(std::chrono::steady_clock::now()-start).count()<<'\n';
   const cppunivariate::sged_held_moment::Profile profile(data,.1,1.4,p);
   for(double z:{-.9,-.4,.05,.6,.95}){
     const auto b=profile.box(z-.001,z+.001);
     std::cout<<"box\t"<<p<<'\t'<<b.lo<<'\t'<<b.hi<<'\t'<<b.upper<<'\t'<<b.score.lo<<'\t'<<b.score.hi<<'\n';
   }
 }
 const std::vector<double> twins{-1.1,1.1};
 const auto tie=cppunivariate::sged_mle({twins.data(),twins.size()},1e-9,200,0,1,1);
 if(!tie.stop_detail.comparison_tie)return 2;
 for(const auto& witness:tie.stop_detail.candidates)
   std::cout<<"tie\t"<<witness.coef[3]<<'\t'<<witness.loglik_lower<<'\t'<<witness.loglik_upper<<'\n';

 const auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/sged/golden.json");
 const auto held=cppunivariate::sged_mle({g.data.data(),g.data.size()},1e-6,1000,g.estimates.at("mean"),g.estimates.at("sd"),g.estimates.at("nu"));
 std::cout<<"goldenfit\t"<<held.nu<<'\t'<<held.mu<<'\t'<<held.sigma<<'\t'<<int(held.status)<<'\t'<<held.certified<<'\t'<<held.xi<<'\t'<<held.logLik<<'\t'<<held.iterations<<'\n';

 for(double sign:{-1.,1.}) {
   const std::vector<double> edge{sign*.2,sign*.2};
   const auto face=cppunivariate::sged_mle({edge.data(),edge.size()},1e-10,200,0,1,20);
   if(!face.certified || face.status!=(sign>0?cppunivariate::SgedStatus::LeftHalfGedBoundary:cppunivariate::SgedStatus::RightHalfGedBoundary))return 3;
   std::cout<<"face\t"<<-sign<<'\t'<<face.stop_detail.limit_coef[0]<<'\t'<<face.stop_detail.limit_coef[1]<<'\t'<<face.nu<<'\t'<<face.logLik<<'\n';
 }

}
