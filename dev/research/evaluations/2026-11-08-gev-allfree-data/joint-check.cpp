#include "cppunivariate/distributions/gev/mle.hpp"
#include "evt-joint-golden.hpp"
#include <iostream>
#include <iomanip>
int main() {
  auto x=gev_golden;std::sort(x.begin(),x.end());const double m=x.front(),d=x.back()-m;
  for(auto& v:x)v=(v-m)/d;
  const cppunivariate::span data{x.data(),x.size()};
  auto get=[&](double xi){return cppunivariate::gev_joint_detail::inner(data,xi,100);};
  std::cout<<std::setprecision(17)<<"{\"data\":[";
  for(std::size_t j=0;j<x.size();++j)std::cout<<(j?",":"")<<x[j];
  std::cout<<"],\"rows\":[";int index=0;
  for(auto slab:{std::pair<double,double>{-.9,-.89},{-1e-5,1e-5},{.3,.31}}) {
    const bool gap=slab.second<=-.25;
    const auto r=cppunivariate::gev_joint_detail::region(data,slab.first,slab.second,100,get,gap);
    const auto b=cppunivariate::gev_joint_detail::bounds(data,{slab.first,slab.second},r.v,100,gap);
    if(!r.ok||!b.ok)return 1;
    std::cout<<(index++?",":"")<<"{\"xi\":["<<slab.first<<","<<slab.second<<"],\"nuisance\":["<<r.v.lo<<","<<r.v.hi<<"],\"gap\":"<<(gap?"true":"false")<<",\"jet\":[";
    int j=0;for(auto a:{b.value,b.x,b.v,b.xx,b.xv,b.vv})std::cout<<(j++?",":"")<<"["<<a.lo<<","<<a.hi<<"]";
    const auto c=cppunivariate::gev_joint_detail::public_bounds(data,{slab.first,slab.second},r.v,100,gap);
    std::cout<<"],\"coefficients\":[["<<c.first.lo<<","<<c.first.hi<<"],["<<c.second.lo<<","<<c.second.hi<<"]]}";
  }
  const double cut=-.99;
  double scale=0;for(double t:x)scale+=(1-t)/x.size();
  const double face=-std::log(scale)-1;
  const auto e=cppunivariate::gev_joint_detail::coordinate(get(cut),cut,true);
  if(!e.ok)return 1;
  const double upper=cppunivariate::gev_joint_detail::face_upper(data,cut,100,face+1e-10,get);
  std::vector<double> sample{0,.22725282343514291,.56276665892011701,.58275143915118077,1};
  const auto fit=cppunivariate::gev_mle({sample.data(),sample.size()});
  if(fit.status!=cppunivariate::GevStatus::Ok)return 1;
  std::cout<<"],\"face\":{\"cut\":"<<cut<<",\"gap_upper\":"<<e.v.hi<<",\"upper\":"<<upper<<"},\"seeded_sample\":[";
  for(std::size_t j=0;j<sample.size();++j)std::cout<<(j?",":"")<<sample[j];
  std::cout<<"],\"seeded_fit\":["<<fit.mu<<","<<fit.sigma<<","<<fit.xi<<","<<fit.logLik<<"]}\n";
}
