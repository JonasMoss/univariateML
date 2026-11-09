#include "cppunivariate/distributions/gev/mle.hpp"
#include <iostream>
#include <iomanip>
int main() {
  const std::vector<double> data{0,1e-12,1e-6,.03,.2,.7,1};
  const cppunivariate::span x{data.data(),data.size()};
  std::cout<<std::setprecision(17)<<"[";bool comma=false;
  for(const auto xi:{cppunivariate::gev_joint_detail::Range{-.001,.001},{-1e-18,1e-18},{0,0},{-.1,-.09},{.1,.11},{-.25,-.25},{.25,.25},{-.8,-.799}}) {
    for(const auto v:{cppunivariate::gev_joint_detail::Range{1,1},{1,1.01},{1.5,1.51}}) {
      const auto b=cppunivariate::gev_joint_detail::evaluate(x,xi,v,100);
      const auto n=cppunivariate::gev_joint_detail::nuisance_evaluate(x,xi,v,100);
      if(!b.ok||!n.ok)return 1;
      if(comma)std::cout<<",";comma=true;
      std::cout<<"{\"xi\":["<<xi.lo<<","<<xi.hi<<"],\"v\":["<<v.lo<<","<<v.hi<<"],\"jet\":[";
      bool c=false;for(auto r:{b.value,b.x,b.v,b.xx,b.xv,b.vv,n.v,n.vv}) {
        if(c)std::cout<<",";c=true;std::cout<<"["<<r.lo<<","<<r.hi<<"]";
      }
      std::cout<<"]}";
    }
  }
  std::cout<<"]\n";
}
