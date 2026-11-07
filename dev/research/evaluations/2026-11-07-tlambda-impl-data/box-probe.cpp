// Cost probe for the three-dimensional certificate; no fitting route calls it.
#include "cppunivariate/distributions/tlambda/mle.hpp"
#include <iostream>
#include <iomanip>
#include <chrono>
struct Box {std::array<double,3> lo,hi;double upper;};
int main(int argc,char**) {
  const bool negative=argc>1;
  std::vector<double> x;
  for(int i=0;i<31;++i)x.push_back(cppunivariate::distributions::qtlambda((i+.5)/31,0,1,.14));
  const cppunivariate::span data{x.data(),x.size()};
  const auto f=cppunivariate::tlambda_mle(data,1e-7,1000,negative?-1:NAN);
  if(f.status!=cppunivariate::TlambdaStatus::Ok)return 1;
  const double n=x.size(),target=f.logLik-1e-7*n;
  double delta=INFINITY;
  for(std::size_t i=1;i<x.size();++i)delta=std::min(delta,(x[i]-x[i-1])/3);
  const double logM=(negative?-3:-1)*std::log(2.),smax=logM-target/n,distance=std::exp(logM/2-target/n);
  double smin=(target+2*n*std::log(delta))/n;
  for(int k=1;k<=1;++k)smin=std::min(smin,(target-k*logM+2*(n-k)*std::log(delta))/(n-2*k));
  const std::array<double,3> lower{x.front()-distance,smin,-1},upper{x.back()+distance,smax,negative?-1.:1.};
  std::int64_t work=0;
  auto bound=[&](Box& b) {
    ++work;double v=-n*b.lo[1];
    for(double z:x) {
      const double d=z>=b.lo[0] && z<=b.hi[0]?0:std::min(std::abs(z-b.lo[0]),std::abs(z-b.hi[0]));
      const double t=d/std::exp(b.hi[1]);
      if(b.lo[2]>0 && t>1/b.lo[2]){b.upper=-INFINITY;return true;}
      const auto p=cppunivariate::tlambda_cert_detail::inverse(t,b.lo[2],1000);
      if(!p.ok)return false;
      v-=cppunivariate::tlambda_detail::logderiv(-p.lo,b.hi[2]);
    }
    b.upper=v+cppunivariate::tlambda_cert_detail::margin(v+n);return true;
  };
  const auto begin=std::chrono::steady_clock::now();
  Box root{lower,upper,INFINITY};if(!bound(root))return 2;
  std::vector<Box> boxes{root};
  auto order=[](const Box& a,const Box& b){return a.upper<b.upper;};
  const auto cap=cppunivariate::certificate_budget(1000);
  while(work+2<=cap && !boxes.empty() && boxes.front().upper>f.logLik+1e-7*n) {
    std::pop_heap(boxes.begin(),boxes.end(),order);const auto b=boxes.back();boxes.pop_back();
    int j=0;for(int k=1;k<3;++k)if(upper[k]>lower[k] && (b.hi[k]-b.lo[k])/(upper[k]-lower[k])>(b.hi[j]-b.lo[j])/(upper[j]-lower[j]))j=k;
    const double mid=b.lo[j]+(b.hi[j]-b.lo[j])/2;
    if(mid==b.lo[j] || mid==b.hi[j])return 3;
    Box a=b,c=b;a.hi[j]=mid;c.lo[j]=mid;
    for(auto candidate:{a,c}) {
      if(!bound(candidate))return 4;
      if(candidate.upper>f.logLik+1e-7*n){boxes.push_back(candidate);std::push_heap(boxes.begin(),boxes.end(),order);}
    }
  }
  std::cout<<std::setprecision(17)<<"{\"fixed_negative_shape\":"<<(negative?"true":"false")<<",\"n\":31,\"iterlim\":1000,\"evaluations\":"<<work<<",\"unresolved_boxes\":"<<boxes.size()<<",\"incumbent\":"<<f.logLik<<",\"global_upper\":"<<(boxes.empty()?f.logLik:boxes.front().upper)<<",\"required_gap\":"<<1e-7*n<<",\"seconds\":"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-begin).count()<<"}\n";
}
