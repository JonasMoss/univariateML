// Diagnostic subdivision only. Excludes intervals strictly below an evaluated
// witness; never returns a fitted estimate or asserts root existence/uniqueness.
#include "cppunivariate/distributions/ged/mle.hpp"
#include <algorithm>
#include <iomanip>
#include <iostream>
#include <queue>
#include <vector>
#include <utility>
struct Segment {
  double lo,hi,upper;
  bool operator<(const Segment& b)const{return upper<b.upper;}
};
int main() {
  std::vector<double>x;double v;while(std::cin>>v)x.push_back(v);
  const cppunivariate::span data{x.data(),x.size()};
  std::cout<<std::setprecision(17);
  for(int mask=0;mask<4;++mask) {
    const double mean=mask&1?-.017698887677594307:NAN;
    const double sd=mask&2?1.0178559902092952:NAN;
    const auto r=cppunivariate::ged_mle(data,1e-8,200,mean,sd,NAN);
    if(r.status!=cppunivariate::GedStatus::Ok){std::cout<<mask<<" no_witness\n";continue;}
    const auto witness=cppunivariate::ged_detail::conditional(data,r.nu,r.mu,r.sigma,200);
    const double lower=witness.logLik-cppunivariate::ged_shape_detail::margin(std::abs(witness.logLik),x.size())*(1+r.nu);
    const long long cap=cppunivariate::certificate_budget(200);
    long long work=0;double cutoff=2;
    auto tail=cppunivariate::ged_shape_detail::tail(data,cutoff,mean,sd,200);++work;
    while(tail.valid && tail.upper>=lower && work<cap) {
      cutoff*=2;tail=cppunivariate::ged_shape_detail::tail(data,cutoff,mean,sd,200);++work;
    }
    if(!tail.valid || tail.upper>=lower){std::cout<<mask<<" tail_unclosed "<<work<<"\n";continue;}
    std::priority_queue<Segment> queue;
    auto initial=cppunivariate::ged_shape_detail::interval(data,1,cutoff,mean,sd,200);++work;
    queue.push({1,cutoff,initial.upper});
    bool failed=false;std::vector<Segment> retained;
    while(!queue.empty() && work+2<=cap) {
      const auto segment=queue.top();
      if(segment.upper<lower){queue.pop();continue;}
      const double left=queue.top().lo,right=queue.top().hi;
      // This is only a diagnostic enclosure of unresolved shape values.
      // It supplies no quantitative root inclusion inside the small interval.
      if(left>=r.nu-1e-5 && right<=r.nu+1e-5){retained.push_back(segment);queue.pop();continue;}
      queue.pop();const double mid=left/2+right/2;
      for(const auto endpoints:{std::pair<double,double>{left,mid},{mid,right}}) {
        const auto bound=cppunivariate::ged_shape_detail::interval(data,endpoints.first,endpoints.second,mean,sd,200);++work;
        if(!bound.valid){failed=true;break;}
        if(bound.upper>=lower)queue.push({endpoints.first,endpoints.second,bound.upper});
      }
      if(failed)break;
    }
    double lo=INFINITY,hi=-INFINITY,upper=lower;
    const auto remaining=queue.size()+retained.size();
    for(const auto& s:retained){lo=std::min(lo,s.lo);hi=std::max(hi,s.hi);upper=std::max(upper,s.upper);}
    while(!queue.empty()){const auto s=queue.top();queue.pop();lo=std::min(lo,s.lo);hi=std::max(hi,s.hi);upper=std::max(upper,s.upper);}
    std::cout<<mask<<" cutoff "<<cutoff<<" evaluations "<<work<<" remaining "<<remaining
             <<" unresolved_shape "<<lo<<" "<<hi<<" likelihood_gap "<<upper-lower
             <<" arithmetic_failure "<<failed<<"\n";
  }
}
