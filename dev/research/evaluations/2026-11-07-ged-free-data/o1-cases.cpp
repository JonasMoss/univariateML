#include "cppunivariate/distributions/ged/mle.hpp"
#include <iostream>
int main(){std::vector<double>x;double v;while(std::cin>>v)x.push_back(v);for(int mask=0;mask<4;++mask)for(int it:{1,2,10,200}){auto r=cppunivariate::ged_mle({x.data(),x.size()},1.220703e-4,it,mask&1?-.017698887677594307:NAN,mask&2?1.0178559902092952:NAN,NAN);std::cout<<mask<<" "<<it<<" "<<int(r.status)<<" "<<r.certified<<" "<<r.stop_detail.budget_exhausted<<" "<<r.stop_detail.candidates.size()<<" "<<r.stop_detail.loglik_lower<<" "<<r.stop_detail.loglik_upper<<"\n";}}
