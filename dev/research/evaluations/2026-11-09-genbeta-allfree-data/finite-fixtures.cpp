#include "cppunivariate/distributions/genbeta/mle.hpp"
#include "cppunivariate/distributions/genbeta/dpqr.hpp"
#include <iostream>
#include <iomanip>
int main(){std::cout<<std::setprecision(17);
 for(auto v:{std::array<double,3>{{.4,.3,2}},std::array<double,3>{{3,2,.7}},std::array<double,3>{{.3,4,3}}}){
  std::vector<double>x;for(int i=0;i<300;++i)x.push_back(cppunivariate::distributions::qgenbeta((i+.5)/300,v[0],v[1],v[2],1,true,false));
  const auto r=cppunivariate::genbeta_mle({x.data(),x.size()},1);
  std::cout<<v[0]<<" "<<v[1]<<" "<<v[2]<<" "<<int(r.status)<<" "<<int(r.certificate_failure)<<" "<<r.allfree_certified<<" "<<r.iterations<<" "<<r.shape1<<" "<<r.shape2<<" "<<r.shape3<<" "<<r.logLik<<"\n";
  if(v[0]==3) {
    if(r.status!=cppunivariate::GenbetaStatus::Ok||r.stop_detail.budget_exhausted||
       r.allfree_certified||r.stop_detail.candidates.empty()||!std::isinf(r.stop_detail.loglik_upper))return 1;
  } else if(r.status!=cppunivariate::GenbetaStatus::Ok||!r.allfree_certified)return 1;
 }}
