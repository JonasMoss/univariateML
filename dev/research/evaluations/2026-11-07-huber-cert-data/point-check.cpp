#include "cppunivariate/distributions/huber/mle.hpp"
#include "cppunivariate/distributions/huber/dpqr.hpp"
#include <iostream>
#include <iomanip>
int main() {
  std::vector<double> x;
  for(int i=0;i<100000;++i)x.push_back(cppunivariate::distributions::qhuber((i+.5)/100000,2,3,.8,true,false));
  std::cout<<std::setprecision(17);
  for(int cert=0;cert<2;++cert) {
    const auto r=cert ? cppunivariate::huber_mle({x.data(),x.size()},1e-10,200,NAN) :
        cppunivariate::huber_candidate_mle({x.data(),x.size()},1e-10,200,NAN);
    long double h=0,g=0,t=0,ll=0;
    for(double v:x) {
      const long double z=(static_cast<long double>(v)-r.mu)/r.sigma,a=std::abs(z),c=r.epsilon;
      const long double psi=a<=c ? z : std::copysign(c,z);
      h+=psi;g+=z*psi;if(a>c)t+=c*(a-c);
      ll-=std::log(r.sigma)+std::log(cppunivariate::huber_z(r.epsilon))+
          (a<=c ? z*z/2 : c*(a-c/2));
    }
    const long double score=2*x.size()*std::exp(-r.epsilon*r.epsilon/2)/
        (r.epsilon*cppunivariate::huber_z(r.epsilon))-t;
    std::cout<<"certified="<<cert<<" stored="<<r.logLik<<" point="<<double(ll)
        <<" location_mean="<<double(h/x.size())<<" scale_mean="<<double(g/x.size()-1)
        <<" total_shape="<<double(score)<<" gate="<<cppunivariate::huber_free_score_gate({x.data(),x.size()},r,false,false,1e-10)<<"\n";
    if(cert && (r.status!=cppunivariate::HuberStatus::Ok || std::abs(score)>1e-3 ||
        std::abs(h/x.size())>1e-7 || std::abs(g/x.size()-1)>1e-10 ||
        std::abs(ll-r.logLik)>1e-8))return 1;
  }
}
