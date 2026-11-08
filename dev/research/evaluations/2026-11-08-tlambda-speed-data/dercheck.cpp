#include "proto.hpp"
#include <cstdio>
using namespace cppunivariate;
int main(){
  for(double l:{-0.9,-0.3,0.0,0.14,0.6}) for(double z:{-3.0,-0.4,0.2,1.1,7.0}){
    if(l>0 && std::abs(z)*l>=1) continue;
    auto lg=[&](double zz,double ll){return distributions::dtlambda(zz,0,1,ll,true);};
    const double h=1e-5;
    auto o=proto::eval(z,l==0?1e-300:l);
    auto o0=proto::eval(z,l);
    double psi=(lg(z+h,l)-lg(z-h,l))/(2*h), dpsi=(lg(z+h,l)-2*lg(z,l)+lg(z-h,l))/(h*h), dl=(lg(z,l+h)-lg(z,l-h))/(2*h);
    std::printf("l=%5.2f z=%5.2f lg %.3e psi %.3e dpsi %.3e dl %.3e\n",l,z,o0.lg-lg(z,l),o0.psi-psi,o0.dpsi-dpsi,o0.dl-dl);
  }
}
