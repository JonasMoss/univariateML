#include "cppunivariate/distributions/genbeta/mle.hpp"
#include <iomanip>
#include <iostream>
int main() {
  std::cout<<std::setprecision(17);int sample=0;
  for(const auto& y:std::vector<std::vector<double>>{{.1,.3,1,2.6},{.5,1.5},{.01,.1,.5,3.39},{.2,.6,1.1,2.1}}) {
    double mean=0,m2=0,m4=0,c=0;for(double z:y)mean+=z/y.size();
    for(double z:y){const double v=z/mean;m2+=v*v/y.size();m4+=v*v*v*v/y.size();c-=std::log(v)/y.size();}
    const auto g=cppunivariate::gamma_mle({y.data(),y.size()},1e-10,300);
    const auto t=cppunivariate::genbeta_allfree_bounds::tails(y,mean,0,g.shape,-INFINITY,300);
    std::int64_t work=0;
    const auto b=cppunivariate::genbeta_gamma_face_cert::neighborhood(1,c,m2,m4,t,300,work);
    std::cout<<sample++<<' '<<b.enclosed<<' '<<b.h<<' '<<b.quadratic<<' '<<b.fourth<<' '<<b.kl<<' '<<b.kh<<' '<<b.ql<<' '<<b.qh<<' '<<b.nl<<' '<<b.nh<<'\n';
  }
}
