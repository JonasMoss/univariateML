#include "cppunivariate/distributions/pareto4/mle.hpp"
#include "json_loader.hpp"
#include <iostream>
#include <iomanip>
int main() {
  const auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/pareto4/golden.json");
  const auto r=cppunivariate::pareto4_mle(cppunivariate::span{g.data.data(),g.data.size()},g.estimates.at("min"),1e-10,1000);
  long double ll=0,gk=g.data.size(),gc=g.data.size(),gs=-static_cast<long double>(g.data.size())*r.shape2;
  for(double v:g.data) {
    const long double t=std::log((static_cast<long double>(v)-r.min)/r.scale),z=r.shape2*t;
    const long double sp=std::max(z,0.0L)+std::log1p(std::exp(-std::abs(z))),p=z>0?1/(1+std::exp(-z)):std::exp(z)/(1+std::exp(z));
    ll+=std::log(static_cast<long double>(r.shape1))+std::log(static_cast<long double>(r.shape2))-std::log(static_cast<long double>(r.scale))+(r.shape2-1)*t-(r.shape1+1)*sp;
    gk-=r.shape1*sp;gc+=z-(r.shape1+1)*z*p;gs+=(r.shape1+1)*r.shape2*p;
  }
  std::cout<<std::setprecision(17)<<int(r.status)<<' '<<r.shape1<<' '<<r.shape2<<' '<<r.scale<<' '<<r.logLik<<"\nrelative errors "<<r.shape1/g.estimates.at("shape1")-1<<' '<<r.shape2/g.estimates.at("shape2")-1<<' '<<r.scale/g.estimates.at("scale")-1<<"\nindependent ll "<<ll<<" ll delta "<<ll-g.logLik<<" score "<<std::sqrt(gk*gk+gc*gc+gs*gs)<<'\n';
}
