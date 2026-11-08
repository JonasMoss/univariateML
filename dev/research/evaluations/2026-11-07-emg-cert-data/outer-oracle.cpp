#include "cppunivariate/distributions/emg/mle.hpp"
#include "json_loader.hpp"
#include <iostream>
#include <iomanip>
int main() {
  const auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/emg/golden.json");
  std::cout<<std::unitbuf<<std::setprecision(17)<<"mask,status,mu,sigma,lambda,mu_lo,mu_hi,sigma_lo,sigma_hi,lambda_lo,lambda_hi,root,accurate\n";
  for(int mask=0;mask<7;++mask) {
    const double m=mask&1?g.estimates.at("mu"):NAN,s=mask&2?g.estimates.at("sigma"):NAN,l=mask&4?g.estimates.at("lambda"):NAN;
    const cppunivariate::span x{g.data.data(),g.data.size()};
    const auto f=cppunivariate::emg_mle(x,1e-10,500,m,s,l);
    const double k=f.sigma*f.lambda;
    const cppunivariate::emg_detail::CertModel model(x,m,s,l,mask!=6,k);
    auto p=model.initial(k);
    if(model.location>=0)p[model.location]=model.fixed_rate?f.mu*l:f.mu/f.sigma;
    if(model.scale>=0)p[model.scale]=1/f.sigma;
    if(model.shape>=0)p[model.shape]=k;
    const auto c=cppunivariate::emg_detail::cert_validate_root(model,p,1e-10,500);
    const auto coef=model.coefficients(c.enclosure);
    std::cout<<mask<<','<<static_cast<int>(f.status)<<','<<f.mu<<','<<f.sigma<<','<<f.lambda;
    for(const auto v:coef)std::cout<<','<<v.lo<<','<<v.hi;
    std::cout<<','<<c.ok<<','<<c.accurate<<'\n';
  }
}
