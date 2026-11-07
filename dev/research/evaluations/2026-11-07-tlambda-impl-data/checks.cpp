#include "cppunivariate/distributions/tlambda/mle.hpp"
#include "bindings/common/distributions/tlambda/fit.hpp"
#include <iostream>
#include <iomanip>
int main() {
  std::cout<<std::setprecision(17);
  for(double l:{-3.,-1.,-.1,-1e-12,0.,1e-12,.14,.9,1.5,3.})
    for(double p:{1e-50,1e-12,.01,.1,.499999,.5}) {
      const double z=-cppunivariate::distributions::qtlambda(p,0,1,l);
      if(!std::isfinite(z) || (l>0 && z>=1/l))continue;
      const auto b=cppunivariate::tlambda_cert_detail::inverse(z,l,1000);
      std::cout<<"inverse "<<l<<' '<<z<<' '<<b.lo<<' '<<b.hi<<' '<<b.ok<<'\n';
    }
  const std::vector<double> x{-2,-1,.5,2};
  for(double l:{0.,.14,.5,.9})for(int mask:{0,1,2,3}) {
    const auto f=cppunivariate::tlambda_mle({x.data(),x.size()},1e-7,1000,l,mask&1?.2:NAN,mask&2?4:NAN);
    std::cout<<"fit "<<l<<' '<<mask<<' '<<int(f.status)<<' '<<f.mu<<' '<<f.sigma<<' '<<f.logLik<<' '<<f.certificate_lower<<' '<<f.certificate_upper<<'\n';
  }
  for(int kind:{0,1,2}) {
    const auto f=cppunivariate::tlambda_mle({x.data(),x.size()},1e-6,2000,kind==2?NAN:-1,kind==0?NAN:0,kind==1?NAN:1);
    std::cout<<"scalar "<<kind<<' '<<int(f.status)<<' '<<f.mu<<' '<<f.sigma<<' '<<f.lambda<<' '<<f.logLik<<' '<<f.certificate_lower<<' '<<f.certificate_upper<<' '<<f.iterations<<'\n';
  }
  for(int kind:{3,4,5}) {
    const auto f=cppunivariate::tlambda_mle({x.data(),x.size()},1e-6,2000,3,kind==5?0:NAN,kind==3?9:NAN);
    std::cout<<"scalar "<<kind<<' '<<int(f.status)<<' '<<f.mu<<' '<<f.sigma<<' '<<f.lambda<<' '<<f.logLik<<' '<<f.certificate_lower<<' '<<f.certificate_upper<<' '<<f.iterations<<'\n';
  }
  // Independent binding-neutral checks also exercise the low-budget payload.
  const std::vector<double> modes{-80./9,80./9};
  const auto budget=umvl_binding::fit_tlambda({modes.data(),modes.size()},{},{true,1},{true,-1},1e-12,64);
  const std::vector<double> tied{-.25,.25};
  const auto tie=umvl_binding::fit_tlambda({tied.data(),tied.size()},{},{true,1},{true,1.5});
  if(budget.error_kind!="budget" || budget.candidates.empty() || tie.error_kind!="tie" || tie.candidates.size()!=2)return 1;
}
