#include "distributions/huber/fit.hpp"
#include <iostream>
int main() {
  int failed=0;
  for(double scale:{1e-300,1e300}) {
    const std::vector<double> x{-scale,scale};
    const auto r=cppunivariate::huber_mle({x.data(),x.size()},1e-10,200,NAN);
    const auto b=umvl_binding::fit_huber({x.data(),x.size()});
    std::cout<<scale<<" status="<<int(r.status)<<" kind="<<b.error_kind
      <<" core_candidates="<<r.stop_detail.candidates.size()
      <<" binding_candidates="<<b.candidates.size()
      <<" lower="<<r.stop_detail.loglik_lower<<" upper="<<r.stop_detail.loglik_upper<<"\n";
    failed+=b.error_kind!="numerical" || b.candidates.empty() ||
      !std::isfinite(b.loglik_lower) || b.loglik_upper!=INFINITY;
  }
  const std::vector<double> x{-100,100};
  const auto b=umvl_binding::fit_huber({x.data(),x.size()},{},
      {true,std::numeric_limits<double>::min()},{true,1.345});
  std::cout<<"nonrepresentable objective kind="<<b.error_kind<<" candidates="
    <<b.candidates.size()<<" lower="<<b.loglik_lower<<" upper="<<b.loglik_upper<<"\n";
  failed+=b.error_kind!="numerical" || !std::isnan(b.loglik_lower) || b.loglik_upper!=INFINITY;
  std::vector<double> large{0};
  for(int j=0;j<2500;++j){large.push_back(-1e308);large.push_back(1e308);}
  const auto large_fit=cppunivariate::huber_mle({large.data(),large.size()},1e-10,200,NAN);
  bool laplace=false;
  for(const auto& candidate:large_fit.stop_detail.candidates)
    if(candidate.model==cppunivariate::StopModel::Laplace &&
        candidate.coef.size()==2 && candidate.coef[1]>0 &&
        std::isfinite(candidate.coef[1]) && std::isfinite(candidate.loglik_lower))laplace=true;
  std::cout<<"large Laplace witness retained="<<laplace<<" status="<<int(large_fit.status)<<"\n";
  failed+=!laplace;
  return failed;
}
