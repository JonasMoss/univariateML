#include <cppunivariate/distributions/zinb/certificate.hpp>
#include <chrono>
#include <iomanip>
#include <iostream>
#include <vector>
int main() {
  int n,held;double r,p,pi;
  std::cout<<std::setprecision(17);
  while(std::cin>>n>>r>>p>>pi>>held) {
    std::vector<int>x(n);for(int& v:x)std::cin>>v;
    const double nan=std::numeric_limits<double>::quiet_NaN();
    const auto start=std::chrono::steady_clock::now();
    const auto a=r<0?cppunivariate::zinb_fixed_prob_mle({x.data(),x.size()},p,(held&2)?pi:nan):cppunivariate::zinb_fixed_size_mle({x.data(),x.size()},r,(held&1)?p:nan,(held&2)?pi:nan);
    const auto mid=std::chrono::steady_clock::now();
    const auto b=cppunivariate::zinb_candidate_mle({x.data(),x.size()},1e-10,200,r<0?nan:r,(held&1)?p:nan,(held&2)?pi:nan);
    const auto end=std::chrono::steady_clock::now();
    std::cout<<int(a.fit.status)<<' '<<int(a.failure)<<' '<<a.fit.prob<<' '<<a.fit.pi<<' '<<a.fit.logLik<<' '
      <<a.stop_detail.loglik_lower<<' '<<a.stop_detail.loglik_upper<<' '<<a.stop_detail.candidates.size()<<' '
      <<int(b.status)<<' '<<b.prob<<' '<<b.pi<<' '<<b.logLik<<' '
      <<std::chrono::duration<double,std::micro>(mid-start).count()<<' '
      <<std::chrono::duration<double,std::micro>(end-mid).count()<<' '<<a.fit.size;
    for(const auto& c:a.stop_detail.candidates)std::cout<<' '<<c.coef[1]<<' '<<c.loglik_lower<<' '<<c.loglik_upper;
    std::cout<<'\n';
  }
}
