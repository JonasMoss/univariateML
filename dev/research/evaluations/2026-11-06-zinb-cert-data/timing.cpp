#include <cppunivariate/distributions/zinb/certificate.hpp>
#include <algorithm>
#include <chrono>
#include <iomanip>
#include <iostream>
#include <vector>
int main() {
  int n;double r,p,pi;std::cin>>n>>r>>p>>pi;
  std::vector<int>x(n);for(int& v:x)std::cin>>v;
  const double nan=std::numeric_limits<double>::quiet_NaN();double checksum=0;
  std::cout<<std::setprecision(12);
  for(int mask=0;mask<7;++mask)for(int method=0;method<2;++method) {
    std::vector<double> batches;
    for(int batch=0;batch<3;++batch) {
      const auto start=std::chrono::steady_clock::now();
      for(int j=0;j<100;++j) {
        cppunivariate::ZinbResult fit;
        if(mask==6)fit=method?cppunivariate::zinb_mle({x.data(),x.size()}):cppunivariate::zinb_candidate_mle({x.data(),x.size()});
        else if(mask<4)fit=method?cppunivariate::zinb_fixed_size_mle({x.data(),x.size()},r,(mask&1)?p:nan,(mask&2)?pi:nan).fit:
          cppunivariate::zinb_candidate_mle({x.data(),x.size()},1e-10,200,r,(mask&1)?p:nan,(mask&2)?pi:nan);
        else fit=method?cppunivariate::zinb_fixed_prob_mle({x.data(),x.size()},p,mask==5?pi:nan).fit:
          cppunivariate::zinb_candidate_mle({x.data(),x.size()},1e-10,200,nan,p,mask==5?pi:nan);
        if(fit.status!=cppunivariate::ZinbStatus::Ok)return 2;
        checksum+=fit.logLik;
      }
      batches.push_back(std::chrono::duration<double,std::micro>(std::chrono::steady_clock::now()-start).count()/100);
    }
    std::sort(batches.begin(),batches.end());
    std::cout<<mask<<' '<<method<<' '<<batches[1]<<'\n';
  }
  std::cerr<<"checksum "<<checksum<<'\n';
}
