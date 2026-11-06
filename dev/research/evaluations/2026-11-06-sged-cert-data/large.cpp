// Workstation timing probe: smooth certificates, n=100000; deterministic data.
#include <cppunivariate/distributions/sged/mle.hpp>
#include <chrono>
#include <ctime>
#include <iostream>
#include <iomanip>
int main() {
  std::vector<double> x;
  for(int i=0;i<100000;++i)x.push_back(std::sin(.17*i)+.2*std::cos(.31*i)+.001*(i%19)-.2);
  std::cout<<std::setprecision(17);
  bool ok=true;
  for(int which=0;which<3;++which) {
    const auto start=std::chrono::steady_clock::now();const auto cpu=std::clock();
    const auto r=cppunivariate::sged_mle({x.data(),x.size()},1e-8,200,which==2?.1:NAN,which==1?1.5:NAN,4,1.7);
    std::cout<<which<<' '<<x.size()<<' '<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<' '<<double(std::clock()-cpu)/CLOCKS_PER_SEC<<' '<<int(r.status)<<' '<<r.certified<<' '<<r.iterations<<' '<<r.mu<<' '<<r.sigma<<' '<<r.logLik<<'\n';
    ok=ok && r.status==cppunivariate::SgedStatus::Ok && r.certified;
  }
  return ok?0:1;
}
