#include "cppunivariate/distributions/bhatt/mle.hpp"
#include "../../../../univariateML-cpp/tests/json_loader.hpp"
#include <iostream>
#include <iomanip>
int main(){
  const auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/bhatt/golden.json");
  bool ok=true;std::cout<<std::setprecision(17);
  for(int mask=1;mask<8;++mask){
    const auto f=cppunivariate::bhatt_mle(mltest::as_span(g.data),1e-10,1000,mask&2?g.estimates.at("sigma"):NAN,mask&4?g.estimates.at("a"):NAN,mask&1?g.estimates.at("mu"):NAN);
    std::cout<<"held_mask="<<mask<<" status="<<int(f.status)<<" iterations="<<f.iterations<<" mu="<<f.mu<<" sigma="<<f.sigma<<" a="<<f.a<<"\n";
    ok=ok&&f.status==cppunivariate::BhattStatus::Ok;
  }
  const std::vector<double> x{10};
  cppunivariate::bhatt_detail::WidthCertificate certificate(mltest::as_span(x),1e-10,500,0,.1);
  cppunivariate::bhatt_detail::CertRoot left,right;
  const bool bounds=certificate.profile(90,left)&&certificate.profile(110,right)&&certificate.push(90,110,left.ll_upper,right.ll_upper);
  const double peak=cppunivariate::distributions::dbhatt(10,0,.1,10.25,true);
  std::cout<<"peak="<<peak<<" tangent_upper="<<(bounds?certificate.queue.top().upper:NAN)<<"\n";
  ok=ok&&bounds&&certificate.queue.top().upper>=peak;
  const std::vector<double> tiny{1e-200,2e-200};
  const auto normal=cppunivariate::bhatt_mle(mltest::as_span(tiny),1e-10,500,NAN,0,0);
  std::cout<<"normal_tiny_status="<<int(normal.status)<<" relative_sigma="<<normal.sigma/1e-200<<"\n";
  ok=ok&&normal.status==cppunivariate::BhattStatus::Ok;
  std::vector<double> spiky(20,0);spiky.push_back(-10);spiky.push_back(10);
  const auto unresolved=cppunivariate::bhatt_mle(mltest::as_span(spiky),1e-8,500,4,NAN,0);
  std::cout<<"normal_unresolved_status="<<int(unresolved.status)<<" witnesses="<<unresolved.stop_detail.candidates.size()<<"\n";
  ok=ok&&unresolved.status==cppunivariate::BhattStatus::Unresolved;
  return ok?0:1;
}
