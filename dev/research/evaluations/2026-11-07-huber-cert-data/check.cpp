#include "cppunivariate/distributions/huber/mle.hpp"
#include "cppunivariate/distributions/huber/dpqr.hpp"
#include <chrono>
#include <iostream>
#include <iomanip>
int main() {
  std::cout<<std::setprecision(17);
  for(const auto& x:{std::vector<double>{-1,1},std::vector<double>{10,13},
      std::vector<double>{-3,0,3},std::vector<double>{-2,-1,0,1,2},
      std::vector<double>{-10,-.1,0,.1,10},std::vector<double>{-7,-1,-1,0,2,12}}) {
    const auto r=cppunivariate::huber_mle({x.data(),x.size()},1e-10,200,NAN);
    std::cout<<"sample "<<x.size()<<" "<<x.front()<<" status "<<int(r.status)<<" budget "<<r.stop_detail.budget_exhausted<<" tie "<<r.stop_detail.comparison_tie<<" bounds "<<r.stop_detail.loglik_lower<<" "<<r.stop_detail.loglik_upper<<" finite "<<r.mu<<" "<<r.sigma<<" "<<r.epsilon<<" "<<r.logLik<<"\n";
  }
  const std::vector<double>x{-7,-1,-1,0,2,12};
  for(int free_shape=0;free_shape<2;++free_shape)for(int certificate=0;certificate<2;++certificate) {
    const auto begin=std::chrono::steady_clock::now();cppunivariate::HuberResult r{};
    for(int i=0;i<20;++i)r=certificate ?
      cppunivariate::huber_mle({x.data(),x.size()},1e-10,200,free_shape?NAN:1.345) :
      cppunivariate::huber_candidate_mle({x.data(),x.size()},1e-10,200,free_shape?NAN:1.345);
    std::cout<<"timing free="<<free_shape<<" certified="<<certificate<<" seconds="<<std::chrono::duration<double>(std::chrono::steady_clock::now()-begin).count()/20<<" status="<<int(r.status)<<" ll="<<r.logLik<<"\n";
  }
  for(int kind=0;kind<8;++kind) {
    std::vector<double> y;
    for(int i=0;i<30;++i)y.push_back((i%3==0?kind+1.:.1)*(i-10)/10.);
    const auto r=cppunivariate::huber_mle({y.data(),y.size()},1e-10,1,NAN,0,1);
    std::cout<<"low-budget "<<kind<<" status="<<int(r.status)<<" budget="<<r.stop_detail.budget_exhausted<<" tie="<<r.stop_detail.comparison_tie<<" bounds="<<r.stop_detail.loglik_lower<<" "<<r.stop_detail.loglik_upper<<"\n";
  }
  std::vector<double> large;
  for(int i=0;i<100000;++i)large.push_back(cppunivariate::distributions::qhuber((i+.5)/100000,2,3,.8,true,false));
  for(int certificate=0;certificate<2;++certificate) {
    const auto begin=std::chrono::steady_clock::now();
    const auto r=certificate ? cppunivariate::huber_mle({large.data(),large.size()},1e-10,200,NAN) : cppunivariate::huber_candidate_mle({large.data(),large.size()},1e-10,200,NAN);
    std::cout<<"large certified="<<certificate<<" seconds="<<std::chrono::duration<double>(std::chrono::steady_clock::now()-begin).count()<<" status="<<int(r.status)<<" parameters="<<r.mu<<" "<<r.sigma<<" "<<r.epsilon<<" ll="<<r.logLik<<"\n";
  }
  const double gaussian=std::sqrt(2*std::acos(-1.0));
  auto gain=[&](double c) {
    const double z=cppunivariate::huber_z(c),d=2*std::exp(-c*c/2)/(c*c*z);
    return d*d/2-std::log(z/gaussian);
  };
  double lo=.7,hi=.85;
  for(int j=0;j<100;++j){const double c=(lo+hi)/2;if(gain(c)>0)lo=c;else hi=c;}
  const double c=(lo+hi)/2,a=c+2*std::exp(-c*c/2)/(c*c*cppunivariate::huber_z(c));
  const std::vector<double> near_face{a,a};
  for(int cert=0;cert<2;++cert) {
    const auto r=cert ? cppunivariate::huber_mle({near_face.data(),near_face.size()},1e-10,200,NAN,0,1) : cppunivariate::huber_candidate_mle({near_face.data(),near_face.size()},1e-10,200,NAN,0,1);
    std::cout<<"near-normal certified="<<cert<<" critical_c="<<c<<" residual="<<a<<" status="<<int(r.status)<<" tie="<<r.stop_detail.comparison_tie<<" bounds="<<r.stop_detail.loglik_lower<<" "<<r.stop_detail.loglik_upper<<"\n";
  }
}
