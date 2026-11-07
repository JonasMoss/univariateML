#include "distributions/huber/fit.hpp"
#include <random>
#include <iostream>
#include <iomanip>
int main() {
  std::mt19937 rng(11629);
  std::normal_distribution<double> normal;
  std::exponential_distribution<double> exponential;
  int budget=0,tie=0,ok=0,face=0;
  std::cout<<std::setprecision(17);
  for(int rep=0;rep<5000;++rep) {
    std::vector<double>x;
    const int n=4*(1+rep%32);
    const double scale=std::exp((rep%21-10)/4.);
    for(int i=0;i<n;++i) {
      double v=rep%3==0 ? normal(rng) : exponential(rng)-exponential(rng);
      if(rep%3==2 && i%7==0)v*=10;
      x.push_back(scale*v);
    }
    const auto r=cppunivariate::huber_mle({x.data(),x.size()},1e-10,1,NAN,0,1);
    budget+=r.stop_detail.budget_exhausted;tie+=r.stop_detail.comparison_tie;
    ok+=r.status==cppunivariate::HuberStatus::Ok;
    face+=r.status==cppunivariate::HuberStatus::NormalLimit;
    if(r.stop_detail.budget_exhausted) {
      const auto out=umvl_binding::fit_huber({x.data(),x.size()},{true,0},{true,1},{},1e-10,1);
      std::cout<<"rep "<<rep<<" n "<<n<<" kind "<<out.error_kind<<" lower "<<r.stop_detail.loglik_lower<<" upper "<<r.stop_detail.loglik_upper<<" candidates "<<r.stop_detail.candidates.size()<<"\nx";
      for(double v:x)std::cout<<" "<<v;
      std::cout<<"\n";
      const auto legacy=cppunivariate::huber_candidate_mle({x.data(),x.size()},1e-10,200,NAN,0,1);
      std::cout<<"legacy default status "<<int(legacy.status)<<" finite point ll "<<legacy.logLik<<" selected face ll "<<legacy.stop_detail.limit_loglik<<"\n";
      const auto resolved=cppunivariate::huber_mle({x.data(),x.size()},1e-10,200,NAN,0,1);
      std::cout<<"raised iterlim status "<<int(resolved.status)<<" budget "<<resolved.stop_detail.budget_exhausted<<" tie "<<resolved.stop_detail.comparison_tie<<" ll "<<resolved.logLik<<"\n";
      return out.error_kind=="budget" && r.stop_detail.loglik_upper>=r.stop_detail.loglik_lower && !resolved.stop_detail.budget_exhausted ? 0 : 1;
    }
  }
  std::cout<<"5000 samples: budget="<<budget<<" tie="<<tie<<" ok="<<ok<<" normal="<<face<<"\n";
}
