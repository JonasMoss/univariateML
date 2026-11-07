#include "cppunivariate/distributions/fatigue/mle.hpp"
#include "cppunivariate/distributions/sgomp/mle.hpp"
#include "cppunivariate/distributions/genbeta/mle.hpp"
#include <iostream>
#include <iomanip>
#include <random>
int main() {
  std::cout<<std::setprecision(17);
  std::mt19937 rng(11622);std::normal_distribution<double> normal;
  for(int j=0;j<24;++j) {
    std::vector<double> x;for(int i=0;i<8;++i)x.push_back(std::exp(normal(rng)*(j%3+1)));
    const double a=j%2?3:.7;
    const auto f=cppunivariate::fatigue_mle({x.data(),x.size()},1e-10,300,a);
    const auto p=cppunivariate::fatigue_mle({x.data(),x.size()},1e-10,300);
    const auto s=cppunivariate::sgomp_mle({x.data(),x.size()},1e-10,300);
    std::cout<<"sample "<<j;for(double v:x)std::cout<<" "<<v;std::cout<<"\n";
    std::cout<<"fatigue "<<int(f.status)<<" "<<f.alpha<<" "<<f.beta<<" "<<f.logLik<<"\n";
    std::cout<<"profile "<<int(p.status)<<" "<<p.alpha<<" "<<p.beta<<" "<<p.logLik<<"\n";
    std::cout<<"sgomp "<<int(s.status)<<" "<<s.b<<" "<<s.eta<<" "<<s.logLik<<" "<<s.stop_detail.budget_exhausted<<" "<<s.stop_detail.comparison_tie<<"\n";
  }
  for(double p:{.1,2.,10.})for(double q:{.01,.7,2.,30.}) {
    const std::vector<double> x{.02,.2,.6,.9};
    const auto f=cppunivariate::genbeta_mle({x.data(),x.size()},1,1e-7,200,p,q);
    std::cout<<"genbeta "<<int(f.status)<<" "<<p<<" "<<q<<" "<<f.shape3<<" "<<f.logLik<<"\n";
  }
  for(double q:{1e-16,1e-100,1e-300}) {
    const std::vector<double> x{.02,.2,.6,.9};
    const auto f=cppunivariate::genbeta_mle({x.data(),x.size()},1,1e-7,200,2,q);
    std::cout<<"genbeta "<<int(f.status)<<" "<<2<<" "<<q<<" "<<f.shape3<<" "<<f.logLik<<"\n";
  }
  {
    const std::vector<double> x{.5};const double q=std::exp(705.);
    const auto f=cppunivariate::genbeta_mle({x.data(),x.size()},1,1e-7,200,2,q);
    std::cout<<"genbeta_large "<<int(f.status)<<" "<<2<<" "<<q<<" "<<f.shape3<<" "<<f.logLik<<"\n";
  }
  for(double q:{1e-100,1e-300}) {
    const std::vector<double> x{.02,.2,.6,.9};
    const auto f=cppunivariate::genbeta_mle({x.data(),x.size()},1,1e-7,1000,.5,q);
    std::cout<<"genbeta "<<int(f.status)<<" "<<.5<<" "<<q<<" "<<f.shape3<<" "<<f.logLik<<"\n";
  }
  const std::vector<double> near{1,1+2.220446049250313e-16,1+4.440892098500626e-16,1+6.661338147750939e-16,1+8.881784197001252e-16};
  const auto f=cppunivariate::fatigue_mle({near.data(),near.size()},1e-4,100);
  std::cout<<"near "<<int(f.status)<<" "<<f.alpha<<" "<<f.beta<<" "<<f.logLik<<"\n";
  {
    const std::vector<double> one{1},two{1,2};
    const auto a=cppunivariate::fatigue_mle_shifted({two.data(),two.size()},-1e300,1e-10,200);
    const auto b=cppunivariate::fatigue_mle_shifted({one.data(),one.size()},.1,1e-10,200,std::numeric_limits<double>::quiet_NaN(),.9);
    const auto c=cppunivariate::fatigue_mle_shifted({one.data(),one.size()},0,1e-10,200,std::numeric_limits<double>::quiet_NaN(),1);
    std::cout<<"translation "<<int(a.status)<<" "<<int(b.status)<<" "<<int(c.status)<<"\n";
  }

}
