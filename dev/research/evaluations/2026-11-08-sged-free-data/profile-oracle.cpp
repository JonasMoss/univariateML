#include "cppunivariate/distributions/sged/mle.hpp"
#include <iostream>
#include <iomanip>
int main(){std::cout<<std::setprecision(17);std::vector<double> golden;double input;while(std::cin>>input)golden.push_back(input);
 const std::vector<double> small{-1,-.55,-.1,.03,.2,.6,1};
 std::cout<<"DATA small "<<small.size();for(double v:small)std::cout<<' '<<v;std::cout<<'\n';
 const cppunivariate::span y{small.data(),small.size()};
 for(double xi:{.3,1.4,3.})for(double p:{1.15,1.7,3.,8.,40.}) {
  const double q=p+.01;const cppunivariate::sged_weighted_shape::Weights w(xi);const auto b=cppunivariate::sged_weighted_shape::box(y,p,q,w,200);
  std::cout<<"W "<<xi<<' '<<p<<' '<<q<<' '<<b.valid<<' '<<b.mode.lo<<' '<<b.mode.hi<<' '<<b.score.lo<<' '<<b.score.hi<<' '<<b.curvature.lo<<' '<<b.curvature.hi<<' '<<b.coefficients.valid<<' '<<b.coefficients.mean.lo<<' '<<b.coefficients.mean.hi<<' '<<b.coefficients.logsd.lo<<' '<<b.coefficients.logsd.hi<<'\n';
 }
 for(double p:{1.15,1.7,2.5,4.,8.})for(std::size_t j=1;j<small.size();++j) {
  const double lo=small[j-1],hi=small[j];const auto b=cppunivariate::sged_mode_skew::box(y,lo,hi,p);
  std::cout<<"M "<<p<<' '<<lo<<' '<<hi<<' '<<b.valid<<' '<<b.score.lo<<' '<<b.score.hi<<' '<<b.curvature.lo<<' '<<b.curvature.hi<<' '<<b.upper<<'\n';
 }
 const cppunivariate::span x{golden.data(),golden.size()};
 for(double xi:{.3,.7,1.4,3.}) {
  const auto r=cppunivariate::sged_mle(x,1e-10,200,NAN,NAN,NAN,xi);
  std::cout<<"FW "<<xi<<' '<<int(r.status)<<' '<<r.certified<<' '<<r.mu<<' '<<r.sigma<<' '<<r.nu<<' '<<r.xi<<' '<<r.logLik<<' '<<r.iterations<<'\n';
 }
 for(double p:{1.15,1.7,2.5,4.,8.}) {
  const auto r=cppunivariate::sged_mle(x,1e-10,200,NAN,NAN,p);
  std::cout<<"FM "<<p<<' '<<int(r.status)<<' '<<r.certified<<' '<<r.mu<<' '<<r.sigma<<' '<<r.nu<<' '<<r.xi<<' '<<r.logLik<<' '<<r.iterations<<'\n';
 }
}
