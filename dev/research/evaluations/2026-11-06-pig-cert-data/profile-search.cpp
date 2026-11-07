// Diagnostic search for competing held-mean shape peaks; not a certificate.
#include "cppunivariate/distributions/pig/mle.hpp"
#include <iostream>
#include <iomanip>
int main() {
 int cases=0, multiple=0;
 for(int k : {2,3,4,5,10,20,50}) for(int zeros : {0,1,2,5,20,100})
 for(double multiplier : {.02,.1,.5,1.,2.,10.,100.}) {
  std::vector<int> x(zeros,0);x.push_back(k);
  cppunivariate::pig_detail::Counts data({x.data(),x.size()});
  const double mu=data.average*multiplier;
  double before=data.likelihood(mu,mu*mu*std::exp(-20));
  double current=data.likelihood(mu,mu*mu*std::exp(-19.75));
  int peaks=0;
  for(int j=2;j<=160;++j) {
   double next=data.likelihood(mu,mu*mu*std::exp(-20+.25*j));
   if(current>before+1e-9 && current>next+1e-9)++peaks;
   before=current;current=next;
  }
  if(peaks>1) {
   ++multiple;
   std::cout<<"multiple,"<<k<<","<<zeros<<","<<std::setprecision(17)<<mu<<","<<peaks<<"\n";
  }
  ++cases;
 }
 std::cout<<"cases,"<<cases<<",multiple,"<<multiple<<"\n";
}
