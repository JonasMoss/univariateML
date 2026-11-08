#include "cppunivariate/distributions/betapr/mle.hpp"
#include <fstream>
#include <iostream>
#include <iomanip>
int main(){
 std::ifstream in("dev/research/evaluations/2026-11-08-betapr-joint-data/r-samples.txt");
 std::string label;std::size_t n;
 while(in>>label>>n){std::vector<double> x(n);for(double& v:x)in>>v;
  const auto r=cppunivariate::betapr_mle(cppunivariate::span{x.data(),x.size()});
  std::cout<<std::setprecision(17)<<label<<" "<<int(r.status)<<" "<<int(r.certificate_failure)<<" "<<r.iterations<<" "<<r.joint_exterior.cover_cells<<" "<<r.alpha<<" "<<r.beta<<" "<<r.scale<<" "<<r.logLik<<"\n";
 }
}
