#include <fstream>
#include <iostream>
#include <iomanip>
#include "bindings/common/distributions/sged/fit.hpp"
#include "bindings/common/distributions/sstd/fit.hpp"
#include "bindings/common/distributions/snorm/fit.hpp"
#include "cppunivariate/distributions/sged/dpqr.hpp"
#include "cppunivariate/distributions/sstd/dpqr.hpp"
int main(int argc,char** argv) {
 std::ifstream in(argv[1]);std::cout<<std::setprecision(17);
 int index,n;std::string family;
 while(in>>index>>family>>n){
  std::vector<umvl_binding::Held> h(family=="snorm"?3:4);
  for(auto& v:h){std::string t;in>>t;double q=std::stod(t);if(!std::isnan(q))v={true,q,1};}
  std::vector<double> x(n);for(auto& v:x)in>>v;
  umvl_binding::FitResult r;
  if(family=="sged")r=umvl_binding::fit_sged({x.data(),x.size()},h[0],h[1],h[2],h[3],1e-10);
  if(family=="sstd")r=umvl_binding::fit_sstd({x.data(),x.size()},h[0],h[1],h[2],h[3],1e-10);
  if(family=="snorm")r=umvl_binding::fit_snorm({x.data(),x.size()},h[0],h[1],h[2],200);
  auto number=[](double v){if(std::isfinite(v))std::cout<<v;else std::cout<<"null";};
  std::cout<<"{\"index\":"<<index<<",\"error\":"<<std::quoted(r.error)<<",\"parameters\":[";
  std::vector<double> p;for(auto v:r.estimates){if(!p.empty())std::cout<<",";p.push_back(v.second);number(v.second);}
  std::cout<<"],\"logLik\":";number(r.logLik);std::cout<<",\"certified\":";double cert=NAN;for(auto v:r.extra)if(v.first=="certified")cert=v.second;number(cert);
  double sum=0,correction=0;for(double v:x)if(p.size()==h.size()){
   double ld=family=="sged"?cppunivariate::distributions::dsged(v,p[0],p[1],p[2],p[3],true):family=="sstd"?cppunivariate::distributions::dsstd(v,p[0],p[1],p[2],p[3],true):cppunivariate::distributions::dsnorm(v,p[0],p[1],p[2],true);
   double y=ld-correction,t=sum+y;correction=(t-sum)-y;sum=t;
  }
  std::cout<<",\"dpqr\":";number(p.size()==h.size()?sum:NAN);std::cout<<"}"<<std::endl;
 }
}
