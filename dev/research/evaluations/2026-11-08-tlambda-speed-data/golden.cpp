#include "proto.hpp"
#include <cstdio>
#include <fstream>
#include <sstream>
#include <string>
int main(int argc,char**argv){
  std::ifstream f(argc>1?argv[1]:"golden_data.txt"); std::vector<double> x; double v; while(f>>v) x.push_back(v);
  cppunivariate::span sp{x.data(),x.size()};
  auto L=cppunivariate::tlambda_mle(sp,1e-7,1000);
  auto P=proto::fit(x);
  std::printf("legacy %.17g %.17g %.17g %.17g\nproto  %.17g %.17g %.17g %.17g passes=%ld\n",L.mu,L.sigma,L.lambda,L.logLik,P.mu,P.sigma,P.lambda,cppunivariate::tlambda_detail::likelihood(sp,P.mu,P.sigma,P.lambda),P.passes);
}
