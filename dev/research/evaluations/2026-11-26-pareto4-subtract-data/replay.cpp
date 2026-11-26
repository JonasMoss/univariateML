#include <cppunivariate/distributions/pareto4/mle.hpp>
#include <cppunivariate/distributions/pareto3/mle.hpp>
#include <cstdio>
int main() {
  const double u=0x1p54;
  for(auto x : {std::array<double,2>{-5,10}, {5,9}, {1,5}, {-1,1}}) {
    cppunivariate::span data{x.data(),x.size()};
    for(int mask=0;mask<8;++mask) {
      auto r=cppunivariate::pareto4_mle(data,-u,1e-10,1000,mask&1?u:NAN,mask&2?1:NAN,mask&4?u/4:NAN);
      std::printf("IV %g %g mask %d status %d cert %d %.17g %.17g %.17g face %.17g\n",x[0],x[1],mask,int(r.status),r.certified,r.shape1,r.shape2/u,r.scale,r.stop_detail.limit_coef.empty()?NAN:r.stop_detail.limit_coef[0]);
    }
    for(int mask=0;mask<4;++mask) {
      auto r=cppunivariate::pareto3_mle(data,-u,1e-10,1000,mask&1?u/4:NAN,mask&2?u:NAN);
      std::printf("III %g %g mask %d status %d cert %d %.17g %.17g\n",x[0],x[1],mask,int(r.status),r.certified,r.shape/u,r.scale);
    }
  }
}
