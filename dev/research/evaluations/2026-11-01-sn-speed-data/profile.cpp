#include "old-profile.hpp"
#include "new-profile.hpp"
#include "../../../../univariateML-cpp/tests/json_loader.hpp"
#include <iostream>
int main() {
 std::cerr<<"version,mask,alpha,iterations,objectives,phi,tail_recurrences\n";
 const auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/sn/golden.json");
 const cppunivariate::span x{g.data.data(),g.data.size()};
 const double nan=cppunivariate::special::kNaN;
 std::cout<<"version,mask,inner,iterations,objectives,phi,tail_recurrences,score_mills,face_observations\n";
 for(int mask=0;mask<8;++mask) {
  cppunivariate::sn_baseline_detail::profile_mask=mask; cppunivariate::sn_detail::profile_mask=mask;
  double c=mask&1?g.estimates.at("xi"):nan,w=mask&2?g.estimates.at("omega"):nan,a=mask&4?g.estimates.at("alpha"):nan;
  cppunivariate::sn_baseline_detail::counts={};cppunivariate::sn_detail::counts={};
  cppunivariate::sn_baseline_mle(x,1e-9,100,c,w,a);cppunivariate::sn_mle(x,1e-9,100,c,w,a);
  auto print=[&](const char* version,const auto& v) {std::cout<<version<<','<<mask<<','<<v.inner<<','<<v.iterations<<','<<v.objectives<<','<<v.phi<<','<<v.tails<<','<<v.score_mills<<','<<v.face_observations<<'\n';};
  print("old",cppunivariate::sn_baseline_detail::counts);print("new",cppunivariate::sn_detail::counts);
 }
}
