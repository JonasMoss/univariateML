#define DOCTEST_CONFIG_DISABLE
#include "test_support.hpp"
#include "cppunivariate/distributions/betapr/mle.hpp"
#include <iostream>
int main(){
 const auto golden=mltest::load_distribution_golden("betapr");
 for(const auto& x:std::vector<std::vector<double>>{{.03,.1,.4,.7,1,2,4,12},golden.data}){
 const auto r=cppunivariate::betapr_mle(mltest::as_span(x));const auto& b=r.joint_exterior;
 std::cout<<std::setprecision(17)<<x.size()<<" "<<int(r.status)<<" "<<int(b.status)<<" "<<b.witness_lower<<" "<<b.harmonic_lower<<" "<<b.harmonic_upper<<" "<<b.gamma_upper<<" "<<b.invgamma_upper<<" "<<b.gamma_shape2<<" "<<b.invgamma_shape1<<" "<<b.evaluations<<" "<<b.gamma_signed_shape2<<" "<<b.invgamma_signed_shape1<<"\n";
 }
 const std::vector<double> ordinary{1,2,3,4};
 const auto b=cppunivariate::betapr_joint_exterior_bounds(mltest::as_span(ordinary),1,1,2);
 std::cout<<std::setprecision(17)<<ordinary.size()<<" -1 "<<int(b.status)<<" "<<b.witness_lower<<" "<<b.harmonic_lower<<" "<<b.harmonic_upper<<" "<<b.gamma_upper<<" "<<b.invgamma_upper<<" "<<b.gamma_shape2<<" "<<b.invgamma_shape1<<" "<<b.evaluations<<" "<<b.gamma_signed_shape2<<" "<<b.invgamma_signed_shape1<<"\n";
}
