#define DOCTEST_CONFIG_DISABLE
#include "test_support.hpp"
#include "cppunivariate/distributions/betapr/mle.hpp"
#include "cppunivariate/distributions/betapr/oracle-fixtures.inc"
#include <iostream>
int main(){
 auto fixtures=betapr_oracle_fixtures();auto golden=mltest::load_distribution_golden("betapr");
 fixtures.push_back({golden.data,golden.logLik,cppunivariate::BetaPrimeStatus::Ok});fixtures.push_back({{1,2,3,4},0,cppunivariate::BetaPrimeStatus::GammaBoundary});
 int i=0;for(const auto& f:fixtures){auto r=cppunivariate::betapr_mle(mltest::as_span(f.x));const auto& b=r.joint_exterior;
 std::cout<<std::setprecision(15)<<i++<<" status "<<int(r.status)<<" expected "<<int(f.status)<<" fail "<<int(r.certificate_failure)<<" eval "<<r.iterations<<" ext "<<int(b.status)<<" upper "<<r.stop_detail.loglik_upper<<" tie "<<r.stop_detail.comparison_tie<<" cut "<<b.gamma_shape2<<" "<<b.invgamma_shape1<<" signed "<<b.gamma_signed_shape2<<" "<<b.invgamma_signed_shape1<<" cells "<<b.cover_cells<<" pts "<<b.profile_evaluations<<" patches "<<b.retained_maxima<<" nCandidates "<<r.stop_detail.candidates.size()<<"\n";
 for(const auto& c:r.stop_detail.candidates){std::cout<<"  "<<int(c.model)<<" "<<c.loglik_lower<<" "<<c.loglik_upper;for(double v:c.coef)std::cout<<" "<<v;std::cout<<"\n";}
 }
}
