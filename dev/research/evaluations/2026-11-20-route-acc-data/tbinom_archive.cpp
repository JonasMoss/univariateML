#include "cppunivariate/distributions/binom/tbinom_mle.hpp"
#include <iomanip>
#include <iostream>

int main() {
  const std::vector<int> x{1000000,1000001,1000003,1000008,1000011};
  const cppunivariate::ispan sample{x.data(),x.size()};
  std::cout<<std::setprecision(17);
  for(double a:{999999.,0.}) {
    const auto proposal=cppunivariate::tbinom_detail::tbinom_design_proposal(sample,a,1000011,NAN);
    const auto result=cppunivariate::tbinom_design_mle(sample,a,1000011,NAN,NAN,1e-10,512);
    for(const auto& fit:{proposal,result}) {
      const auto decision=cppunivariate::outcome(fit);
      std::cout<<"a="<<a<<" status="<<int(fit.status)<<" certified="<<decision.certified
        <<" size="<<fit.size<<" p="<<fit.prob<<" logLik="<<fit.logLik
        <<" face="<<fit.stop_detail.limit_loglik<<" upper="<<fit.stop_detail.loglik_upper<<'\n';
    }
    if(a!=999999 || proposal.stop_detail.limit_coef.empty())continue;
    cppunivariate::binomial_certificate_detail::Work work{0,0,512};
    bool exhausted=false;
    const auto cutoff=cppunivariate::binomial_certificate_detail::signed_cutoff(sample,1000000,1000011,
      proposal.stop_detail.limit_coef[0],work,exhausted);
    std::size_t visits=0;
    if(std::isfinite(cutoff)) {
      const auto coverage=cppunivariate::binomial_certificate_detail::cover_face(sample,1000011,cutoff,
        proposal.stop_detail.limit_loglik,work,[&](double size) {
          ++visits;
          const auto fit=cppunivariate::tbinom_detail::profile(sample,1000000,1000011,size);
          return fit.status==cppunivariate::truncated_detail::Status::Ok?fit.logLik:NAN;
        });
      std::cout<<"cutoff="<<cutoff<<" coverage status="<<int(coverage)<<" profiles="<<visits
        <<" enumeration cells="<<work.cells<<" arithmetic failure="<<work.numerical<<'\n';
    }
  }
}
