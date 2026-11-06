#include "cppunivariate/distributions/paralogis/mle.hpp"
#include "cppunivariate/distributions/paralogis/dpqr.hpp"
#include <iostream>
#include <iomanip>
int main() {
  const std::vector<double> x{std::exp(-.001)};
  const cppunivariate::span data{x.data(),x.size()};
  const auto proposal=cppunivariate::paralogis_shape_proposal(data,1,1e-10,1000);
  const double witness=cppunivariate::distributions::dparalogis(x[0],9000,1,true);
  const auto certificate=cppunivariate::paralogis_cert_detail::certify(data,proposal.shape,1,true);
  const auto result=cppunivariate::paralogis_mle_fixed_scale(data,1,1e-10,1000);
  std::cout<<std::setprecision(17)<<"x\tproposal_shape\tproposal_logLik\twitness_shape\twitness_logLik\tcertificate_ok\tcertificate_budget\tboxes\tpublic_status\tpublic_logLik\n"
    <<x[0]<<'\t'<<proposal.shape<<'\t'<<proposal.logLik<<"\t9000\t"<<witness<<'\t'<<certificate.ok<<'\t'<<certificate.budget<<'\t'<<certificate.boxes<<'\t'<<int(result.status)<<'\t'<<result.logLik<<'\n';
  if(!(proposal.logLik<witness-1))return 2;
  if(result.status==cppunivariate::ParalogisStatus::Ok)return result.logLik>=witness-1e-8?0:3;
  return result.status==cppunivariate::ParalogisStatus::NumericalFailure||result.status==cppunivariate::ParalogisStatus::CertificateBudget?0:4;
}
