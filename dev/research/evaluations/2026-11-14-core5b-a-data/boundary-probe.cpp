#include "cppunivariate/distributions/pareto2/mle.hpp"
#include "cppunivariate/distributions/fpareto/mle.hpp"
#include "cppunivariate/distributions/gev/mle.hpp"
#include <iostream>
#include <iomanip>
#include <vector>
#include <string>
void row(const std::string& name,cppunivariate::FitOutcome outcome,const std::vector<double>& values) {
  std::cout<<"{\"name\":\""<<name<<"\",\"kind\":\""<<cppunivariate::outcome_kind_name(outcome.kind)<<"\",\"reason\":\""<<cppunivariate::outcome_reason_name(outcome.reason)<<"\",\"values\":[";
  for(std::size_t i=0;i<values.size();++i)std::cout<<(i?",":"")<<std::setprecision(17)<<values[i];
  std::cout<<"]}\n";
}
int main() {
  const std::vector<double> zeros{0,0,0},mixed{0,1},critical{0,0,0,0,1,3},constant{2,2};
  auto fit=cppunivariate::pareto2_mle({zeros.data(),zeros.size()},0,1e-10,1000,2,3);
  row("held-zero",cppunivariate::outcome(fit),{fit.shape,fit.scale,fit.logLik});
  fit=cppunivariate::pareto2_mle({mixed.data(),mixed.size()},0,1e-10,1000,2,NAN);
  row("scale-root",cppunivariate::outcome(fit),{fit.shape,fit.scale,fit.logLik});
  fit=cppunivariate::pareto2_mle({mixed.data(),mixed.size()},0,1e-10,1000,NAN,1);
  row("shape-root",cppunivariate::outcome(fit),{fit.shape,fit.scale,fit.logLik});
  fit=cppunivariate::pareto2_mle({mixed.data(),mixed.size()},0,1e-10,1000,1,NAN);
  row("pareto-critical",cppunivariate::outcome(fit),{fit.stop_detail.loglik_lower,fit.stop_detail.loglik_upper});
  for(bool held:{false,true}) {
    const auto gev=cppunivariate::gev_mle({critical.data(),critical.size()},1e-10,100,.5,held?0:NAN,NAN);
    row(held?"gev-held-critical":"gev-free-critical",cppunivariate::outcome(gev),{gev.stop_detail.loglik_lower,gev.stop_detail.loglik_upper});
  }
  const auto fp=cppunivariate::fpareto_mle({constant.data(),constant.size()},0,1e-10,1000,.3,NAN,.4,NAN);
  row("fpareto-constant",cppunivariate::outcome(fp),{});
}
