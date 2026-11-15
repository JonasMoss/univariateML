#include "cppunivariate/distributions/gamma/mle.hpp"
#include "cppunivariate/distributions/pareto/censored.hpp"
#include "cppunivariate/distributions/norm/tnorm_mle.hpp"
#include "bindings/common/modifiers.hpp"
#include "bindings/common/distributions/gamma/fit.hpp"
#include <vector>
#include <cassert>
#include <iostream>
int main() {
  const std::vector<double> x{1e308,1e308};
  const auto g=cppunivariate::gamma_mle({x.data(),x.size()},1e-10,100,1e308,NAN);
  assert(g.status==cppunivariate::GammaStatus::IterationFailure);
  assert(g.evaluation_failure);
  assert(cppunivariate::outcome(g).reason==cppunivariate::OutcomeReason::Evaluation);
  const std::vector<double> low{2,1},up{2,INFINITY};
  const auto p=cppunivariate::pareto_censored_mle({{low.data(),low.size()},{up.data(),up.size()}},NAN,2);
  assert(p.status==cppunivariate::ParetoCensoredStatus::InvalidInput);
  assert(p.unbounded_after_support_clamp);
  assert(cppunivariate::outcome(p).kind==cppunivariate::OutcomeKind::Nonexistence);
  assert(cppunivariate::outcome(p).reason==cppunivariate::OutcomeReason::Unbounded);
  const std::vector<double> constant{1,1,1};
  for(int design=0;design<3;++design) {
    const auto t=cppunivariate::tnorm_mle({constant.data(),constant.size()},design==1?1:NAN,design==2?1:NAN,1e-10,100,0,.5);
    assert(t.estimated_bounds_collapse);
    assert(cppunivariate::outcome(t).kind==cppunivariate::OutcomeKind::Nonexistence);
    assert(cppunivariate::outcome(t).reason==cppunivariate::OutcomeReason::Unbounded);
  }
  const std::vector<std::pair<std::string,umvl_binding::Held>> normal_holds{{"mean",{true,0}},{"sd",{true,.5}}};
  for(int design=0;design<3;++design) {
    const auto result=umvl_binding::fit_modifier("norm",{constant.data(),constant.size()},normal_holds,"none",true,design==1?1:NAN,design==2?1:NAN);
    assert(result.outcome.kind==cppunivariate::OutcomeKind::Nonexistence);
    assert(result.outcome.reason==cppunivariate::OutcomeReason::Unbounded);
  }
  const std::vector<double> right{INFINITY,INFINITY},below{.25,.5};
  const auto limit=umvl_binding::fit_censored("weibull",{{below.data(),below.size()},{right.data(),right.size()}},{{"scale",{true,1}}});
  assert(limit.outcome.kind==cppunivariate::OutcomeKind::Limit);
  assert(limit.model=="unavailable:point-mass");
  assert(limit.limit_estimates.size()==1 && limit.limit_estimates[0].first=="point" && limit.limit_estimates[0].second==1);
  assert(limit.limit_loglik==0);
  const auto gamma_fit=umvl_binding::fit_gamma({x.data(),x.size()},{true,1e308});
  assert(gamma_fit.outcome.reason==cppunivariate::OutcomeReason::Evaluation);
  const std::vector<double> counts{2,4,3};
  const auto prepared=cppunivariate::count_observations({counts.data(),counts.size()},true);
  assert(prepared.status==cppunivariate::CountInputStatus::Ok && prepared.lower==2 && prepared.upper==4);
  const std::vector<int> iz{0};
  const auto b=cppunivariate::binom_mle({iz.data(),iz.size()},1e308,1e-10,200,.9);
  const auto zi=cppunivariate::zib_mle({iz.data(),iz.size()},1e-10,200,1e308,.9,0);
  assert(cppunivariate::outcome(b).reason==cppunivariate::OutcomeReason::Evaluation);
  std::cerr<<"ZI: status="<<int(zi.status)<<" eval="<<zi.evaluation_failure<<" LL="<<zi.logLik<<" N="<<zi.size<<" p="<<zi.prob<<" pi="<<zi.pi<<" upper="<<zi.stop_detail.loglik_upper<<" reason="<<int(cppunivariate::outcome(zi).reason)<<"\n";
  assert(cppunivariate::outcome(zi).reason==cppunivariate::OutcomeReason::Evaluation && zi.stop_detail.loglik_upper==INFINITY);
  const std::vector<double> dz{0};
  const auto bw=umvl_binding::fit_modifier("binom",{dz.data(),dz.size()},{{"size",{true,1e308}},{"prob",{true,.9}}},"none",true,0,INFINITY);
  assert(bw.outcome.kind==cppunivariate::OutcomeKind::Failure && bw.outcome.reason==cppunivariate::OutcomeReason::Evaluation);
  const std::vector<double> zz{0,0};
  const auto pw=umvl_binding::fit_modifier("pois",{zz.data(),zz.size()},{{"lambda",{true,1e308}}},"none",true,0,INFINITY);
  assert(pw.outcome.kind==cppunivariate::OutcomeKind::Failure && pw.outcome.reason==cppunivariate::OutcomeReason::Evaluation);
  std::cout<<"Gamma overflow: Evaluation; Pareto collapse: Nonexistence/Unbounded; estimated normal window collapse with held parent: Unbounded; held-normal and Weibull limit binding seams checked; held-count likelihood range failures checked; raw compatibility statuses preserved.\n";
}
