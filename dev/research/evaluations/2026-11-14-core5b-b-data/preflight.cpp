#include "cppunivariate/distributions/beta/mle.hpp"
#include "cppunivariate/distributions/bnbinom/mle.hpp"
#include "cppunivariate/distributions/frechet/mle.hpp"
#include "cppunivariate/distributions/fatigue/mle.hpp"
#include "cppunivariate/distributions/pareto3/mle.hpp"
#include "cppunivariate/distributions/hcauchy/mle.hpp"
#include "cppunivariate/distributions/sn/mle.hpp"
#include "cppunivariate/distributions/st/mle.hpp"
#include "cppunivariate/distributions/trgamma/mle.hpp"
#include "bindings/common/distributions/cmb/fit.hpp"
#include "bindings/common/distributions/bnbinom/fit.hpp"
#include "bindings/common/distributions/st/fit.hpp"
#include "bindings/common/distributions/frechet/fit.hpp"
#include "bindings/common/distributions/genbeta/fit.hpp"
#include "bindings/common/distributions/power/fit.hpp"
#include <cstdio>
#include <vector>
#define VERIFY(test) do { if(!(test)){std::fprintf(stderr,"line %d: %s\n",__LINE__,#test);return 1;} } while(false)
int main() {
  const std::vector<double> x{.5,.25,.125},one{2},support{1,2};
  const cppunivariate::span sx{x.data(),x.size()},so{one.data(),one.size()},ss{support.data(),support.size()};
  VERIFY(cppunivariate::beta_mle(sx,1e-10,100,0,NAN).status==cppunivariate::BetaStatus::InvalidInput);
  const auto ridge=cppunivariate::genbeta_mle(sx,1,1e-10,300,NAN,1,NAN);
  VERIFY(ridge.product_ridge && cppunivariate::outcome(ridge).certified && !cppunivariate::outcome(ridge).unique);
  VERIFY(std::abs(ridge.shape1*ridge.shape3/.72134752044448170368-1)<1e-13);
  const auto gb=umvl_binding::render_fit(umvl_binding::fit_genbeta(sx,{}, {true,1},{},{true,1}));
  VERIFY(gb.error.empty() && gb.outcome.kind==cppunivariate::OutcomeKind::Estimate && !gb.outcome.unique);
  VERIFY(cppunivariate::outcome(cppunivariate::sn_mle(so)).reason==cppunivariate::OutcomeReason::Unbounded);
  VERIFY(cppunivariate::outcome(cppunivariate::trgamma_mle(so)).reason==cppunivariate::OutcomeReason::Unbounded);
  VERIFY(cppunivariate::outcome(cppunivariate::frechet_mle(so)).reason==cppunivariate::OutcomeReason::Unbounded);
  const auto held_location=umvl_binding::fit_frechet(so,{}, {true,0},{});
  VERIFY(held_location.outcome.kind==cppunivariate::OutcomeKind::Nonexistence && held_location.error.find("nonconstant")!=std::string::npos);
  const auto flat=cppunivariate::st_mle(so,1e-10,200,2,1,NAN,.5);
  VERIFY(cppunivariate::outcome(flat).certified && !cppunivariate::outcome(flat).unique);
  const auto outside=umvl_binding::fit_power(ss,{true,1},{});
  VERIFY(outside.outcome.reason==cppunivariate::OutcomeReason::InvalidInput && outside.error.find("zero likelihood")!=std::string::npos);
  for(const std::vector<int>& binary : {std::vector<int>{0,1},std::vector<int>{0,0,1}}) {
    const cppunivariate::ispan sb{binary.data(),binary.size()};
    const auto c=umvl_binding::render_fit(umvl_binding::fit_cmb(sb));
    VERIFY(c.error.empty() && c.outcome.certified && !c.outcome.unique);
    VERIFY(c.estimates.size()==3 && c.estimates[0].second==1);
    for(const auto& p:c.estimates)VERIFY(std::isfinite(p.second));
    VERIFY(c.estimates[2].second==1);
  }
  const std::vector<double> distinct{.1,.2},tiny_one{.1},wide{-1e308,0,1e308};
  const cppunivariate::span sd{distinct.data(),distinct.size()},stiny{tiny_one.data(),tiny_one.size()},sw{wide.data(),wide.size()};
  VERIFY(cppunivariate::outcome(cppunivariate::pareto3_mle(sd,-1e16,1e-10,1000)).reason==cppunivariate::OutcomeReason::Evaluation);
  VERIFY(cppunivariate::outcome(cppunivariate::pareto3_mle(stiny,-1e16,1e-10,1000,NAN,1e16)).reason==cppunivariate::OutcomeReason::Evaluation);
  VERIFY(cppunivariate::outcome(cppunivariate::frechet_mle(sd,1e-10,100,-1e16)).reason==cppunivariate::OutcomeReason::Evaluation);
  VERIFY(cppunivariate::outcome(cppunivariate::frechet_mle(stiny,1e-10,100,-1e16,NAN,1e16)).reason==cppunivariate::OutcomeReason::Evaluation);
  VERIFY(cppunivariate::outcome(cppunivariate::fatigue_mle_shifted(sd,-1e300,1e-10,200)).reason==cppunivariate::OutcomeReason::Evaluation);
  VERIFY(cppunivariate::outcome(cppunivariate::hcauchy_mle(sw,-1.5e308)).reason==cppunivariate::OutcomeReason::Evaluation);
  VERIFY(cppunivariate::outcome(cppunivariate::st_mle(sw,1e-10,200,NAN,NAN,1,4)).reason==cppunivariate::OutcomeReason::Evaluation);
  const std::vector<double> near_exponent{0,0,1,2,3};
  const auto ne=cppunivariate::st_mle(cppunivariate::span{near_exponent.data(),near_exponent.size()},1e-8,200,0,NAN,NAN,2./3.);
  VERIFY(cppunivariate::outcome(ne).reason==cppunivariate::OutcomeReason::Unbounded);
  const std::vector<double> lnx{4.497141406509849,4.497141451481264,4.497141496452679,4.497141541424094,4.497141631366924};
  const auto ln=cppunivariate::trgamma_mle(cppunivariate::span{lnx.data(),lnx.size()});
  VERIFY(ln.stop_detail.limit_model==cppunivariate::StopModel::Lnorm);
  VERIFY(std::abs(ln.stop_detail.limit_loglik-74.77852803231016)<1e-10);
  const std::vector<int> golden{17,1,2,0,4,13,1,11,5,1,0,0,4,2,1,3,1,1,3,0,2,13,2,0,2,2,0,1,4,2,0,8,1,14,0,4,2,0,2,0,11,5,1,31,0,0,1,22,0,3,5,5,11,11,0,2,0,11,1,0,0,1,1,9,0,0,8,5,5,5,0,2,5,3,3,2,3,0,0,11,3,125,0,0,4,1,47,1,1,4,3,6,3,9,4,6,0,1,2,3,0,0,0,5,1,0,35,64,0,5,4,4,0,2,5,1,1,16,1,2,4,1,5,35,18,0,8,0,2,1,0,4,5,16,6,5,2,4,0,0,3,2,7,1,20,2,7,7,3,0,0,0,2,8,3,5,1,1,0,4,175,20,0,8,0,2,1,0,9,1,1,5,3,4,0,0,1,0,3,8,0,3,0,64,1,29,0,1,3,14,1,0,3,1,5,0,0,7,0,3,0,8,0,6,0,5,0,7,2,2,6,4,0,1,0,0,2,1,3,0,1,6,4,2,3,0,1,1,2,4,4,0,1,0,0,1,7,12,4,0,5,0,7,5,2,0,5,2,8,0,9,5,3,0,8,0,6,1,2,36,4,2,1,0,0,1,0,4,48,36,0,3,4,1,1,5,0,6,22,5,3,2,12,0,0,14,2,3,0,5,7,1,0,2,0,16,4,4,0,2,4,0,0,1,15,6,0,5,12,23,2,10,5,9,2,9,0,0,4,2,0,0,5,0,2,1,13,2,1,3,3,17,0,5,9,0,5,4,1,7,10,0,13,6,1,5,7,1,0,1,0,0,12,2,5,2,1,0,0,0,0,4,1,1,7,1,3,1,3,0,1,6,8,0,0,7,0,4,0,5,1,6,0,3,2,1,0,0,11,6,1,0,5,11,12,42,13,0,51,6,1,1,3,0,0,1,4,3,0,1,28,8,0,18,0,3,7,3,1,0,0,10,2,8,1,46,2,2,21,6,2,1,0,6,6,0,2,4,1,1,0,3,8,1,0,3,0,2,4,1,0,1,1,0,1,0,3,2,4,0,0,2,0,2,2,0,0,2,11,22,0,1,1,5,1,6,2,0,8,13,10,4,5,3,0,0,0,5,12,5,3,1,2,7,4,1,1,2,1,2};
  const auto bn=cppunivariate::bnbinom_mle(cppunivariate::ispan{golden.data(),golden.size()});
  VERIFY(bn.status==cppunivariate::BnbinomStatus::Ok && bn.certified);
  VERIFY(bn.swap_symmetry && !cppunivariate::outcome(bn).unique);
  const auto bnrender=umvl_binding::render_fit(umvl_binding::fit_bnbinom(cppunivariate::ispan{golden.data(),golden.size()}));
  VERIFY(!bnrender.outcome.unique && bnrender.candidates.size()==2);
  const std::vector<double> endpoint{0,1,2};
  const auto ep=umvl_binding::render_fit(umvl_binding::fit_st(cppunivariate::span{endpoint.data(),endpoint.size()},{true,0},{true,1},{},{true,4}));
  VERIFY(ep.outcome.kind==cppunivariate::OutcomeKind::Nonexistence && std::isfinite(ep.limit_loglik));
  const auto held=cppunivariate::bnbinom_mle(cppunivariate::ispan{golden.data(),golden.size()},1e-10,600,bn.size,bn.alpha,bn.beta);
  VERIFY(cppunivariate::outcome(held).certified && cppunivariate::outcome(held).unique);
  std::puts("core5b-b outcome preflight passed");
}
