// Zero-modified likelihood = Bernoulli zero indicator + positive conditional law.
#pragma once
#include "work_budget.hpp"
#include "discrete_stop.hpp"
#include "../distributions/pois/ztpois_mle.hpp"
#include "../distributions/geom/ztgeom_mle.hpp"
#include "../distributions/nbinom/ztnbinom_mle.hpp"
#include "../distributions/binom/ztbinom_mle.hpp"
namespace cppunivariate {
namespace zero_detail {
enum class ModifiedStatus { Ok, InvalidInput, InvalidSize, PoissonLimit, AllZero, TwoPoint, IterationFailure };
struct ModifiedResult { double lambda, size, prob, p0, logLik; int iterations; ModifiedStatus status; StopDetail stop_detail{}; };
// kind: Poisson, geometric, NB, binomial, logarithmic.
inline ModifiedResult modified_mle(ispan x, int kind, double tol, int maxit, double fixed_size) {
  maxit = static_cast<int>(solver_budget(maxit));
  ModifiedResult out{1,1,.5,0,0,0,ModifiedStatus::Ok};
  const bool fixed=!std::isnan(fixed_size);
  if(fixed && (!(fixed_size>=(kind==3?1:0)) || !std::isfinite(fixed_size) ||
      (kind==3 && fixed_size!=std::floor(fixed_size)))) {
    out.status=ModifiedStatus::InvalidSize;return out;
  }
  if(fixed)out.size=fixed_size;
  if(x.size()==0 || !(tol>0)||!std::isfinite(tol)||maxit<1) {out.status=ModifiedStatus::InvalidInput;return out;}
  std::vector<int> pos;
  for(int v:x) {
    if(v<0) {out.status=ModifiedStatus::InvalidInput;return out;}
    if(v>0)pos.push_back(v);
    if(fixed && kind==3 && v>fixed_size) {out.status=ModifiedStatus::InvalidSize;return out;}
  }
  const double n=x.size(), k=pos.size();
  out.p0=(n-k)/n;
  if(pos.empty()) {out.status=ModifiedStatus::AllZero;return out;}
  const ispan positive{pos.data(),pos.size()};
  if(kind==0) {const auto r=ztpois_mle(positive);out.lambda=r.lambda;out.logLik=r.logLik;}
  if(kind==1) {const auto r=ztgeom_mle(positive);out.prob=r.prob;out.logLik=r.logLik;}
  if(kind==2) {
    const auto r=ztnbinom_mle(positive,tol,maxit,fixed_size);
    out.size=r.size;out.prob=r.prob;out.logLik=r.logLik;
    if(r.status==ZtnbinomStatus::IterationFailure)out.status=ModifiedStatus::IterationFailure;
    if(r.status==ZtnbinomStatus::PoissonLimit)out.status=ModifiedStatus::PoissonLimit;
  }
  if(kind==3) {
    const auto r=ztbinom_mle(positive,tol,maxit,fixed_size);
    out.size=r.size;out.prob=r.prob;out.logLik=r.logLik;
    if(r.status==ZtbinomStatus::IterationFailure)out.status=ModifiedStatus::IterationFailure;
    if(r.status==ZtbinomStatus::PoissonLimit)out.status=ModifiedStatus::PoissonLimit;
  }
  if(kind==4) {
    const auto logarithmic=logarithmic_fit(positive,maxit);
    out.prob=logarithmic.all_ones ? 0 : logarithmic.theta;
    out.logLik=logarithmic.all_ones ? 0 : logarithmic.logLik;
  }
  if(k<n)out.logLik+=(n-k)*std::log(out.p0);
  out.logLik+=k*std::log1p(-out.p0);
  if(mean(positive)==1)out.status=ModifiedStatus::TwoPoint;
  return out;
}
}
}
