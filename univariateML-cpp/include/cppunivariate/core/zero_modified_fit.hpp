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
// FMA gives the residual of the rounded count ratio without first rounding
// the product. The public probability coordinate has unit accuracy scale.
inline bool zero_fraction_accurate(double zeros,double n,double p0,double tol,StopDetail& detail) {
  const double residual=std::abs(std::fma(-p0,n,zeros));
  if(residual==0)return true;
  const double error=std::nextafter(residual/n,INFINITY);
  if(error<=tol)return true;
  detail.accuracy_stop=AccuracyStop::Tolerance;
  detail.attainable_tol=error;
  return false;
}
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
  if(kind==0) {
    const auto r=ztpois_mle(positive,NAN,tol,maxit);
    out.lambda=r.lambda;out.logLik=r.logLik;out.iterations=r.iterations;out.stop_detail=r.stop_detail;
    if(r.status!=ZtpoisStatus::Ok)out.status=ModifiedStatus::IterationFailure;
  }
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
  if(kind==0 && out.status==ModifiedStatus::Ok &&
     !zero_fraction_accurate(n-k,n,out.p0,tol,out.stop_detail)) {
    out.status=ModifiedStatus::IterationFailure;
    out.stop_detail.candidates.push_back({{out.lambda,out.p0},out.logLik,out.logLik});
  }
  if(out.status==ModifiedStatus::Ok && mean(positive)==1)out.status=ModifiedStatus::TwoPoint;
  return out;
}
}
}
