#pragma once
#include "face_certificate.hpp"
#include "../../core/work_budget.hpp"
#include "../../core/discrete_stop.hpp"
#include "../ztpois/mle.hpp"
namespace cppunivariate {
enum class ZtbinomStatus { Ok, InvalidInput, InvalidSize, PoissonLimit, IterationFailure };
struct ZtbinomResult { double size, prob, logLik; int iterations; ZtbinomStatus status;   StopDetail stop_detail{};
  bool certified = true;
};
namespace ztbinom_detail {
template <typename F>
inline double maximize(F f, double lo, double hi, double tol, int maxit, bool &done) {
  maxit = static_cast<int>(solver_budget(maxit));
  constexpr double ratio=.6180339887498948482;
  double c=hi-ratio*(hi-lo), d=lo+ratio*(hi-lo), fc=f(c), fd=f(d);
  for(int i=0;i<maxit && hi-lo>tol;++i) {
    if(fc>fd) {hi=d;d=c;fd=fc;c=hi-ratio*(hi-lo);fc=f(c);}
    else {lo=c;c=d;fc=fd;d=lo+ratio*(hi-lo);fd=f(d);}
  }
  done=hi-lo<=tol && std::isfinite(fc) && std::isfinite(fd);
  return fc>fd ? c : d;
}
}
namespace ztbinom_detail {
inline ZtbinomResult ztbinom_mle_proposal(ispan x, double tol=1e-10, int maxit=200,
    double fixed_size=std::numeric_limits<double>::quiet_NaN(),
    double fixed_prob=std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  auto stopped = [&]() -> ZtbinomResult {
  const bool fixed=!std::isnan(fixed_size);
  if(fixed && (!(fixed_size>=1)||!std::isfinite(fixed_size)||fixed_size!=std::floor(fixed_size)))
    return {0,0,0,0,ZtbinomStatus::InvalidSize};
  if(!zero_detail::positive(x)||!(tol>0)||!std::isfinite(tol)||maxit<1)
    return {0,0,0,0,ZtbinomStatus::InvalidInput};
  int mx=1;for(int v:x) mx=std::max(mx,v);
  if(fixed && fixed_size<mx) return {0,0,0,0,ZtbinomStatus::InvalidSize};
  const double m=zero_detail::mean(x), n=x.size();
  if (!std::isnan(fixed_prob)) {
    const double p=fixed_prob;
    if (!(p>=0 && p<=1) || !std::isfinite(p)) return {0,0,0,0,ZtbinomStatus::InvalidInput};
    const auto profile_held=[&](double size) {
      double ll=0;
      for(int v:x) {
        if(p==0) {if(v!=1)ll=-INFINITY;}
        else if(p==1) {if(v!=size)ll=-INFINITY;}
        else ll+=zero_detail::bin_logpmf(v,size,p)-std::log(-std::expm1(size*std::log1p(-p)));
      }
      return ZtbinomResult{size,p,ll,0,std::isfinite(ll)?ZtbinomStatus::Ok:ZtbinomStatus::InvalidInput};
    };
    if(fixed)return profile_held(fixed_size);
    if(p==0 || p==1)return profile_held(mx);
    // At size one the conditional law is exactly the atom at one,
    // including arbitrarily small positive held probabilities.
    if(m==1)return {1,p,0,0,ZtbinomStatus::Ok};
    const std::int64_t cap=enumeration_budget(maxit);
    std::int64_t cost=static_cast<std::int64_t>(x.size());for(int v:x){if(cost>cap || v>cap-cost){cost=cap+1;break;}cost+=v;}
    if(cost>cap){ZtbinomResult failed{NAN,p,NAN,0,ZtbinomStatus::IterationFailure};failed.stop_detail=discrete_budget(failed.stop_detail);return failed;}
    auto best=profile_held(mx);
    std::int64_t work=cost;
      for(std::int64_t j=1;;++j) {
        if(cost>cap-work){
          best.status=ZtbinomStatus::IterationFailure;
          best.stop_detail=discrete_budget(best.stop_detail);
          best.stop_detail.candidates.push_back({{best.size,best.prob},best.logLik,best.logLik});
          best.stop_detail.loglik_lower=best.logLik;
          if(std::isfinite(best.logLik)){best.status=ZtbinomStatus::Ok;best.certified=false;}
          return best;
        }
        work+=cost;
      const double size=double(mx)+j;
      auto candidate=profile_held(size);
      if(candidate.logLik>best.logLik)best=candidate;
      // For size >= mx/p every mass on the observed support decreases.
      // The truncation probability increases, so this bounds all future sizes.
      if(size>=mx/p) {
        const double upper=n*(zero_detail::bin_logpmf(mx,size,p)-std::log(-std::expm1(size*std::log1p(-p))));
        if(upper<best.logLik) return best;
      }
    }
    best.status=ZtbinomStatus::IterationFailure;return best;
  }
  if(m==1) return {fixed?fixed_size:1,0,0,0,ZtbinomStatus::Ok};
  auto profile=[&](double r) {
    const double p=zero_detail::bin_prob(r,m,maxit);
    double ll=0;
    for(int v:x) ll+=zero_detail::bin_logpmf(v,r,p)-std::log(-std::expm1(r*std::log1p(-p)));
    // The shared probability solver's absolute bracket width is insufficient
    // when a held size makes p tiny. Check the total log-odds score as well.
    const double conditional_mean=r*p/(-std::expm1(r*std::log1p(-p)));
    const bool resolved=std::abs(n*(conditional_mean-m))<1e-7;
    return ZtbinomResult{r,p,ll,0,std::isfinite(ll)&&resolved?ZtbinomStatus::Ok:ZtbinomStatus::IterationFailure};
  };
  if(fixed) return profile(fixed_size);
  auto best=profile(mx);
  // Exponential grid in size (mx+exp(t)) with golden-section refinement
  // within each cell: unlike binom's free-size search, there is no known
  // unimodality proof for this profile (prob is re-profiled by exact
  // conditional-mean matching at every candidate size, which does not
  // obviously inherit DeRiggi's untruncated-binomial argument -- see
  // notes/evaluations/2026-10-29-tail85-report.md and the P0 binomial
  // note). This grid search is therefore still numerical, not
  // certified; what is fixed here is the old hardcoded "t<=24" cap
  // (mx+e^24 =~ mx+2.6e10), which failed unconditionally beyond that
  // magnitude regardless of how much iteration budget was left. The
  // grid now always covers at least that same depth (t<24 below is a
  // strict superset of the old search, so no existing case regresses),
  // then keeps growing -- governed only by the iteration budget
  // `maxit` and by `patience` consecutive non-improving cells, both
  // ordinary stopping rules, not a magnitude limit.
  int since_improvement=0, t=0;
  const int grid_floor=24, patience=10, cap=static_cast<int>(solver_budget(maxit));
  for(; t<=cap; ++t) {
    bool improved=false;
    const double r=mx+std::exp(t);
    auto candidate=profile(std::floor(r));
    if(candidate.logLik>best.logLik) {best=candidate;improved=true;}
    const double ll=profile(r).logLik;
    if(!(ll<profile(mx+std::exp(t-1)).logLik || ll<profile(mx+std::exp(t+1)).logLik)) {
      bool done=false;
      const double opt=ztbinom_detail::maximize([&](double s){return profile(mx+std::exp(s)).logLik;},t-1,t+1,tol,maxit,done);
      if(!done) {best.status=ZtbinomStatus::IterationFailure;return best;}
      const double s=mx+std::exp(opt);
      for(double k:{std::floor(s),std::ceil(s)}) {candidate=profile(k);if(candidate.logLik>best.logLik){best=candidate;improved=true;}}
    }
    if(improved) since_improvement=0; else ++since_improvement;
    if(t>=grid_floor && since_improvement>patience) break;
  }
  const auto face=ztpois_mle(x);
  if(face.logLik>=best.logLik-1e-8*n) {
    best.status=ZtbinomStatus::PoissonLimit;
    best.stop_detail=discrete_face(StopModel::Ztpois,{face.lambda},face.logLik);
  }
  // Ran out of the iteration budget while still finding improvements,
  // rather than stabilizing: the budget, not a size, is what is
  // exhausted (a larger `maxit` can always be supplied).
  if(best.status==ZtbinomStatus::Ok && t>cap && since_improvement<=patience)best.status=ZtbinomStatus::IterationFailure;
  return best;

  }();

  return stopped;
}
} // namespace ztbinom_detail
inline ZtbinomResult ztbinom_mle(ispan x,double tol=1e-10,int maxit=200,
    double fixed_size=NAN,double fixed_prob=NAN) {
  bool candidate_ready=false;
  auto checked = [&]() -> ZtbinomResult {
  auto best=ztbinom_detail::ztbinom_mle_proposal(x,tol,maxit,fixed_size,fixed_prob);
  // Only a successful fixed-size solve can supply the finite fallback.
  if(std::isnan(fixed_size) && std::isnan(fixed_prob) && x.size()>0 &&
     best.status!=ZtbinomStatus::InvalidInput && best.status!=ZtbinomStatus::InvalidSize &&
     std::isfinite(best.size) && std::isfinite(best.logLik)) {
    const auto finite=ztbinom_detail::ztbinom_mle_proposal(x,tol,maxit,best.size,NAN);
    candidate_ready=finite.status==ZtbinomStatus::Ok;
    if(!candidate_ready) {
      int observed=0;for(int v:x)observed=std::max(observed,v);
      double value=-INFINITY;
      std::int64_t remaining=enumeration_budget(maxit),cost=static_cast<std::int64_t>(x.size());
      for(int v:x)cost+=v;
      for(std::int64_t j=0;j<solver_budget(maxit);++j) {
        if(cost>remaining)break;
        remaining-=cost;
        const auto f=ztbinom_detail::ztbinom_mle_proposal(x,tol,maxit,observed+double(j),NAN);
        if(f.status!=ZtbinomStatus::Ok || !std::isfinite(f.logLik))continue;
        if(f.logLik>value) {
          value=f.logLik;best.size=f.size;best.prob=f.prob;best.logLik=f.logLik;
          candidate_ready=true;
        }
      }
    }
    if(candidate_ready && best.status==ZtbinomStatus::IterationFailure) {
      best.status=ZtbinomStatus::Ok;best.certified=false;return best;
    }
  }
  if(!std::isnan(fixed_size)||!std::isnan(fixed_prob)||x.size()==0) {
    if(best.status==ZtbinomStatus::IterationFailure)
      binomial_certificate_detail::witness(best.stop_detail,{best.size,best.prob},best.logLik,best.stop_detail.budget_exhausted);
    return best;
  }
  int mx=0;double sum=0;for(int v:x){mx=std::max(mx,v);sum+=v;}
  if(sum==double(x.size()))return best;
  // Size two spans every law on {1,2}; matching the empirical frequency
  // attains the multinomial upper bound on the entire parameter domain.
  if(mx==2) {
    const auto saturated=ztbinom_detail::ztbinom_mle_proposal(x,tol,maxit,2,NAN);
    if(saturated.status==ZtbinomStatus::Ok)return saturated;
  }
  if(best.status==ZtbinomStatus::InvalidInput || best.status==ZtbinomStatus::InvalidSize)return best;
  if(best.status==ZtbinomStatus::IterationFailure) {
    binomial_certificate_detail::witness(best.stop_detail,{best.size,best.prob},best.logLik,false);return best;
  }
  candidate_ready=candidate_ready && std::isfinite(best.logLik) && best.size>=1 && best.prob>=0 && best.prob<=1;
  binomial_certificate_detail::Work work{0,0,maxit};
  if(best.status==ZtbinomStatus::PoissonLimit) {
    const double face=best.stop_detail.limit_loglik;
    bool exhausted=false;
    const double end=binomial_certificate_detail::signed_cutoff(x,1,INFINITY,best.stop_detail.limit_coef[0],work,exhausted);
    if(!std::isfinite(end) && (exhausted || work.numerical)) {
      best.status=ZtbinomStatus::IterationFailure;
      binomial_certificate_detail::witness(best.stop_detail,{best.size,best.prob},best.logLik,exhausted);return best;
    }
    if(std::isfinite(end)) {
      bool finite_seen=false;
      const auto covered=binomial_certificate_detail::cover_face(x,mx,end,face,work,[&](double N) {
        const auto f=ztbinom_detail::ztbinom_mle_proposal(x,tol,maxit,N,NAN);
        if(f.status!=ZtbinomStatus::Ok)return double(NAN);
        if(!finite_seen || f.logLik>best.logLik) {best.size=f.size;best.prob=f.prob;best.logLik=f.logLik;}
        finite_seen=true;
        return f.logLik;
      });
      if(covered==binomial_certificate_detail::FaceCoverage::Face) {
        binomial_certificate_detail::certified_face(best.stop_detail,{best.size,best.prob},best.logLik);return best;
      }
      if(covered==binomial_certificate_detail::FaceCoverage::Finite) {
        best.status=ZtbinomStatus::Ok;best.stop_detail={};
      } else if(covered==binomial_certificate_detail::FaceCoverage::Failure) {
        best.status=ZtbinomStatus::IterationFailure;
        binomial_certificate_detail::witness(best.stop_detail,{best.size,best.prob},best.logLik,!work.numerical);return best;
      }
    }
    if(best.status!=ZtbinomStatus::Ok) {
      binomial_certificate_detail::witness(best.stop_detail,{best.size,best.prob},best.logLik,false,true);return best;
    }
  }
  if(best.logLik==0)return best;
  const double end=binomial_certificate_detail::cutoff(x,1,INFINITY,false,NAN,best.logLik,work);
  if(std::isfinite(end) && best.size>=end)work.numerical=true;
  if(!std::isfinite(end) || work.numerical) {
    best.status=ZtbinomStatus::IterationFailure;
    binomial_certificate_detail::witness(best.stop_detail,{best.size,best.prob},best.logLik,!work.numerical);return best;
  }
  double incumbent=best.logLik;
  const bool complete=binomial_certificate_detail::cover(x,mx,end,incumbent,work,[&](double N) {
    const auto f=ztbinom_detail::ztbinom_mle_proposal(x,tol,maxit,N,NAN);
    if(f.status!=ZtbinomStatus::Ok)return double(NAN);
    if(f.logLik>best.logLik)best=f;
    return f.logLik;
  });
  if(!complete) {
    best.status=ZtbinomStatus::IterationFailure;
    binomial_certificate_detail::witness(best.stop_detail,{best.size,best.prob},best.logLik,!work.numerical);
  }
  if(complete)best.certified=true;
  return best;
  };
  auto best=checked();
  if(candidate_ready && best.status!=ZtbinomStatus::Ok &&
     (best.stop_detail.comparison_tie || !std::isfinite(best.stop_detail.loglik_upper)) &&
     std::isfinite(best.logLik) && best.size>=1 && best.prob>=0 && best.prob<=1) {
    best.status=ZtbinomStatus::Ok;best.certified=false;
  }
  // A double coefficient cannot inherit an exact-arithmetic saturation proof
  // below the conservative forward-error allowance for its probability ratio.
  if(best.status==ZtbinomStatus::Ok && std::isnan(fixed_prob) && best.size>1 &&
     best.prob>0 && best.prob<1 && tol<8*std::numeric_limits<double>::epsilon())
    best.certified=false;
  return best;
}

} // namespace cppunivariate
