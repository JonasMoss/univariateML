#pragma once
#include "../../core/outcome.hpp"
#include "cppunivariate/distributions/gengamma/shape_scan.hpp"
#include "cppunivariate/distributions/gengamma/free_certificate.hpp"
#include "cppunivariate/distributions/gengamma/face_certificate.hpp"
#include "cppunivariate/distributions/gengamma/zero_certificate.hpp"
#include "cppunivariate/core/stop_detail.hpp"
#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {
enum class GengammaStatus { Ok, InvalidData, InvalidFixed, Unbounded, IterationFailure, PowerBoundary, ParetoBoundary };
struct GengammaResult {
  double mu,sigma,Q,logLik;
  int iterations;
  GengammaStatus status;
  bool certified=false;
  StopDetail stop_detail{};
};
namespace gengamma_fit_detail {
inline double mean_loglik(span y,double mu,double sigma,double q) {
  if (!(sigma>0) || !std::isfinite(sigma) || !std::isfinite(mu) || !std::isfinite(q)) return NAN;
  CPPUNIV_EXTENDED_REAL sum=0;
  for(double v:y) {
    const double w=(v-mu)/sigma;
    const double penalty=distributions::gengamma_detail::penalty(w,q);
    if(!std::isfinite(penalty))return NAN;
    sum-=static_cast<CPPUNIV_EXTENDED_REAL>(penalty)+v;
  }
  return static_cast<double>(sum/y.size())-std::log(sigma)-
      distributions::gengamma_detail::half_log_two_pi-distributions::gengamma_detail::remainder(q);
}
}
inline GengammaResult gengamma_mle(span x,double tol=1e-10,int maxit=100,
  double mu=NAN,double sigma=NAN,double q=NAN) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  GengammaResult out{mu,sigma,q,nan,0,GengammaStatus::InvalidData};
  if (!(tol>0) || !std::isfinite(tol) || maxit<1) return out;
  const bool fm=std::isnan(mu), fs=std::isnan(sigma), fq=std::isnan(q);
  if ((!fm && !std::isfinite(mu)) || (!fs && (!(sigma>0)||!std::isfinite(sigma))) || (!fq && !std::isfinite(q))) {
    out.status=GengammaStatus::InvalidFixed;return out;
  }
  if (x.empty()) return out;
  std::vector<double> y; y.reserve(x.size());
  CPPUNIV_EXTENDED_REAL total=0;
  double log_error=0;
  for(double v:x) {
    if (!(v>0)||!std::isfinite(v)) return out;
    const double z=std::log(v);y.push_back(z);total+=z;
    if(z!=0)log_error=std::max(log_error,8*std::numeric_limits<double>::epsilon()*std::abs(z));
  }
  const span logs{y.data(),y.size()};
  if (trgamma_detail::lost_log_distinctions(x,logs)) {out.status=GengammaStatus::IterationFailure;return out;}
  const double mean=static_cast<double>(total/y.size());
  const auto accuracy_failure=[&](double m,double s,double shape,double bound=NAN) {
    out.status=GengammaStatus::IterationFailure;
    record_accuracy(out.stop_detail,tol,{m,s,shape},
      gengamma_fit_detail::mean_loglik(logs,m,s,shape)*y.size(),
      {fm?std::max(std::abs(m),s):0,fs?s:0,fq?std::max(1.,std::abs(shape)):0},
      (fm || fs || fq)?bound:NAN);
    return out;
  };
  const bool constant=std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];});
  const bool log_at_mu=!fm && std::all_of(y.begin(),y.end(),[&](double v){return v==mu;});
  const bool at_mu=log_at_mu && mu==0 && std::all_of(x.begin(),x.end(),[](double v){return v==1;});
  bool rounded_ties=false;
  if (!fm) for(std::size_t i=0;i<y.size();++i)
    if (y[i]==mu && !(mu==0 && x[i]==1)) rounded_ties=true;
  if ((fs || fq) && log_at_mu && !at_mu) {out.status=GengammaStatus::IterationFailure;return out;}
  if (fs && ((fm && constant) || at_mu)) {out.status=GengammaStatus::Unbounded;return out;}
  if (!fq) {
    if (fm && fs) {
      const auto r=gengamma_fit_detail::location_scale(logs,q,tol,maxit);
      if(r.status!=gengamma_fit_detail::ConditionalStatus::Ok){out.status=GengammaStatus::IterationFailure;out.stop_detail=r.stop_detail;return out;}
      mu=r.mu;sigma=r.sigma;
    } else if(fm) {
      const auto r=gengamma_fit_detail::location(logs,sigma,q);
      if(r.status!=gengamma_fit_detail::ConditionalStatus::Ok){out.status=GengammaStatus::IterationFailure;out.stop_detail=r.stop_detail;return out;}
      mu=r.value;
    } else if(fs) {
      const auto r=gengamma_fit_detail::scale(logs,mu,q,tol,maxit);
      if(r.status!=gengamma_fit_detail::ConditionalStatus::Ok){out.status=GengammaStatus::IterationFailure;out.stop_detail=r.stop_detail;return out;}
      sigma=r.value;
    }
    if (fm || fs) {
      std::array<double,3> p{mu,sigma,q};
      if(!gengamma_fit_detail::polish_local(logs,mean,sigma,p,{fm,fs,false},tol,maxit,log_error,out.stop_detail)) {
        out.status=GengammaStatus::IterationFailure;return out;
      }
      mu=p[0];sigma=p[1];
    }
    if (rounded_ties && 8*std::numeric_limits<double>::epsilon()*(1+std::abs(mu))>0.01*tol*sigma)
      return accuracy_failure(mu,sigma,q,800*std::numeric_limits<double>::epsilon()*(1+std::abs(mu))/sigma);
    const double ll=gengamma_fit_detail::mean_loglik(logs,mu,sigma,q)*y.size();
    if (!std::isfinite(ll)) {out.status=GengammaStatus::IterationFailure;return out;}
    out.mu=mu;out.sigma=sigma;out.Q=q;out.logLik=ll;out.status=GengammaStatus::Ok;
    out.certified=true;return out;
  }
  if (!fs && constant && (fm || at_mu)) {
    mu=fm?y[0]:mu;q=0;
    if(fm && log_error>tol*0.25*std::max(std::abs(mu),sigma))
      return accuracy_failure(mu,sigma,q,4*log_error/std::max(std::abs(mu),sigma));
    out.mu=mu;out.sigma=sigma;out.Q=q;
    out.logLik=gengamma_fit_detail::mean_loglik(logs,mu,sigma,q)*y.size();
    if(!std::isfinite(out.logLik)){out.status=GengammaStatus::IterationFailure;return out;}
    out.status=GengammaStatus::Ok;out.certified=true;return out;
  }
  CPPUNIV_EXTENDED_REAL squares=0;
  for(double v:y) {const CPPUNIV_EXTENDED_REAL z=static_cast<CPPUNIV_EXTENDED_REAL>(v)-mean;squares+=z*z;}
  double unit=std::sqrt(static_cast<double>(squares/y.size()));
  if (!fs) unit=sigma;
  else if (!fm) unit=std::max(unit,std::abs(mean-mu));
  if (!(unit>0) || !std::isfinite(unit)) unit=fs?std::abs(y[0]-mu):sigma;
  if (!(unit>0) || !std::isfinite(unit)) {out.status=GengammaStatus::IterationFailure;return out;}
  if (fm && fs && gengamma_fit_detail::multiplicatively_symmetric(x)) {
    const auto certificate=gengamma_fit_detail::certify_zero(x,logs,maxit,log_error);
    if(certificate.certified) {
      std::array<double,3> p{mean,unit,0};
      if(gengamma_fit_detail::polish_local(logs,mean,unit,p,{true,true,false},tol,maxit,log_error,out.stop_detail)) {
        const double ll=gengamma_fit_detail::mean_loglik(logs,p[0],p[1],0)*y.size();
        if(std::isfinite(ll)) {
          out.mu=p[0];out.sigma=p[1];out.Q=0;out.logLik=ll;
          out.iterations=certificate.evaluations;out.status=GengammaStatus::Ok;out.certified=true;return out;
        }
      }
    }
  }
  const auto zero=gengamma_fit_detail::shape_point(logs,0,mu,sigma,fm,fs,tol,maxit);
  if(!zero.valid){out.status=GengammaStatus::IterationFailure;out.stop_detail=zero.stop_detail;return out;}
  const double qmin=std::max(1e-100,std::min(1e-5,std::abs(zero.score)*1e-5));
  double qmax=1;
  const int budget=static_cast<int>(solver_budget(maxit));
  if (!fs) {
    for(int i=0;i<budget;++i) {
      const double bound=-std::log(sigma)-distributions::gengamma_detail::half_log_two_pi-
        distributions::gengamma_detail::remainder(qmax);
      if(std::isfinite(bound) && bound<zero.objective) break;
      if(qmax>std::numeric_limits<double>::max()/2)break;
      qmax*=2;
    }
  } else if(fm) {
    std::vector<double> reverse;reverse.reserve(y.size());for(double v:y)reverse.push_back(-v);
    for(int sign=0;sign<2;++sign) {
      trgamma_detail::LogData d;
      const span z=sign==0?logs:span{reverse.data(),reverse.size()};
      if(!trgamma_detail::standardize(z,d))continue;
      const double n=static_cast<double>(d.n);
      const double t=4*(n+std::log(n)+10)/d.umax;
      const auto c=trgamma_detail::cgf(d,t);int iterations=0;
      const double a=trgamma_detail::gamma_shape(c.K,iterations,maxit);
      if(a>0 && std::isfinite(a)) qmax=std::max(qmax,1/std::sqrt(a));
    }
  } else {
    qmax=std::ldexp(1.,std::min(budget,std::numeric_limits<double>::max_exponent-2));
  }
  if (!(std::isfinite(qmax) && qmax>qmin)) {out.status=GengammaStatus::IterationFailure;return out;}
  const auto candidates=gengamma_fit_detail::shape_candidates(logs,mu,sigma,fm,fs,mean,unit,qmin,qmax,tol,maxit);
  out.iterations=candidates.evaluations;
  const auto strict_face=[&]() {
    if (!(fm && fs)) return false;
    const auto certificate=gengamma_fit_detail::strict_winning_face(logs,maxit,log_error);
    out.iterations+=certificate.evaluations;
    const auto endpoints=std::minmax_element(x.begin(),x.end());
    const auto log_endpoints=std::minmax_element(y.begin(),y.end());
    if(certificate.winner==gengamma_fit_detail::FaceWinner::Power) {
      const double beta=1/(*log_endpoints.second-mean);
      const double ll=(-1+std::log(beta)-mean)*y.size();
      if (!(beta>0 && std::isfinite(beta) && std::isfinite(ll))) return false;
      out.status=GengammaStatus::PowerBoundary;out.logLik=ll;out.certified=true;
      out.stop_detail=limit_detail({*endpoints.second,beta},ll,StopModel::Power);
      const double error=256*std::numeric_limits<double>::epsilon()*(1+y.size())*(1+std::abs(ll));
      out.stop_detail.loglik_lower=ll-error;out.stop_detail.loglik_upper=ll+error;return true;
    }
    if(certificate.winner==gengamma_fit_detail::FaceWinner::Pareto) {
      const double a=1/(mean-*log_endpoints.first);
      const double ll=(-1+std::log(a)-mean)*y.size();
      if (!(a>0 && std::isfinite(a) && std::isfinite(ll))) return false;
      out.status=GengammaStatus::ParetoBoundary;out.logLik=ll;out.certified=true;
      out.stop_detail=limit_detail({a,*endpoints.first},ll,StopModel::Pareto);
      const double error=256*std::numeric_limits<double>::epsilon()*(1+y.size())*(1+std::abs(ll));
      out.stop_detail.loglik_lower=ll-error;out.stop_detail.loglik_upper=ll+error;return true;
    }
    return false;
  };
  if(candidates.values.empty()){
    if(strict_face())return out;
    out.status=GengammaStatus::IterationFailure;
    if(!candidates.stop_detail.candidates.empty())out.stop_detail=candidates.stop_detail;
    return out;
  }
  gengamma_fit_detail::ShapePoint best{};
  bool have_best=false;
  for(const auto& candidate:candidates.values) {
    std::array<double,3> p{candidate.mu,candidate.sigma,candidate.q};
    if(!gengamma_fit_detail::polish_local(logs,mean,unit,p,{fm,fs,true},tol,maxit,log_error,out.stop_detail))continue;
    const auto refined=gengamma_fit_detail::shape_point(logs,p[2],p[0],p[1],false,false,tol,maxit);
    if(refined.valid && (!have_best || refined.objective>best.objective)) {best=refined;have_best=true;}
  }
  if(!have_best){if(strict_face())return out;out.status=GengammaStatus::IterationFailure;return out;}
  // Failed alternative proposals do not diagnose this accepted local fit
  // or any subsequent comparison with a face.
  out.stop_detail={};
  if (rounded_ties && 8*std::numeric_limits<double>::epsilon()*(1+std::abs(mu))>0.01*tol*best.sigma)
    return accuracy_failure(best.mu,best.sigma,best.q,800*std::numeric_limits<double>::epsilon()*(1+std::abs(mu))/best.sigma);
  // Every reachable face supplies a lower bound, even when it is not known
  // to be the globally winning unattained supremum.
  double face=-std::numeric_limits<double>::infinity();
  double face_gap=std::numeric_limits<double>::infinity();
  StopModel face_model=StopModel::None;
  if(fs && fm && !constant) {
    const auto mm=std::minmax_element(y.begin(),y.end());
    const double power_gap=*mm.second-mean,pareto_gap=mean-*mm.first;
    const double power=-1-std::log(power_gap);
    const double pareto=-1-std::log(pareto_gap);
    face=std::max(power,pareto);face_model=power>=pareto?StopModel::Power:StopModel::Pareto;
    face_gap=power>=pareto?power_gap:pareto_gap;
  } else if(fs && !fm && !rounded_ties) {
    const auto mm=std::minmax_element(y.begin(),y.end());
    const bool uncertain_side=std::any_of(y.begin(),y.end(),[&](double v){
      return v!=mu && std::abs(v-mu)<=2*log_error;
    });
    if(!uncertain_side && *mm.second<=mu && mean<mu){face_gap=mu-mean;face=-1-std::log(face_gap);face_model=StopModel::Power;}
    if(!uncertain_side && *mm.first>=mu && mean>mu){face_gap=mean-mu;face=-1-std::log(face_gap);face_model=StopModel::Pareto;}
  }
  const double pad=256*std::numeric_limits<double>::epsilon()*(1+y.size())*
      (1+std::abs(best.objective)+std::abs(face))+
      (std::isfinite(face)?16*log_error*(1/std::max(best.sigma,std::numeric_limits<double>::min())+
                                         1/std::max(face_gap,std::numeric_limits<double>::min())):0);
  if(std::isfinite(face) && face>=best.objective-pad && strict_face())return out;
  if(std::isfinite(face) && face>best.objective+pad) {
    out.status=GengammaStatus::IterationFailure;out.stop_detail.limit_model=face_model;
    out.stop_detail.limit_loglik=(face-mean)*y.size();return out;
  }
  out.mu=best.mu;out.sigma=best.sigma;out.Q=best.q;out.logLik=best.mean_loglik*y.size();
  if (!std::isfinite(out.logLik)) {out.status=GengammaStatus::IterationFailure;return out;}
  out.status=GengammaStatus::Ok;out.stop_detail={};
  out.certified=fm && fs && gengamma_fit_detail::certify_free_candidate(logs,best,tol,maxit,log_error);
  return out;
}
}

namespace cppunivariate {
inline FitOutcome outcome(const GengammaResult& r) {
  if (r.stop_detail.accuracy_stop == AccuracyStop::Tolerance) return FitOutcome::failure(OutcomeReason::Tolerance);
  if (r.stop_detail.accuracy_stop == AccuracyStop::Conditioning) return FitOutcome::failure(OutcomeReason::Conditioning);
  switch (r.status) {
    case GengammaStatus::Ok: return FitOutcome::estimate(r.certified);
    case GengammaStatus::InvalidData: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case GengammaStatus::InvalidFixed: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case GengammaStatus::Unbounded: return FitOutcome::nonexistence(OutcomeReason::Unbounded);
    case GengammaStatus::IterationFailure: return FitOutcome::failure(OutcomeReason::NoConvergence);
    case GengammaStatus::PowerBoundary: return FitOutcome::limit();
    case GengammaStatus::ParetoBoundary: return FitOutcome::limit();
  }
  return {};
}
} // namespace cppunivariate
