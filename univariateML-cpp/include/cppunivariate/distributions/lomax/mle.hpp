// Lomax profile search on the dimensionless bounded existence interval.

#pragma once
#include "../../core/outcome.hpp"
#include "../../core/work_budget.hpp"
#include "../../core/stop_detail.hpp"
#include "../../core/precision.hpp"

#include "../../core/reductions.hpp"
#include "../../core/span.hpp"

#include <cmath>
#include <algorithm>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

enum class LomaxStatus {
  Ok = 0,
  ExponentialLimit,
  InvalidStart,
  IterationFailure,
  AccuracyFailure
};

enum class LomaxCertificateFailure { None, Budget, Arithmetic };

struct LomaxResult {
  double lambda;
  double kappa;
  double logLik;
  int iterations;
  LomaxStatus status;
  LomaxCertificateFailure certificate_failure = LomaxCertificateFailure::None;
  StopDetail stop_detail{};
};

namespace lomax_detail {
// Profile log-likelihood in lambda (kappa at its closed-form maximiser),
// up to a constant: n log(lambda) - n log(S) - S, S = sum log1p(lambda x).
inline double profile_loglik(span x, double lambda) {
  double S = 0.0;
  for (auto v : x) S += std::log1p(lambda * v);
  const double n = static_cast<double>(x.size());
  return n * std::log(lambda) - n * std::log(S) - S;
}
struct Profile {
  CPPUNIV_EXTENDED_REAL A,B,D,T,K;
  CPPUNIV_EXTENDED_REAL score() const {return T/A-B;}
  CPPUNIV_EXTENDED_REAL curvature() const {return K/A+D+(T/A)*(T/A);}
  double error() const {return 8*std::numeric_limits<double>::epsilon()*static_cast<double>(std::abs(T/A)+std::abs(B));}
};
// Positive p-series avoid cancellation in log1p(r)-p and p^2-2*T.
// At p<=1/2 the degree-64 tail is <= p^65/(65*(1-p)).
inline Profile profile(span x,double lambda) {
    CPPUNIV_EXTENDED_REAL S=0,S1=0,S2=0,T=0,K=0;
    CPPUNIV_EXTENDED_REAL c0=0,c1=0,c2=0,c3=0,c4=0;
    const auto add=[](CPPUNIV_EXTENDED_REAL value,CPPUNIV_EXTENDED_REAL& sum,CPPUNIV_EXTENDED_REAL& correction) {
      const CPPUNIV_EXTENDED_REAL y=value-correction,t=sum+y;correction=(t-sum)-y;sum=t;
    };
    for (auto v : x) {
      const CPPUNIV_EXTENDED_REAL r=static_cast<CPPUNIV_EXTENDED_REAL>(lambda)*v;
      const CPPUNIV_EXTENDED_REAL p=r/(1+r),l=std::log1p(r);
      const CPPUNIV_EXTENDED_REAL t=p<=.5?p*p*((1.0/2.0)+p*((1.0/3.0)+p*((1.0/4.0)+p*((1.0/5.0)+p*((1.0/6.0)+p*((1.0/7.0)+p*((1.0/8.0)+p*((1.0/9.0)+p*((1.0/10.0)+p*((1.0/11.0)+p*((1.0/12.0)+p*((1.0/13.0)+p*((1.0/14.0)+p*((1.0/15.0)+p*((1.0/16.0)+p*((1.0/17.0)+p*((1.0/18.0)+p*((1.0/19.0)+p*((1.0/20.0)+p*((1.0/21.0)+p*((1.0/22.0)+p*((1.0/23.0)+p*((1.0/24.0)+p*((1.0/25.0)+p*((1.0/26.0)+p*((1.0/27.0)+p*((1.0/28.0)+p*((1.0/29.0)+p*((1.0/30.0)+p*((1.0/31.0)+p*((1.0/32.0)+p*((1.0/33.0)+p*((1.0/34.0)+p*((1.0/35.0)+p*((1.0/36.0)+p*((1.0/37.0)+p*((1.0/38.0)+p*((1.0/39.0)+p*((1.0/40.0)+p*((1.0/41.0)+p*((1.0/42.0)+p*((1.0/43.0)+p*((1.0/44.0)+p*((1.0/45.0)+p*((1.0/46.0)+p*((1.0/47.0)+p*((1.0/48.0)+p*((1.0/49.0)+p*((1.0/50.0)+p*((1.0/51.0)+p*((1.0/52.0)+p*((1.0/53.0)+p*((1.0/54.0)+p*((1.0/55.0)+p*((1.0/56.0)+p*((1.0/57.0)+p*((1.0/58.0)+p*((1.0/59.0)+p*((1.0/60.0)+p*((1.0/61.0)+p*((1.0/62.0)+p*((1.0/63.0)+p*((1.0/64.0)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))):l-p;
      const CPPUNIV_EXTENDED_REAL k=p<=.5?-2*p*p*p*((1.0/3.0)+p*((1.0/4.0)+p*((1.0/5.0)+p*((1.0/6.0)+p*((1.0/7.0)+p*((1.0/8.0)+p*((1.0/9.0)+p*((1.0/10.0)+p*((1.0/11.0)+p*((1.0/12.0)+p*((1.0/13.0)+p*((1.0/14.0)+p*((1.0/15.0)+p*((1.0/16.0)+p*((1.0/17.0)+p*((1.0/18.0)+p*((1.0/19.0)+p*((1.0/20.0)+p*((1.0/21.0)+p*((1.0/22.0)+p*((1.0/23.0)+p*((1.0/24.0)+p*((1.0/25.0)+p*((1.0/26.0)+p*((1.0/27.0)+p*((1.0/28.0)+p*((1.0/29.0)+p*((1.0/30.0)+p*((1.0/31.0)+p*((1.0/32.0)+p*((1.0/33.0)+p*((1.0/34.0)+p*((1.0/35.0)+p*((1.0/36.0)+p*((1.0/37.0)+p*((1.0/38.0)+p*((1.0/39.0)+p*((1.0/40.0)+p*((1.0/41.0)+p*((1.0/42.0)+p*((1.0/43.0)+p*((1.0/44.0)+p*((1.0/45.0)+p*((1.0/46.0)+p*((1.0/47.0)+p*((1.0/48.0)+p*((1.0/49.0)+p*((1.0/50.0)+p*((1.0/51.0)+p*((1.0/52.0)+p*((1.0/53.0)+p*((1.0/54.0)+p*((1.0/55.0)+p*((1.0/56.0)+p*((1.0/57.0)+p*((1.0/58.0)+p*((1.0/59.0)+p*((1.0/60.0)+p*((1.0/61.0)+p*((1.0/62.0)+p*((1.0/63.0)+p*((1.0/64.0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))):p*p-2*t;
      add(l,S,c0);add(p,S1,c1);add(p*p,S2,c2);add(t,T,c3);add(k,K,c4);
    }
    const double dn=static_cast<double>(x.size());
    S/=dn;S1/=dn;S2/=dn;T/=dn;K/=dn;
    return {S,S1,S2,T,K};
}
inline bool local_enclosure(span x,double rate,double tol,const Profile& center) {
  const double lo=rate*(1-.99*tol),hi=rate*(1+.99*tol);
  if(!(lo>0 && lo<rate && hi>rate && std::isfinite(hi)))return false;
  const auto a=profile(x,lo),b=profile(x,hi);
  if(!(a.score()>a.error() && b.score()< -b.error()))return false;
  // A,B,D increase with rate. This bounds rate^2 times the profile
  // curvature throughout the box, so the signed root is its unique maximum.
  const CPPUNIV_EXTENDED_REAL ratio=b.B/a.A;
  const CPPUNIV_EXTENDED_REAL bound=ratio*ratio+b.D/a.A+b.D-1;
  const double pad=32*std::numeric_limits<double>::epsilon()*
    static_cast<double>(1+ratio*ratio+b.D/a.A+b.D);
  if(!(bound< -pad))return false;
  const double kappa=static_cast<double>(1/center.A);
  const double kl=static_cast<double>(1/a.A),kh=static_cast<double>(1/b.A);
  return std::max((rate-lo)/lo,(hi-rate)/hi)<=tol &&
    std::max(std::abs(kappa/kl-1),std::abs(kappa/kh-1))<=tol;
}
}  // namespace lomax_detail

// lambda0: optional start for lambda; NaN (the default) or a non-positive
// value means the method-of-moments start.
inline LomaxResult lomax_newton_candidate(
    span x, double tol, int maxit,
    double lambda0 = std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0 || maxit<1) return {0.0, 0.0, kNaN, 0, LomaxStatus::InvalidStart};

  double largest=0;for(double v:x)largest=std::max(largest,v);
  CPPUNIV_EXTENDED_REAL total=0,total_correction=0;
  for(double v:x) {
    const CPPUNIV_EXTENDED_REAL y=static_cast<CPPUNIV_EXTENDED_REAL>(v)/largest-total_correction;
    const CPPUNIV_EXTENDED_REAL t=total+y;total_correction=(t-total)-y;total=t;
  }
  const double unit=largest*static_cast<double>(total/static_cast<CPPUNIV_EXTENDED_REAL>(n));
  if (unit > 0.0 && std::isfinite(unit) && (unit < 1e-3 || unit > 1e3)) {
    std::vector<double> z(n);
    for (std::size_t i = 0; i < n; ++i) z[i] = x[i] / unit;
    const double start = std::isfinite(lambda0) && lambda0 > 0.0 ? lambda0 * unit : lambda0;
    const auto r = lomax_newton_candidate(span{z.data(), n}, tol, maxit, start);
    auto out=r;out.lambda=r.lambda/unit;out.logLik-=static_cast<double>(n)*std::log(unit);
    const double jac=static_cast<double>(n)*std::log(unit);
    for(auto& c:out.stop_detail.candidates) {
      if(c.model==StopModel::None && c.coef.size()==2)c.coef[0]/=unit;
      c.loglik_lower-=jac;c.loglik_upper-=jac;
    }

    if(r.status==LomaxStatus::Ok && (!(out.lambda>0 && std::isfinite(out.lambda)) ||
       std::abs(out.lambda*unit-r.lambda)>.25*tol*r.lambda))out.status=LomaxStatus::AccuracyFailure;
    out.stop_detail.candidates.erase(std::remove_if(out.stop_detail.candidates.begin(),out.stop_detail.candidates.end(),
      [](const FitCandidate& c){return c.model==StopModel::None &&
        (c.coef.size()!=2 || !(c.coef[0]>0 && c.coef[1]>0) || !std::isfinite(c.coef[0]+c.coef[1]));}),out.stop_detail.candidates.end());
    if(out.status==LomaxStatus::AccuracyFailure) {
      if(out.lambda>0 && out.kappa>0)record_accuracy(out,tol,{out.lambda,out.kappa},{out.lambda,out.kappa});
      else{out.stop_detail.accuracy_stop=AccuracyStop::None;out.stop_detail.attainable_tol=NAN;}
    }
    return out;
  }

  const double m = cppunivariate::mean(x);
  double sum_xsq = 0.0;
  for (auto v : x) sum_xsq += v * v;
  const double s = sum_xsq / static_cast<double>(n);
  const double m2 = m * m;

  const double alpha0 = 2.0 * (s - m2) / (s - 2.0 * m2);
  double lambda = (std::isfinite(lambda0) && lambda0 > 0.0)
                      ? lambda0
                      : 1.0 / (m * (alpha0 - 1.0));
  if (std::isfinite(lambda0) && lambda0>0.0 &&
      (lambda0*m < 1e-8 || lambda0*m > 1e8))
    return lomax_newton_candidate(x,tol,maxit); // A starting hint outside resolvable score arithmetic.
  if (lambda <= 0.0 || !std::isfinite(lambda)) {
    return {kNaN, kNaN, kNaN, 0, LomaxStatus::InvalidStart};
  }

  tol=std::min(tol,1e-10);
  double previous=std::numeric_limits<double>::infinity();
  int iter = 1;
  for (; iter <= maxit; ++iter) {
    const auto profile=lomax_detail::profile(x,lambda);
    const CPPUNIV_EXTENDED_REAL psi=profile.B/profile.A;
    const CPPUNIV_EXTENDED_REAL f=profile.score(),df=profile.curvature();

    // f is lambda times the profile score, and df is lambda squared
    // times its derivative. Their quotient is the relative Newton correction.
    // Newton is an ascent step only where the profile is concave (df < 0);
    // elsewhere (possible from a user-supplied lambda0 far from the MLE)
    // move geometrically uphill. Every step is halved until the profile
    // log-likelihood does not decrease, and convergence is only declared
    // on a full Newton step, so a shrunken step cannot fake convergence.
    // From the moment start the plain Newton step is normally accepted.
    const bool newton = df < 0.0;
    double step = newton ? lambda * f / df : (f > 0.0 ? -lambda : 0.5 * lambda);
    if (!std::isfinite(step)) {
      return {kNaN, kNaN, kNaN, iter, LomaxStatus::IterationFailure};
    }
    if (lambda - step <= 0.0) step = 0.5 * lambda;
    const double lp_old = lomax_detail::profile_loglik(x, lambda);
    int halvings = 0;
    for (; halvings < maxit; ++halvings) {
      const double lp_new = lomax_detail::profile_loglik(x, lambda - step);
      if (lp_new >= lp_old - 1e-12 * (std::abs(lp_old) + 1.0)) break;
      step *= 0.5;
    }
    if (halvings == maxit)
      return {kNaN,kNaN,kNaN,iter,LomaxStatus::IterationFailure};
    const double lambda_new = lambda - step;
    if (newton && halvings == 0 &&
        std::abs(step) < .25 * tol * std::abs(lambda) &&
        std::abs(step / lambda * psi) < .25 * tol) {
      const bool quadratic=(std::isfinite(previous) && std::abs(step/lambda)<=.25*previous) ||
        std::abs(step/lambda)<64*std::numeric_limits<double>::epsilon();
      if(quadratic) {lambda=lambda_new;break;}
      // A starting point can already be at the arithmetic floor, leaving
      // no observable contraction. Signed endpoints and curvature then
      // establish accuracy directly in both returned coefficients.
      if(lomax_detail::local_enclosure(x,lambda,tol,profile))break;
    }
    previous=std::abs(step/lambda);
    lambda = lambda_new;
  }

  CPPUNIV_EXTENDED_REAL sum_log1p=0,correction=0;
  for(auto v:x) {
    const CPPUNIV_EXTENDED_REAL y=std::log1p(static_cast<CPPUNIV_EXTENDED_REAL>(lambda)*v)-correction;
    const CPPUNIV_EXTENDED_REAL t=sum_log1p+y;correction=(t-sum_log1p)-y;sum_log1p=t;
  }
  const double dn = static_cast<double>(n);
  const double kappa = dn / sum_log1p;
  const double logLik =
      dn * (std::log(kappa) + std::log(lambda) - 1.0 - 1.0 / kappa);
  if(iter>maxit)return {lambda,kappa,logLik,iter,LomaxStatus::IterationFailure};
  return {lambda, kappa, logLik, iter, LomaxStatus::Ok};
}

// The global certificate is separate from Newton proposal generation.
} // namespace cppunivariate
#include "certificate.hpp"
namespace cppunivariate {
inline LomaxResult lomax_mle(span x, double tol, int maxit,
    double lambda0 = std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  auto r=lomax_cert_detail::fit(x,tol,maxit,lambda0);
  if(r.status!=LomaxStatus::Ok)return r;
  const auto polished=lomax_newton_candidate(x,tol,maxit,r.lambda);
  if(polished.status!=LomaxStatus::Ok)return polished;
  auto out=polished;out.iterations+=r.iterations;return out;
}

inline LomaxResult lomax_mle(span x,double tol,int maxit,double lambda0,double lambda,double kappa) {
  maxit = static_cast<int>(solver_budget(maxit));
  if(std::isnan(lambda) && std::isnan(kappa))return lomax_mle(x,tol,maxit,lambda0);
  const double nan=std::numeric_limits<double>::quiet_NaN();
  if(x.size()==0 || !(tol>0 && std::isfinite(tol)) || maxit<=0 || (!std::isnan(lambda) && !(lambda>0 && std::isfinite(lambda))) ||
     (!std::isnan(kappa) && !(kappa>0 && std::isfinite(kappa))))return {nan,nan,nan,0,LomaxStatus::InvalidStart};
  for(double v:x)if(!(v>0 && std::isfinite(v)))return {nan,nan,nan,0,LomaxStatus::InvalidStart};
  tol=std::min(tol,1e-10);
  const double n=static_cast<double>(x.size());int iterations=0;
  if(std::isnan(lambda)) {
    // Strictly decreasing total log-rate score, computed with log weights.
    // An unrepresentable root is a numerical failure, never a capped estimate.
    const double lower=std::log(std::numeric_limits<double>::denorm_min()),upper=std::log(std::numeric_limits<double>::max());
    const double logfactor=std::log1p(kappa);
    double largest_log=0;
    std::vector<double> logs,small(x.size());
    for(double v:x){logs.push_back(std::log(v));largest_log=std::max(largest_log,std::abs(logs.back()));}
    const auto evaluate=[&](double t,double& error) {
      double high=0,maximum=-INFINITY;
      for(std::size_t i=0;i<x.size();++i) {
        const double z=t+logs[i];if(z>=0)++high;
        small[i]=logfactor-std::abs(z)-std::log1p(std::exp(-std::abs(z)));
        maximum=std::max(maximum,small[i]);
      }
      // The high observations alone give F <= n-(1+kappa)*high/2.
      if(high>0 && kappa>4*n/high) {error=0;return -1.;}
      const double constant=std::fma(-kappa,high,n-high);
      if(constant!=0)maximum=std::max(maximum,std::log(std::abs(constant)));
      CPPUNIV_EXTENDED_REAL sum=constant==0?0:std::copysign(std::exp(std::log(std::abs(constant))-maximum),constant),c=0,absolute=std::abs(sum);
      for(std::size_t i=0;i<x.size();++i) {
        const double term=std::copysign(std::exp(small[i]-maximum),t+logs[i]>=0?1.:-1.);
        const CPPUNIV_EXTENDED_REAL y=static_cast<CPPUNIV_EXTENDED_REAL>(term)-c,u=sum+y;
        c=(u-sum)-y;sum=u;absolute+=std::abs(term);
      }
      error=8*std::numeric_limits<double>::epsilon()*
        (1+std::abs(t)+largest_log+logfactor+std::abs(maximum))*static_cast<double>(absolute);
      return static_cast<double>(sum);
    };
    const auto positive=[&](double t){double e=0;const double g=evaluate(t,e);return g>e;};
    const auto negative=[&](double t){double e=0;const double g=evaluate(t,e);return g< -e;};
    double lo=std::max(lower,-std::log(x[0])-std::log(kappa)-2),hi=std::min(upper,lo+4),step=32;
    lo=std::min(lo,upper);
    for(std::int64_t j=0;j<solver_budget(maxit) && (!positive(lo) && lo>lower);++j){lo=std::max(lower,lo-step);step*=2;}
    step=32;
    for(std::int64_t j=0;j<solver_budget(maxit) && (!negative(hi) && hi<upper);++j){hi=std::min(upper,hi+step);step*=2;}
    if(!(positive(lo) && negative(hi)))return {nan,kappa,nan,iterations,LomaxStatus::IterationFailure};
    bool accurate=false,rounding=false;
    for(;iterations<maxit;) {
      if(hi-lo<=.5*tol) {accurate=true;break;}
      const double mid=lo+(hi-lo)/2;double error=0;
      const double g=evaluate(mid,error);++iterations;
      if(g>error)lo=mid;
      else if(g< -error)hi=mid;
      else {
        // Each independent endpoint probe spends a solver evaluation.
        if(maxit-iterations<2)break;
        iterations+=2;
        const double left=std::max(lo,mid-.24*tol),right=std::min(hi,mid+.24*tol);
        if(positive(left) && negative(right)) {lo=left;hi=right;accurate=true;}
        else rounding=true;
        break;
      }
    }
    if(!accurate && hi-lo<=.5*tol)accurate=true;
    if(!accurate) {
      const double rate=std::exp(lo+(hi-lo)/2);
      double ll=0;for(double v:x)ll+=std::log(rate)+std::log(kappa)-(kappa+1)*std::log1p(rate*v);
      LomaxResult out{rate,kappa,ll,iterations,rounding?LomaxStatus::AccuracyFailure:LomaxStatus::IterationFailure};
      if(rounding){if(out.lambda>0 && out.kappa>0)record_accuracy(out,tol,{rate,kappa},{rate,0});else{out.stop_detail.accuracy_stop=AccuracyStop::None;out.stop_detail.attainable_tol=NAN;}}
      return out;
    }
    // This raw-sign polish only chooses a point inside the already signed
    // enclosure. Even an exhausted polish retains the coefficient guarantee.
    double a=lo,b=hi;
    for(;iterations<maxit && b-a>.01*tol;++iterations) {
      const double mid=a+(b-a)/2;double error=0;const double g=evaluate(mid,error);
      if(g==0) {a=mid;b=mid;break;}
      if(g>0)a=mid;else b=mid;
    }
    const double selected=a+(b-a)/2;lambda=std::exp(selected);
    if(!(lambda>0 && std::isfinite(lambda)) || std::abs(std::log(lambda)-selected)>.25*tol)
      {double ll=0;for(double v:x)ll+=std::log(lambda)+std::log(kappa)-(kappa+1)*std::log1p(lambda*v);
      LomaxResult out{lambda,kappa,ll,iterations,LomaxStatus::AccuracyFailure};{if(out.lambda>0 && out.kappa>0)record_accuracy(out,tol,{lambda,kappa},{lambda,0});else{out.stop_detail.accuracy_stop=AccuracyStop::None;out.stop_detail.attainable_tol=NAN;}}return out;}
  }

  double s=0;for(double v:x){const double z=std::log(lambda)+std::log(v);s+=std::max(z,0.0)+std::log1p(std::exp(-std::abs(z)));}
  if(std::isnan(kappa))kappa=n/s;
  const double ll=n*(std::log(lambda)+std::log(kappa))-(kappa+1)*s;
  if(!(lambda>0 && std::isfinite(lambda) && kappa>0 && std::isfinite(kappa) && std::isfinite(ll)))
    return {lambda,kappa,ll,iterations,LomaxStatus::IterationFailure};
  return {lambda,kappa,ll,iterations,LomaxStatus::Ok};
}

}  // namespace cppunivariate

namespace cppunivariate {
inline FitOutcome outcome(const LomaxResult& r) {
  if (r.stop_detail.accuracy_stop == AccuracyStop::Tolerance) return FitOutcome::failure(OutcomeReason::Tolerance);
  if (r.stop_detail.accuracy_stop == AccuracyStop::Conditioning) return FitOutcome::failure(OutcomeReason::Conditioning);
  if (r.status != LomaxStatus::ExponentialLimit && r.certificate_failure == LomaxCertificateFailure::Budget) return FitOutcome::budget();
  if (r.status != LomaxStatus::ExponentialLimit && r.certificate_failure == LomaxCertificateFailure::Arithmetic) return FitOutcome::failure(OutcomeReason::Evaluation);
  switch (r.status) {
    case LomaxStatus::Ok: return FitOutcome::estimate(true);
    case LomaxStatus::ExponentialLimit: return FitOutcome::limit(false);
    case LomaxStatus::InvalidStart: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case LomaxStatus::IterationFailure: return FitOutcome::failure(OutcomeReason::NoConvergence);
    case LomaxStatus::AccuracyFailure: return FitOutcome::failure(OutcomeReason::Tolerance);
  }
  return {};
}
} // namespace cppunivariate
