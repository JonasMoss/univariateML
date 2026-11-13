#pragma once
#include "../../core/outcome.hpp"
#include "../../core/work_budget.hpp"
#include "../../core/precision.hpp"
#include "../../core/span.hpp"
#include "../../core/stop_detail.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>

namespace cppunivariate {
inline constexpr double kGpdXiLower = -1.0;

enum class GpdStatus { Ok, InvalidData, InvalidShape, IterationFailure };
enum class GpdCertificateFailure { None, Budget, Arithmetic, Tie, ThresholdCollapse };
struct GpdResult {
  double mu, sigma, xi, logLik;
  int iterations;
  GpdStatus status; unsigned restricted = 0;
  GpdCertificateFailure certificate_failure = GpdCertificateFailure::None;
  StopDetail stop_detail{};
};
// log1p(xi*z)/xi, including the interior exponential member.
inline double gpd_log_ratio(double z, double xi) {
  const double v = xi*z;
  if (std::abs(v) < 1e-5)
    return z*(1-v/2+v*v/3-v*v*v/4+v*v*v*v/5);
  return std::log1p(v)/xi;
}
inline double gpd_loglik(span y, double sigma, double xi) {
  if (!(sigma>0)) return -std::numeric_limits<double>::infinity();
  if (xi == -1) {
    for (double v:y) if (v>sigma) return -std::numeric_limits<double>::infinity();
    return -static_cast<double>(y.size())*std::log(sigma);
  }
  double ll = -static_cast<double>(y.size())*std::log(sigma);
  for (double v : y) {
    if (!(sigma > 0) || !(1+xi*(v/sigma) > 0))
      return -std::numeric_limits<double>::infinity();
    ll -= (1+xi)*gpd_log_ratio(v/sigma, xi);
  }
  return ll;
}
// For fixed xi > -1 the scale score is strictly decreasing in sigma
// after multiplication by sigma: (1+xi)*mean(y/(sigma+xi*y))-1.
inline GpdResult gpd_scale(span y, double mu, double xi, double tol, int maxit) {
  maxit = static_cast<int>(solver_budget(maxit));
  double M=0;
  for(double v:y)M=std::max(M,v);
  auto failure=[&](int work) {
    GpdResult out{mu,NAN,xi,NAN,work,GpdStatus::IterationFailure};
    out.certificate_failure=GpdCertificateFailure::Arithmetic;
    const double candidate=gpd_loglik(y,M,xi);
    out.stop_detail.loglik_lower=candidate;
    out.stop_detail.loglik_upper=INFINITY;
    if(std::isfinite(candidate))out.stop_detail.candidates.push_back({{mu,M,xi},candidate,candidate});
    return out;
  };
  if(!(M>0))return failure(0);
  double m=0,mean_correction=0;
  for(double v:y) {
    const double term=(v/M)/y.size()-mean_correction,total=m+term;
    mean_correction=(total-m)-term;m=total;
  }
  if(xi==-1)return {mu,M,xi,gpd_loglik(y,M,xi),0,GpdStatus::Ok};
  if(xi==0) {
    const double scale=M*m,ll=gpd_loglik(y,scale,xi);
    if(!(scale>0) || !std::isfinite(scale) || !std::isfinite(ll) ||
        std::abs(scale/M-m)>1e-8*m)return failure(0);
    return {mu,scale,xi,ll,0,GpdStatus::Ok};
  }
  // Work in max-excess units, so scale bracketing cannot overflow merely
  // because the input was rescaled. Score signs remain conditional on the
  // stated binary64 allowance; ambiguous comparisons stop.
  double lo=std::max(0.,-xi),hi=std::max(1.,2*lo);
  const double eta=512*std::numeric_limits<double>::epsilon()*(1+std::log(y.size()+1.));
  auto score=[&](double scale) {
    CPPUNIV_EXTENDED_REAL value=0,correction=0;
    for(double v:y) {
      if(v==0)continue;
      const CPPUNIV_EXTENDED_REAL r=static_cast<CPPUNIV_EXTENDED_REAL>(v)/M;
      const CPPUNIV_EXTENDED_REAL term=(1+static_cast<CPPUNIV_EXTENDED_REAL>(xi))*r/(scale+static_cast<CPPUNIV_EXTENDED_REAL>(xi)*r)/y.size()-correction;
      const CPPUNIV_EXTENDED_REAL total=value+term;
      correction=(total-value)-term;value=total;
    }
    return static_cast<double>(value)-1;
  };
  int work=0;
  while(std::isfinite(hi) && score(hi)>=-eta && work<maxit){hi*=2;++work;}
  if(!std::isfinite(hi) || !(score(hi)<-eta))return failure(work);
  const double target=std::min(tol,1e-8);
  for(;work<maxit;++work) {
    const double mid=lo+(hi-lo)/2,value=score(mid);
    if(value>eta)lo=mid;
    else if(value<-eta)hi=mid;
    else {
      bool signed_box=false;
      double radius=std::max(2e-12,8*eta)*mid;
      for(int k=0;k<maxit;++k,radius*=2) {
        const double l=mid-radius,h=mid+radius;
        if(l<=lo || h>=hi)break;
        if(score(l)>eta && score(h)<-eta){lo=l;hi=h;signed_box=true;break;}
      }
      if(!signed_box)return failure(work);
      // The enclosure certifies coefficients to 1e-7 relative accuracy;
      // tighten the representative independently below.
      if(hi-lo<=1e-7*mid)break;
    }
    if(hi-lo<=1e-8*hi)break;
  }
  if(work>=maxit) {
    auto out=failure(work);out.certificate_failure=GpdCertificateFailure::Budget;return out;
  }
  double center=(lo+hi)/2;
  // Nominate a more accurate representative inside the signed enclosure.
  double l=lo,h=hi;
  for(int k=0;k<maxit && h-l>target*h;++k) {
    center=(l+h)/2;
    if(score(center)>0)l=center;else h=center;
  }
  const double scale=M*((l+h)/2),ll=gpd_loglik(y,scale,xi);
  if(!(scale>0) || !std::isfinite(scale) || !std::isfinite(ll))return failure(work);
  if(std::abs(scale/M-(l+h)/2)>1e-8*((l+h)/2))return failure(work);
  return {mu,scale,xi,ll,work+1,GpdStatus::Ok};
}
} // namespace cppunivariate
#include "certificate.hpp"
namespace cppunivariate {
inline GpdResult gpd_mle_impl(span x, double tol=1e-12, int maxit=200,
                         double mu=0, double xi=std::numeric_limits<double>::quiet_NaN(),
                         double sigma=std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  GpdResult bad{mu,0,xi,0,0,GpdStatus::InvalidData};
  if (x.size()==0 || !std::isfinite(mu) || !(tol>0) || !std::isfinite(tol) || maxit<1) return bad;
  std::vector<double> y;
  double M=0, minimum=std::numeric_limits<double>::infinity();
  for (double v : x) {
    const double z=v-mu;
    if (!(z>=0) || !std::isfinite(z)) return bad;
    y.push_back(z); M=std::max(M,z); minimum=std::min(minimum,z);
  }
  // At the closed threshold, positive-shape scale collapse has exponent
  // (n-k)/xi-k. With free shape any threshold tie permits this path.
  if(std::isnan(sigma)) {
    const double k=static_cast<double>(std::count(y.begin(),y.end(),0.0));
    if(M==0 || (k>0 && (std::isnan(xi) || (xi>0 && xi*k>=y.size()-k)))) {
      bad.certificate_failure=GpdCertificateFailure::ThresholdCollapse;
      bad.stop_detail.loglik_upper=INFINITY;
      if(M>0 && !std::isnan(xi) && xi>0 && xi*k==y.size()-k) {
        double limit=0;
        for(double v:y)if(v>0)limit-=(1+1/xi)*(std::log(xi)+std::log(v));
        bad.stop_detail.loglik_upper=limit;
        bad.stop_detail.limit_loglik=limit;
      }
      return bad;
    }
  }
  const span ys{y.data(),y.size()};
  if(!std::isnan(sigma)) {
    if(!(sigma>0) || !std::isfinite(sigma)) return bad;
    if(!std::isnan(xi)) {
      if(!std::isfinite(xi)) {bad.status=GpdStatus::InvalidShape;return bad;}
      const double ll=gpd_loglik(ys,sigma,xi);
      return {mu,sigma,xi,ll,0,std::isfinite(ll) ? GpdStatus::Ok : GpdStatus::InvalidData};
    }
    return gpd_cert_detail::fit(ys,mu,tol,maxit,sigma);
  }
  if (!std::isnan(xi)) {
    if (!std::isfinite(xi) || xi < -1) { bad.status=GpdStatus::InvalidShape; return bad; }
    return gpd_scale(ys,mu,xi,tol,maxit);
  }
  return gpd_cert_detail::fit(ys,mu,tol,maxit);
}

inline GpdResult gpd_mle(span x,double tol=1e-12,int maxit=200,double mu=0,double xi=std::numeric_limits<double>::quiet_NaN(),double sigma=std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  auto r=gpd_mle_impl(x,tol,maxit,mu,xi,sigma);
  if(r.status==GpdStatus::Ok && (std::isnan(xi)) && r.xi==kGpdXiLower) r.restricted|=16u;
  return r;
}
} // namespace cppunivariate

namespace cppunivariate {
inline FitOutcome outcome(const GpdResult& r) {
  if (r.certificate_failure == GpdCertificateFailure::Budget) return FitOutcome::budget();
  if (r.certificate_failure == GpdCertificateFailure::Arithmetic) return FitOutcome::failure(OutcomeReason::Evaluation);
  if (r.certificate_failure == GpdCertificateFailure::Tie) return FitOutcome::estimate(false);
  if (r.certificate_failure == GpdCertificateFailure::ThresholdCollapse) return std::isfinite(r.stop_detail.limit_loglik) ? FitOutcome::limit() : FitOutcome::nonexistence(OutcomeReason::Unbounded);
  switch (r.status) {
    case GpdStatus::Ok: return FitOutcome::estimate(true);
    case GpdStatus::InvalidData: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case GpdStatus::InvalidShape: return FitOutcome::failure(OutcomeReason::InvalidArgument);
    case GpdStatus::IterationFailure: return FitOutcome::failure(OutcomeReason::NoConvergence);
  }
  return {};
}
} // namespace cppunivariate
