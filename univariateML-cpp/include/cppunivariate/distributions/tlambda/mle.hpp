#pragma once
#include "../../core/maximizer_set.hpp"
#include <string_view>
#include "../../core/outcome.hpp"
#include "../../core/work_budget.hpp"
#include "../../core/stop_detail.hpp"
#include "dpqr.hpp"
#include "fit_inverse.hpp"
#include "../../core/span.hpp"
#include <algorithm>
#include <array>
#include <cmath>
#include <limits>
#include <vector>
namespace cppunivariate {
inline constexpr double kTlambdaLambdaLower = -1.0;
inline constexpr double kTlambdaLambdaUpper = 1.0;

enum class TlambdaStatus { Ok, InvalidData, InvalidShape, IterationFailure, Unbounded, CollapseLimit, InvalidSupport, CertificateFailure, TransformFailure };
struct TlambdaResult { double mu,sigma,lambda,logLik; int iterations; TlambdaStatus status; unsigned restricted = 0; bool certified=false; double certificate_lower=NAN,certificate_upper=NAN; StopDetail stop_detail={};
  MaximizerSet maximizers{}; bool singleton_proved = false; };
namespace tlambda_detail {
inline TlambdaResult uniform_fit(double lo,double hi,double n,double lambda,int maxit,double tol=1e-10) {
  maxit = static_cast<int>(solver_budget(maxit));
  const double gap=hi-lo;
  const double mu=std::isfinite(gap)?lo+gap/2:lo/2+hi/2;
  double sigma=std::max(mu-lo,hi-mu)*lambda;
  for(int j=0;j<maxit && valid(mu,sigma,lambda) &&
      (slack(lo,mu,sigma,lambda)<0 || slack(hi,mu,sigma,lambda)<0);++j)
    sigma=std::nextafter(sigma,INFINITY);
  const double ll=-n*(std::log(sigma)+std::log(2/lambda));
  const double ideal=-n*logstandard(hi,lo,1);
  const double pad=512*std::numeric_limits<double>::epsilon()*(n+std::abs(ll)+std::abs(ideal));
  // A rounded midpoint can require a materially wider interval. Never call
  // that represented interval the exact uniform optimum merely because it fits.
  const bool ok=valid(mu,sigma,lambda) && std::isfinite(ll) && std::isfinite(ideal) &&
      slack(lo,mu,sigma,lambda)>=0 && slack(hi,mu,sigma,lambda)>=0 && ideal-ll<=n*std::max(tol,1e-7)+pad;
  TlambdaResult out{mu,sigma,lambda,ll,0,ok?TlambdaStatus::Ok:TlambdaStatus::CertificateFailure};
  if(!ok && valid(mu,sigma,lambda) && std::isfinite(ll) &&
      slack(lo,mu,sigma,lambda)>=0 && slack(hi,mu,sigma,lambda)>=0) {
    out.stop_detail.candidates.push_back({{mu,sigma,lambda},ll-pad,ll+pad});
    out.stop_detail.loglik_lower=ll-pad;out.stop_detail.loglik_upper=INFINITY;
    out.stop_detail.segment_loglik_upper=ideal+pad;
  }
  return out;
}
// A constant sample with held scale maximizes at translations of a uniform
// member, or at both support contacts for a held shape strictly between 1 and 2.
inline TlambdaResult constant_location(double value,std::size_t n,double sigma,double lambda,double tol) {
  const bool free_shape=std::isnan(lambda);
  if(free_shape)lambda=kTlambdaLambdaUpper;
  const bool uniform=lambda==1 || lambda==2;
  TlambdaResult out{value,sigma,lambda,NAN,0,TlambdaStatus::CertificateFailure};
  if(!uniform && !(lambda>1 && lambda<2))return out;
  const double width=sigma/lambda;
  const double left=value-width,right=value+width;
  if(!uniform) {
    // Use the same rounded support contacts as the public density. When
    // the affine residual is nonzero, also bound the coefficient error.
    const auto contact=[&](double mu,double image) {
      if(!std::isfinite(mu))return false;
      if(slack(value,mu,sigma,lambda)==0)return true;
      if(image!=value)return false;
      const double distance=std::abs(value-mu);
      const double delta=std::abs(std::fma(lambda,distance,-sigma))/lambda;
      const double pad=4*std::numeric_limits<double>::epsilon()*distance;
      return delta+pad<=tol*std::abs(mu);
    };
    if(!(left<value && value<right) || !contact(left,left+width) || !contact(right,right-width))return out;
    out.mu=left;
  }
  out.logLik=-static_cast<double>(n)*(std::log(sigma)+(lambda==1?std::log(2.):0));
  out.status=TlambdaStatus::Ok;
  out.certified=true;
  if(free_shape)out.restricted|=32u;
  // A uniform member attains its peak on its entire closed support.
  // For 1 < lambda < 2 strict concavity of the quantile derivative
  // gives equality only at the two support contacts.
  const auto rounded = [&](double endpoint, double offset) {
    const double back = endpoint - value;
    const double residual = (value - (endpoint - back)) + (offset - back);
    return !std::isfinite(endpoint) || residual != 0 ||
        std::fma(width, lambda, -sigma) != 0 ||
        (lambda != 1 && width+width != sigma && lambda == 2);
  };
  out.maximizers.dimension = 3;
  if (uniform) {
    Box piece;
    piece.axes = {interval(left, right, std::isfinite(left), std::isfinite(right)), fixed(sigma), fixed(lambda)};
    piece.axes[0].range.lower_rounded = rounded(left, -width);
    piece.axes[0].range.upper_rounded = rounded(right, width);
    if (out.maximizers.add_box(piece) != MaxsetStatus::Ok) out.status = TlambdaStatus::CertificateFailure;
  } else {
    const auto point = [&](double endpoint, double offset) {
      Point result{{{endpoint, sigma, lambda}}, {}};
      if (rounded(endpoint, offset)) {
        const double error = 4*std::numeric_limits<double>::epsilon()*width;
        const double lower = std::nextafter(endpoint-error, -INFINITY);
        const double upper = std::nextafter(endpoint+error, INFINITY);
        result.enclosure = Enclosure{{{lower, sigma, lambda}}, {{upper, sigma, lambda}}};
      }
      return result;
    };
    const std::array<Point, 2> points{point(left, -width), point(right, width)};
    for (const auto& p : points) if (p.enclosure) {
      const double m = p.estimate.values[0];
      const double error = std::max(m-p.enclosure->lower.values[0], p.enclosure->upper.values[0]-m);
      if (!(error <= tol*std::abs(m))) out.status = TlambdaStatus::CertificateFailure;
    }
    if (out.maximizers.add_points({points.data(), points.size()}) != MaxsetStatus::Ok)
      out.status = TlambdaStatus::CertificateFailure;
  }
  return out;
}
inline double likelihood(span x,double mu,double sigma,double lambda) {
  double ll=0;for(double v:x)ll+=distributions::dtlambda(v,mu,sigma,lambda,true);return ll;
}
}
#include "profile.hpp"
inline TlambdaResult tlambda_mle_impl(span x,double tol=1e-10,int maxit=1000,double lambda=NAN) {
  return tlambda_fit_detail::profile(x,tol,maxit,lambda);
}
inline TlambdaResult tlambda_mle_impl(span x,double tol,int maxit,double lambda,double mu,double sigma) {
  return tlambda_fit_detail::profile(x,tol,maxit,lambda,mu,sigma);
}


#include "certificate.hpp"

inline TlambdaResult tlambda_mle(span x,double tol=1e-10,int maxit=1000,double lambda=std::numeric_limits<double>::quiet_NaN()) {
  if(!std::isnan(lambda)&&!std::isfinite(lambda))return {NAN,NAN,NAN,NAN,0,TlambdaStatus::InvalidShape};
  if(!(tol>0 && std::isfinite(tol)) || maxit<1 || x.empty() || !std::all_of(x.begin(),x.end(),[](double v){return std::isfinite(v);}))return {NAN,NAN,NAN,NAN,0,TlambdaStatus::InvalidData};

  if(tol>0 && std::isfinite(tol) && maxit>0 && !x.empty() && std::isfinite(x[0]) &&
      (true) && std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];}))
    return {NAN,NAN,NAN,NAN,0,TlambdaStatus::Unbounded};

  maxit = static_cast<int>(solver_budget(maxit));
  auto r=tlambda_cert_detail::fit(x,tol,maxit,lambda);
  if(r.status==TlambdaStatus::InvalidData && tol>0 && std::isfinite(tol) && maxit>0 && !x.empty() &&
      std::all_of(x.begin(),x.end(),[](double v){return std::isfinite(v);}))r.status=TlambdaStatus::TransformFailure;

  if(r.status==TlambdaStatus::Ok && (std::isnan(lambda)) && r.lambda==kTlambdaLambdaLower) r.restricted|=16u;
  if(r.status==TlambdaStatus::Ok && (std::isnan(lambda)) && r.lambda==kTlambdaLambdaUpper) r.restricted|=32u;
  return r;
}
inline TlambdaResult tlambda_mle(span x,double tol,int maxit,double lambda,double mu,double sigma) {
  if(!std::isnan(lambda)&&!std::isfinite(lambda))return {NAN,NAN,NAN,NAN,0,TlambdaStatus::InvalidShape};
  if(!(tol>0 && std::isfinite(tol)) || maxit<1 || x.empty() || !std::all_of(x.begin(),x.end(),[](double v){return std::isfinite(v);}))return {NAN,NAN,NAN,NAN,0,TlambdaStatus::InvalidData};

  if(tol>0 && std::isfinite(tol) && maxit>0 && !x.empty() && std::isfinite(x[0]) &&
      (std::isnan(sigma) && (std::isnan(mu)||mu==x[0])) && std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];}))
    return {NAN,NAN,NAN,NAN,0,TlambdaStatus::Unbounded};

  maxit = static_cast<int>(solver_budget(maxit));
  if(std::isnan(mu) && sigma>0 && std::isfinite(sigma) &&
      std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];})) {
    const auto flat=tlambda_detail::constant_location(x[0],x.size(),sigma,lambda,tol);
    if(flat.status==TlambdaStatus::Ok)return flat;
    // A uniform center is exact even when the second witness cannot be represented.
    if(std::isnan(lambda) || lambda==1 || lambda==2) {
      auto stop=flat;
      stop.stop_detail.accuracy_stop=AccuracyStop::Conditioning;
      return stop;
    }
  }
  auto r=tlambda_cert_detail::fit(x,tol,maxit,lambda,mu,sigma);
  if(r.status==TlambdaStatus::InvalidData && tol>0 && std::isfinite(tol) && maxit>0 && !x.empty() &&
      std::all_of(x.begin(),x.end(),[](double v){return std::isfinite(v);}))r.status=TlambdaStatus::TransformFailure;

  if(r.status==TlambdaStatus::Ok && (std::isnan(lambda) && std::isnan(mu)) && r.lambda==kTlambdaLambdaLower) r.restricted|=16u;
  if(r.status==TlambdaStatus::Ok && (std::isnan(lambda)) && r.lambda==kTlambdaLambdaUpper) r.restricted|=32u;
  return r;
}

inline bool family_point(const TlambdaResult&, const FitCandidate& c) {
  return c.model == StopModel::None && c.coef.size() == 3 && std::isfinite(c.coef[0]) &&
         std::isfinite(c.coef[1]) && std::isfinite(c.coef[2]) && c.coef[1] > 0;
}
// Support-contact ties list certificate points with likelihood bounds.
inline bool accurate_point(const TlambdaResult&, const FitCandidate&) { return false; }

inline FitOutcome outcome(const TlambdaResult& r) {
  if (r.status == TlambdaStatus::CertificateFailure) {
    if (r.stop_detail.budget_exhausted) return FitOutcome::budget();
    if (r.stop_detail.comparison_tie) return comparison_outcome_of(r);
  }
  if (r.stop_detail.accuracy_stop == AccuracyStop::Tolerance) return FitOutcome::failure(OutcomeReason::Tolerance);
  if (r.stop_detail.accuracy_stop == AccuracyStop::Conditioning) return FitOutcome::failure(OutcomeReason::Conditioning);
  switch (r.status) {
    case TlambdaStatus::TransformFailure:return FitOutcome::failure(OutcomeReason::Evaluation);
    case TlambdaStatus::Ok: return r.maximizers.size ? FitOutcome::estimate(r.certified, false) :
        (r.singleton_proved ? FitOutcome::singleton(r.certified) : FitOutcome::unknown_cardinality(r.certified));
    case TlambdaStatus::InvalidData: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case TlambdaStatus::InvalidShape: return FitOutcome::failure(OutcomeReason::InvalidArgument);
    case TlambdaStatus::IterationFailure: return FitOutcome::failure(OutcomeReason::NoConvergence);
    case TlambdaStatus::Unbounded: return FitOutcome::nonexistence(OutcomeReason::Unbounded);
    case TlambdaStatus::CollapseLimit: return FitOutcome::nonexistence(OutcomeReason::Unattained);
    case TlambdaStatus::InvalidSupport: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case TlambdaStatus::CertificateFailure: return FitOutcome::failure(OutcomeReason::Evaluation);
  }
  return {};
}


}
