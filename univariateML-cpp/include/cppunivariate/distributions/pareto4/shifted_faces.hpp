#pragma once

namespace cppunivariate {
namespace pareto4_detail {

inline BurrMLE shifted_power_fit(span x,double min,double b,double tol,int maxit) {
  namespace cert=burr_cert_detail;
  BurrMLE out{NAN,b,NAN,NAN,0,BurrStatus::NumericalFailure};
  const double anchor=*std::max_element(x.begin(),x.end()), y0=anchor-min;
  const double residual=excess_residual(anchor,min,y0);
  std::vector<double> powered;
  std::vector<cert::Interval> log_powered;
  cert::Sum powered_sum;
  double lower=1,total=0;
  for(double v:x) {
    const double l=centered_log(v,anchor,min), t=b*l;
    const double u=std::exp(t);
    if(!(u>0) || !std::isfinite(u))return out;
    const double eps=std::numeric_limits<double>::epsilon();
    const double log_error=16*eps*(std::abs((v-anchor)/y0)<.5?std::abs(l):
        std::abs(std::log(v-min))+std::abs(std::log(y0))+std::abs(l));
    const double error=b*log_error+16*eps*(1+std::abs(t));
    const cert::Interval log_box(t-error,t+error);
    const auto power_box=cert::exp(log_box);
    if(!power_box.valid())return out;
    lower=std::min(lower,std::max(0.,power_box.lo));
    total+=u;
    powered_sum.add(power_box);
    log_powered.push_back(log_box);
    powered.push_back(u);
  }
  const double n=x.size();
  const span y{powered.data(),powered.size()};
  // The range bound excludes every finite rate. For two observations,
  // increasing the powered ratio increases the profile gap at every rate,
  // so a deliberately enlarged ratio can also prove the exponential face.
  bool face=lower>.5;
  if(!face && x.size()==2 && lower>0) {
    const std::array<double,2> widened{{lower,1}};
    face=fallback_lomax_face_proved(span{widened.data(),widened.size()},maxit);
  }
  const auto restore=[&](double offset) {
    return y0+std::fma(y0,std::expm1(offset),residual);
  };
  if(face) {
    const double mean=total/n;
    const double scale=restore(std::log(mean)/b);
    if(!(scale>0) || !std::isfinite(scale))return out;
    double ll=0;
    for(double v:x) {
      const double t=b*shifted_log(v,min,scale);
      ll+=std::log(b)-std::log(scale)+t-t/b-std::exp(t);
    }
    if(!std::isfinite(ll))return out;
    out.status=BurrStatus::WeibullBoundary;
    out.stop_detail=limit_detail({b,scale},ll,StopModel::Weibull);
    return out;
  }
  auto r=lomax_mle(y,tol,maxit);
  if(r.status!=LomaxStatus::Ok)r=lomax_newton_candidate(y,tol,maxit);
  if(!fallback_lomax_search_accepted(y,r))return out;
  // The exponential face is always reachable. Its lower bound can
  // refute a finite search even without an exhaustive comparison.
  cert::Sum sum;
  for(auto l:log_powered)sum.add(cert::sp(cert::log(cert::Interval(r.lambda))+l));
  const auto g=sum.get()/n;
  const auto gain=cert::log(cert::Interval(r.lambda))+cert::log(powered_sum.get()/n)-cert::log(g)-g;
  if(gain.valid() && gain.hi<0)return out;
  out.shape1=r.kappa;
  out.scale=restore(-std::log(r.lambda)/b);
  out.iterations=r.iterations;
  out.certified=false;
  if(out.shape1>0 && out.scale>0 && std::isfinite(out.scale))out.status=BurrStatus::Ok;
  return out;
}

inline double pareto_endpoint_low(span x,double min,double high) {
  const double first=*std::min_element(x.begin(),x.end());
  const double excess=first-min;
  return (excess-high)+excess_residual(first,min,excess);
}

// Work directly on relative log excesses of the original observations.
// The global comparison is made before projecting scale back to binary64.
inline BurrMLE shifted_joint_fit(span x,double min,double tol,int maxit) {
  namespace cert=burr_cert_detail;
  BurrMLE out{NAN,NAN,NAN,NAN,0,BurrStatus::NumericalFailure};
  if(x.empty() || maxit<=1 || !burr_valid_controls(tol,maxit))return out;
  const double least=*std::min_element(x.begin(),x.end());
  const double greatest=*std::max_element(x.begin(),x.end());
  if(least==greatest) {
    out.status=BurrStatus::Unbounded;
    out.logLik=INFINITY;
    return out;
  }
  std::vector<double> sorted(x.begin(),x.end());
  std::nth_element(sorted.begin(),sorted.begin()+sorted.size()/2,sorted.end());
  const double ref=sorted[sorted.size()/2];
  cert::Data data;
  data.maxit=maxit;
  data.anchor=ref-min;
  data.anchor_low=excess_residual(ref,min,data.anchor);
  const double log_anchor=std::log(data.anchor)+std::log1p(data.anchor_low/data.anchor);
  std::vector<std::pair<double,double>> logs;
  int multiplicity=0;
  double pareto_distance=0;
  for(double v:x) {
    const double z=centered_log(v,ref,min);
    const double margin=32*std::numeric_limits<double>::epsilon();
    const double mag=std::abs((v-ref)/data.anchor)<.5?std::abs(z):
        std::abs(std::log(v-min))+std::abs(log_anchor)+std::abs(z);
    const double error=margin*mag+std::numeric_limits<double>::denorm_min();
    logs.emplace_back(z,error);
    multiplicity+=v==least;
    pareto_distance+=centered_log(v,least,min);
  }
  if(!cert::standardize(data,logs,log_anchor))return out;
  const double endpoint=least-min;
  const double low=excess_residual(least,min,endpoint);
  const double supported=low<0?std::nextafter(endpoint,0.):endpoint;
  const auto recenter=[&](const cert::Proposal& raw,const cert::Proposal& proposal,
                          cert::Data& local,double& shift) {
    shift=data.center-proposal.b/raw.c;
    if(!std::isfinite(shift))return false;
    for(const auto& p:logs) {
      local.y.push_back(p.first-shift);
      local.box.push_back(cert::padded(p.first-p.second,p.first+p.second)-shift);
    }
    return true;
  };
  const auto result=cert::fit_data(data,supported,multiplicity,
      certificate_budget(maxit),tol,recenter);
  out.iterations=result.profiles;
  if(result.outcome==cert::Outcome::Finite) {
    out.shape1=result.k;
    out.shape2=result.c;
    out.scale=result.scale;
    out.logLik=burr_local_loglik(x,out.shape1,out.shape2,out.scale,false,min);
    if(!std::isfinite(out.logLik))return out;
    out.status=BurrStatus::Ok;
    out.certified=result.accurate;
    return out;
  }
  if(result.outcome==cert::Outcome::Pareto) {
    const double exponent=x.size()/pareto_distance;
    const double ll=x.size()*(std::log(exponent)-1-std::log(endpoint)-
                              std::log1p(low/endpoint))-pareto_distance;
    if(!std::isfinite(exponent) || !std::isfinite(ll))return out;
    out={0,INFINITY,supported,ll,result.profiles,BurrStatus::ParetoBoundary};
    out.stop_detail=limit_detail({exponent,supported},ll,StopModel::Pareto);
    if(out.stop_detail.candidates.empty()) {
      out.status=BurrStatus::NumericalFailure;
      return out;
    }
    const auto& face=result.stop_detail.candidates[0];
    const double margin=64*std::numeric_limits<double>::epsilon()*(x.size()+std::abs(ll));
    const double lower=std::min(ll,face.loglik_lower-margin);
    const double upper=std::max(ll,face.loglik_upper+margin);
    out.stop_detail.loglik_lower=lower;
    out.stop_detail.loglik_upper=upper;
    out.stop_detail.candidates[0].loglik_lower=lower;
    out.stop_detail.candidates[0].loglik_upper=upper;
    return out;
  }
  if(result.outcome==cert::Outcome::Weibull) {
    const auto& face=result.stop_detail.candidates[1];
    double ll=0;
    for(double v:x) {
      const double z=result.c*shifted_log(v,min,result.scale);
      ll+=std::log(result.c)-std::log(result.scale)+z-z/result.c-std::exp(z);
    }
    if(!std::isfinite(ll))return out;
    out={INFINITY,result.c,result.scale,ll,result.profiles,BurrStatus::WeibullBoundary};
    out.stop_detail=limit_detail({result.c,result.scale},ll,StopModel::Weibull);
    out.stop_detail.loglik_lower=face.loglik_lower;
    out.stop_detail.loglik_upper=face.loglik_upper;
    const double margin=64*std::numeric_limits<double>::epsilon()*(x.size()+std::abs(ll));
    out.certified=ll>=face.loglik_lower-margin && ll<=face.loglik_upper+margin;
  }
  return out;
}
// With exactly two distinct log excesses there is an increasing affine map
// between the exact and rounded logs. The full Burr profile, including
// both limiting heights, is invariant up to its data Jacobian.
inline BurrMLE shifted_two_support_pareto(span x,double min,const BurrMLE& parent) {
  const auto range=std::minmax_element(x.begin(),x.end());
  const double anchor=*range.first, endpoint=anchor-min;
  const double residual=excess_residual(anchor,min,endpoint);
  double total=0;
  for(double v:x) {
    if(v!=anchor && v!=*range.second)return parent;
    total+=centered_log(v,anchor,min);
  }
  if(!(total>0))return parent;
  const double n=x.size(), alpha=n/total;
  const double ll=n*(std::log(alpha)-1-std::log(endpoint)-std::log1p(residual/endpoint))-total;
  if(!std::isfinite(ll))return {NAN,NAN,NAN,NAN,0,BurrStatus::NumericalFailure};
  const double supported_endpoint=residual<0?std::nextafter(endpoint,0.):endpoint;
  BurrMLE out{0,INFINITY,supported_endpoint,ll,parent.iterations,BurrStatus::ParetoBoundary};
  out.stop_detail=limit_detail({alpha,supported_endpoint},ll,StopModel::Pareto);
  return out;
}
}
}
