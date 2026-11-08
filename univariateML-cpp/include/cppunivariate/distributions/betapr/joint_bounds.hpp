// Uniform joint-shape exteriors. These bounds do not certify an interior search.
#pragma once
#include "../../core/span.hpp"
#include "../../core/work_budget.hpp"
#include "../../special/special.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>
namespace cppunivariate {
enum class BetaPrimeJointBoundStatus { Unavailable, Geometry, FiniteExterior,
                                      Budget, Arithmetic };
struct BetaPrimeJointBounds {
  BetaPrimeJointBoundStatus status=BetaPrimeJointBoundStatus::Unavailable;
  // Every point beating witness_lower has harmonic shape h in (lower,upper).
  double witness_lower=NAN, witness_upper=NAN, harmonic_lower=NAN, harmonic_upper=NAN;
  double incumbent_lower=NAN;
  // Upper bounds on the gamma/inverse-gamma faces over retained shapes.
  double gamma_upper=INFINITY, invgamma_upper=INFINITY;
  // If finite, q>=gamma_shape2 or p>=invgamma_shape1 cannot beat the witness.
  double gamma_shape2=INFINITY, invgamma_shape1=INFINITY;
  double gamma_tail_upper=INFINITY, invgamma_tail_upper=INFINITY;
  // Within retained h, these tails lie below the corresponding face supremum.
  // They do not assert that the face beats the finite interior.
  double gamma_signed_shape2=INFINITY, invgamma_signed_shape1=INFINITY;
  double logmean=NAN, pair_loss=NAN;
  int evaluations=0;
  bool global_certified=false;
  std::int64_t total_evaluations=0,cover_cells=0,profile_evaluations=0;
  int retained_maxima=0;
  std::vector<double> coefficient_lower,coefficient_upper;
  bool excludes(double p,double q) const {
    if(!(p>0)||!(q>0)||!std::isfinite(p)||!std::isfinite(q))return false;
    const double h=p<=q?p/(1+p/q):q/(1+q/p);
    return (std::isfinite(harmonic_lower)&&h<=harmonic_lower)||
      (std::isfinite(harmonic_upper)&&h>=harmonic_upper)||
      (std::isfinite(gamma_shape2)&&q>=gamma_shape2)||
      (std::isfinite(invgamma_shape1)&&p>=invgamma_shape1);
  }
};
namespace betapr_joint_detail {
struct Sum {
  double value=0,correction=0;
  void add(double x){const double y=x-correction,t=value+y;correction=(t-value)-y;value=t;}
};
inline double softplus(double x) {
  return std::max(x,0.)+std::log1p(std::exp(-std::abs(x)));
}
// For h>=H>=n/(2J), this is decreasing in h, uniformly over scale.
inline double large_upper(double h,double n,double logmean,double loss) {
  return n*(.5*(std::log(h)-std::log(2*3.14159265358979323846))+
    1/(48*h)-logmean)-h*loss;
}
// Gamma(shape a, freely profiled scale) is strictly concave in a.
// A supporting tangent bounds its maximum on [lo,hi], even if the root solve
// has not converged. Bisection only tightens that bound.
struct GammaTangent {double upper=INFINITY,lower=-INFINITY,shape=NAN;};
inline GammaTangent gamma_tangent(double lo,double hi,double logmean,
    double log_arithmetic_mean,double n,int maxit,int& evaluations,
    std::int64_t limit=std::numeric_limits<std::int64_t>::max()) {
  if(!(lo>0)||!(hi>=lo)||!std::isfinite(hi))return {};
  double left=lo,right=hi;
  for(int j=0;j<maxit;++j) {
    if(evaluations>=limit)return {};
    const double a=left+(right-left)/2;
    const double score=std::log(a)-special::digamma(a)+logmean-log_arithmetic_mean;
    ++evaluations;
    if(!std::isfinite(score))return {};
    if(score>0)left=a;else right=a;
    if(right-left<=1e-12*a)break;
  }
  const double a=left+(right-left)/2,lg=special::lgamma(a);
  const double f=a*std::log(a)-lg-a+a*(logmean-log_arithmetic_mean)-logmean;
  const double score=std::log(a)-special::digamma(a)+logmean-log_arithmetic_mean;
  const double bound=f+std::max(score*(lo-a),score*(hi-a));
  const double error=4096*std::numeric_limits<double>::epsilon()*
    (1+std::abs(f)+std::abs(lg)+a*(1+std::abs(std::log(a))+std::abs(logmean)+
     std::abs(log_arithmetic_mean))+hi*(1+std::abs(score)));
  if(!std::isfinite(f)||!std::isfinite(score)||!std::isfinite(bound)||
     !std::isfinite(error)||!std::isfinite(n*(bound+error))||
     !std::isfinite(n*(f-error)))return {};
  return {n*(bound+error),n*(f-error),a};
}
// Uniform signed tail: local negative 1/q coefficient plus a strict gamma
// likelihood gap outside that neighborhood. See the dossier for the proof.
inline double signed_cutoff(double lo,double hi,double lm,double la,double n,
    double second,double ymax,const GammaTangent& face,int maxit,int& evaluations,
    std::int64_t limit=std::numeric_limits<std::int64_t>::max()) {
  const double eps=std::numeric_limits<double>::epsilon();
  const double variance=std::max(0.,second-1)+1e-10*(1+second);
  const double a=face.shape,threshold=1/variance;
  if(!(a>lo&&a<hi&&a<threshold)||!std::isfinite(face.lower))return INFINITY;
  const double left=std::max(lo,.75*a),right=std::min(hi,std::min(1.25*a,(a+threshold)/2));
  const double negative=left*.5*(1-right*variance);
  if(!(right>a&&left<a&&negative>0))return INFINITY;
  double outside=-INFINITY;
  if(left>lo)outside=std::max(outside,gamma_tangent(lo,left,lm,la,n,maxit,evaluations,limit).upper);
  if(right<hi)outside=std::max(outside,gamma_tangent(right,hi,lm,la,n,maxit,evaluations,limit).upper);
  const double gap=face.lower-outside;
  if(!(gap>4096*eps*n*(1+hi)))return INFINITY;
  const double v=second*(1+1e-10)+1e-10,m=ymax*(1+1e-10);
  const double remainder=right*right/4+2*right*right*right*m*(1+2*v)+2*right*right*right*v;
  const double factor=n*hi*hi*(1+2*v);
  const double cutoff=std::max(std::max(hi,2*hi*m),
    std::max(2*remainder/negative,2*factor/gap))*(1+1e-10);
  return std::isfinite(cutoff)?cutoff:INFINITY;
}
}
// witness coefficients are diagnostics from the existing all-free search.
// No claim about that search's global optimality is made by this function.
inline BetaPrimeJointBounds betapr_joint_exterior_bounds(span x,double p,double q,
    double scale,int maxit=1000,double incumbent=-INFINITY,
    std::int64_t limit=std::numeric_limits<std::int64_t>::max()) {
  BetaPrimeJointBounds out;
  maxit=static_cast<int>(solver_budget(maxit));
  if(x.size()<2||!(p>0)||!(q>0)||!(scale>0)||!std::isfinite(p)||
     !std::isfinite(q)||!std::isfinite(scale))return out;
  out.status=BetaPrimeJointBoundStatus::Arithmetic;
  const double eps=std::numeric_limits<double>::epsilon(),n=x.size();
  std::vector<double> logs;logs.reserve(x.size());
  betapr_joint_detail::Sum mean,mag;
  for(double v:x) {
    if(!(v>0)||!std::isfinite(v))return out;
    const double l=std::log(v);logs.push_back(l);mean.add(l/n);mag.add(std::abs(l)/n);
  }
  out.logmean=mean.value;
  std::sort(logs.begin(),logs.end());
  if(logs.front()==logs.back())return out;
  const double ls=std::log(scale),lb=special::lbeta(p,q);
  betapr_joint_detail::Sum a,b;
  for(double l:logs){a.add(betapr_joint_detail::softplus(ls-l)/n);
                     b.add(betapr_joint_detail::softplus(l-ls)/n);}
  const double ll=n*(-lb-p*a.value-q*b.value-mean.value);
  const double err=4096*eps*n*(1+std::abs(lb)+p*a.value+q*b.value+
    mag.value+(p+q)*(1+mag.value+std::abs(ls)));
  if(!std::isfinite(ll)||!std::isfinite(err))return out;
  out.witness_lower=ll-err;out.witness_upper=ll+err;
  out.incumbent_lower=std::max(out.witness_lower,incumbent);
  out.harmonic_lower=std::exp((out.incumbent_lower/n+mean.value)-4096*eps*(1+mag.value));
  betapr_joint_detail::Sum loss;
  for(std::size_t i=0;i<logs.size()/2;++i) {
    const double gap=std::max(0.,logs[logs.size()-1-i]-logs[i]-
      64*eps*(1+std::abs(logs[i])+std::abs(logs[logs.size()-1-i])));
    loss.add(gap*(gap/(4+2*gap)));
  }
  out.pair_loss=std::max(0.,loss.value-128*eps*(1+loss.value));
  if(!(out.pair_loss>0)||!(out.harmonic_lower>0)||
     !std::isfinite(out.harmonic_lower))return out;
  double h=std::max(1.,n/(2*out.pair_loss));
  bool excluded=false;
  for(int j=0;j<maxit;++j) {
    if(out.evaluations>=limit){out.status=BetaPrimeJointBoundStatus::Budget;return out;}
    ++out.evaluations;
    const double upper=betapr_joint_detail::large_upper(h,n,mean.value,out.pair_loss);
    const double margin=4096*eps*(1+std::abs(upper)+n*(1+mag.value)+h*out.pair_loss);
    if(std::isfinite(upper)&&upper+margin<out.incumbent_lower){excluded=true;break;}
    h*=2;if(!std::isfinite(h))return out;
  }
  if(!excluded){out.status=BetaPrimeJointBoundStatus::Budget;return out;}
  out.harmonic_upper=h;out.status=BetaPrimeJointBoundStatus::Geometry;
  // h<H and q>=2H imply p<2H (and conversely). This makes each remaining
  // one-large-shape tail uniform in the other shape.
  const double shape_hi=2*h;
  if(!(shape_hi>out.harmonic_lower)||!std::isfinite(shape_hi))return out;
  for(int side=0;side<2;++side) {
    const double maxlog=side?-logs.front():logs.back(),lm=side?-mean.value:mean.value;
    betapr_joint_detail::Sum scaled;
    for(double l:logs)scaled.add(std::exp((side?-l:l)-maxlog)/n);
    const double la=maxlog+std::log(scaled.value);
    betapr_joint_detail::Sum second;
    for(double l:logs){const double y=std::exp((side?-l:l)-la);second.add(y*y/n);}
    const double ymax=std::exp(maxlog-la);
    const auto face=betapr_joint_detail::gamma_tangent(out.harmonic_lower,shape_hi,
      lm,la,n,maxit,out.evaluations,limit);
    const double signed_bound=betapr_joint_detail::signed_cutoff(out.harmonic_lower,
      shape_hi,lm,la,n,second.value,ymax,face,maxit,out.evaluations,limit);
    if(out.evaluations>=limit){out.status=BetaPrimeJointBoundStatus::Budget;return out;}
    if(side)out.invgamma_signed_shape1=signed_bound;else out.gamma_signed_shape2=signed_bound;
    double upper=face.upper;
    // Reciprocal densities acquire -2*sum(log x).
    if(side)upper-=2*n*mean.value;
    upper+=4096*eps*n*(1+mag.value);
    if(side)out.invgamma_upper=upper;else out.gamma_upper=upper;
    if(!std::isfinite(upper)||!(upper<out.incumbent_lower))continue;
    const double factor=n*shape_hi*shape_hi*(1+2*second.value);
    double cutoff=std::max(1.,2*shape_hi*std::max(1.,ymax))*(1+1e-10);
    if(!std::isfinite(factor)||!std::isfinite(cutoff))continue;
    for(int j=0;j<maxit;++j) {
      if(out.evaluations>=limit){out.status=BetaPrimeJointBoundStatus::Budget;return out;}
      ++out.evaluations;
      const double tail=upper+factor/cutoff*(1+1e-10);
      if(tail<out.incumbent_lower) {
        if(side){out.invgamma_shape1=cutoff;out.invgamma_tail_upper=tail;}
        else {out.gamma_shape2=cutoff;out.gamma_tail_upper=tail;}
        break;
      }
      cutoff*=2;if(!std::isfinite(cutoff))break;
    }
  }
  if(std::isfinite(out.gamma_shape2)&&std::isfinite(out.invgamma_shape1))
    out.status=BetaPrimeJointBoundStatus::FiniteExterior;
  return out;
}
} // namespace cppunivariate
