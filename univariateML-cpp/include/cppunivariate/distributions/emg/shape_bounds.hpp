#pragma once
#include "dpqr.hpp"
#include "../../core/span.hpp"
#include "../../core/work_budget.hpp"

namespace cppunivariate {
namespace emg_detail {
// Uniform upper bound over 0 < k <= kmax and all physical nuisance
// coordinates. Relaxing holds preserves this upper bound. Nonconstant data
// are required; infinity means that this bound cannot exclude the tail.
// Proof: retain one minimum's Phi factor and optimize location and rate.
inline double small_shape_upper(span x,double kmax) {
  if(x.empty() || !(kmax>0&&std::isfinite(kmax)))return INFINITY;
  const double xmin=*std::min_element(x.begin(),x.end());
  CPPUNIV_EXTENDED_REAL gap=0;
  for(double v:x) {if(!std::isfinite(v))return INFINITY;gap+=static_cast<CPPUNIV_EXTENDED_REAL>(v)-xmin;}
  if(!(gap>0&&std::isfinite(gap)))return INFINITY;
  const CPPUNIV_EXTENDED_REAL n=x.size(),k=kmax;
  const CPPUNIV_EXTENDED_REAL face=n*(std::log(n/gap)-1);
  // For k <= kmax, max(0, (n*k)^2/2-log(2))-n*k^2/2
  // is bounded by max(0, n*(n-1)*kmax^2/2-log(2)).
  const CPPUNIV_EXTENDED_REAL extra=std::max(CPPUNIV_EXTENDED_LITERAL(0.0,0.0L),
      n*(n-1)*k*k/2-std::log(CPPUNIV_EXTENDED_LITERAL(2.0,2.0L)));
  const CPPUNIV_EXTENDED_REAL value=face+extra;
  const double upper=static_cast<double>(value+128*std::numeric_limits<double>::epsilon()*(n+std::abs(face)+extra));
  return std::isfinite(upper)?upper:INFINITY;
}
// With held mu <= min(x), retain every endpoint Phi factor. This bound
// includes the half-density for each equality, uniformly over optimized rate.
inline double held_location_small_shape_upper(span x,double mu,double kmax,double lambda=NAN) {
  if(x.empty()||!std::isfinite(mu)||!(kmax>0&&std::isfinite(kmax))||
      (!std::isnan(lambda)&&!(lambda>0&&std::isfinite(lambda))))return INFINITY;
  CPPUNIV_EXTENDED_REAL gap=0;std::size_t ties=0;
  for(double v:x) {
    if(!std::isfinite(v)||v<mu)return INFINITY;
    gap+=static_cast<CPPUNIV_EXTENDED_REAL>(v)-mu;if(v==mu)++ties;
  }
  const CPPUNIV_EXTENDED_REAL n=x.size();
  if(!std::isfinite(gap)||(std::isnan(lambda)&&!(gap>0)))return INFINITY;
  const CPPUNIV_EXTENDED_REAL rate=std::isnan(lambda)?n/gap:static_cast<CPPUNIV_EXTENDED_REAL>(lambda);
  const CPPUNIV_EXTENDED_REAL face=n*std::log(rate)-rate*gap-ties*std::log(CPPUNIV_EXTENDED_LITERAL(2.0,2.0L));
  // d/dk log Phi(-k)=-r(k) <= -r(0)=-sqrt(2/pi).
  // The resulting convex quadratic has its maximum at 0 or kmax.
  const CPPUNIV_EXTENDED_REAL extra=std::max(CPPUNIV_EXTENDED_LITERAL(0.0,0.0L),
      n*static_cast<CPPUNIV_EXTENDED_REAL>(kmax)*kmax/2-
      ties*std::sqrt(2/std::acos(-CPPUNIV_EXTENDED_LITERAL(1.0,1.0L)))*kmax);
  const CPPUNIV_EXTENDED_REAL value=face+extra;
  const double upper=static_cast<double>(value+128*std::numeric_limits<double>::epsilon()*(n+std::abs(face)+extra));
  return std::isfinite(upper)?upper:INFINITY;
}
// f_k(z)=k int_0^inf exp(-k*t) phi(z-t) dt gives
// f_k(z) <= (hi/lo) f_lo(z), uniformly in z. Consequently a certified
// nuisance-profile upper bound at lo extends over [lo,hi] by n log(hi/lo).
// A held physical rate fixes b=lambda/k; use a relaxed inner upper bound,
// not the fixed-rate inner value at lo, when applying this inequality.
inline double shape_interval_upper(double lower_profile_upper,std::size_t n,double lo,double hi) {
  if(!(lo>0&&hi>=lo&&std::isfinite(hi))||!std::isfinite(lower_profile_upper))return INFINITY;
  const double extra=static_cast<double>(n)*std::log1p((hi-lo)/lo);
  return lower_profile_upper+extra+128*std::numeric_limits<double>::epsilon()*(n+std::abs(lower_profile_upper)+extra);
}
}
}

namespace cppunivariate {
namespace emg_detail {
// Global upper bound for k >= kmin, including optimized nuisance coordinates.
// Values below threshold need no exclusion; the result bounds the entire tail
// by taking their maximum with the normal-envelope bound. Proof in the report.
inline double large_shape_upper(span x,double kmin,double threshold,int maxit=500,
    double mu=NAN,double sigma=NAN,double lambda=NAN,double normal_upper=NAN) {
  if(x.empty()||!(kmin>2&&std::isfinite(kmin))||!std::isfinite(threshold)||
      (!std::isnan(mu)&&!std::isfinite(mu))||
      (!std::isnan(sigma)&&!(sigma>0&&std::isfinite(sigma)))||
      (!std::isnan(lambda)&&!(lambda>0&&std::isfinite(lambda))))return INFINITY;
  for(double v:x)if(!std::isfinite(v))return INFINITY;
  const CPPUNIV_EXTENDED_REAL n=x.size(),normal_constant=emg_detail::logphi(0);
  if(!std::isnan(lambda)) {
    // sigma=k/lambda and every convolution density is at most the normal peak.
    const CPPUNIV_EXTENDED_REAL upper=n*(std::log(lambda)-std::log(kmin)+normal_constant);
    const double value=static_cast<double>(upper+128*std::numeric_limits<double>::epsilon()*(n+std::abs(upper)));
    return std::isfinite(value)?value:INFINITY;
  }
  const double xmin=*std::min_element(x.begin(),x.end()),xmax=*std::max_element(x.begin(),x.end());
  CPPUNIV_EXTENDED_REAL distance=static_cast<CPPUNIV_EXTENDED_REAL>(xmax)-xmin;
  if(!std::isnan(mu)) {distance=0;for(double v:x)distance+=std::abs(static_cast<CPPUNIV_EXTENDED_REAL>(v)-mu);}
  CPPUNIV_EXTENDED_REAL bmax=std::isnan(sigma)?0:1/static_cast<CPPUNIV_EXTENDED_REAL>(sigma);
  if(std::isnan(sigma)) {
    if(!(distance>0&&std::isfinite(distance)))return INFINITY;
    // f_k(z) <= C exp(-|z|/2), C=2 exp(1/8)/sqrt(2*pi), uniformly k>=1.
    const CPPUNIV_EXTENDED_REAL logc=std::log(CPPUNIV_EXTENDED_LITERAL(2.0,2.0L))+CPPUNIV_EXTENDED_LITERAL(.125,.125L)+normal_constant;
    auto envelope=[&](CPPUNIV_EXTENDED_REAL b) {
      const CPPUNIV_EXTENDED_REAL value=n*std::log(b)+n*logc-b*distance/2;
      return value+128*std::numeric_limits<double>::epsilon()*(n+std::abs(value));
    };
    bmax=2*n/distance; // The envelope's maximum; it decreases beyond here.
    if(envelope(bmax)<threshold)return threshold;
    bool excluded=false;
    for(int it=0;it<solver_budget(maxit);++it) {
      bmax*=2;
      if(!std::isfinite(bmax))return INFINITY;
      if(envelope(bmax)<threshold) {excluded=true;break;}
    }
    if(!excluded)return INFINITY;
  }
  // The standardized mode lies in (0,2/k). A free location optimum
  // therefore lies in [xmin-2/(k*b),xmax], bounding every positive residual.
  const CPPUNIV_EXTENDED_REAL zmax=std::isnan(mu)?
      bmax*(static_cast<CPPUNIV_EXTENDED_REAL>(xmax)-xmin)+2/static_cast<CPPUNIV_EXTENDED_REAL>(kmin):
      bmax*std::max(CPPUNIV_EXTENDED_LITERAL(0.0,0.0L),static_cast<CPPUNIV_EXTENDED_REAL>(xmax)-mu);
  if(!(zmax<kmin))return INFINITY;
  CPPUNIV_EXTENDED_REAL normal_mu=mu;
  if(std::isnan(mu)) {normal_mu=0;for(double v:x)normal_mu+=static_cast<CPPUNIV_EXTENDED_REAL>(v)/n;}
  CPPUNIV_EXTENDED_REAL largest=0;
  for(double v:x) largest=std::max(largest,std::abs(static_cast<CPPUNIV_EXTENDED_REAL>(v)-normal_mu));
  if(!std::isfinite(largest))return INFINITY;
  CPPUNIV_EXTENDED_REAL square=0;
  if(largest>0)for(double v:x) {
    const CPPUNIV_EXTENDED_REAL z=(static_cast<CPPUNIV_EXTENDED_REAL>(v)-normal_mu)/largest;
    square+=z*z/n;
  }
  const CPPUNIV_EXTENDED_REAL sd=std::isnan(sigma)?largest*std::sqrt(square):static_cast<CPPUNIV_EXTENDED_REAL>(sigma);
  if(!(sd>0&&std::isfinite(sd)))return INFINITY;
  const CPPUNIV_EXTENDED_REAL ratio=largest/sd;
  CPPUNIV_EXTENDED_REAL normal=n*(normal_constant-std::log(sd)-square*ratio*ratio/2);
  if(!std::isnan(normal_upper)){if(!std::isfinite(normal_upper))return INFINITY;normal=std::max(normal,static_cast<CPPUNIV_EXTENDED_REAL>(normal_upper));}
  // M(t) <= 1/t: f_k(z) <= phi(z)/(1-z/k) for z<k.
  const CPPUNIV_EXTENDED_REAL upper=normal-n*std::log1p(-zmax/kmin);
  const CPPUNIV_EXTENDED_REAL margin=256*std::numeric_limits<double>::epsilon()*(n+std::abs(normal)+std::abs(upper));
  const double value=static_cast<double>(std::max(static_cast<CPPUNIV_EXTENDED_REAL>(threshold),upper+margin));
  return std::isfinite(value)?value:INFINITY;
}
}
}

namespace cppunivariate {
namespace emg_detail {
// Conditional small-shape tail bound, respecting every physical hold.
inline double conditional_small_shape_upper(span x,double kmax,double mu=NAN,double sigma=NAN,double lambda=NAN) {
  if(x.empty()||!(kmax>0&&std::isfinite(kmax))||
      (!std::isnan(mu)&&!std::isfinite(mu))||
      (!std::isnan(sigma)&&!(sigma>0&&std::isfinite(sigma)))||
      (!std::isnan(lambda)&&!(lambda>0&&std::isfinite(lambda))))return INFINITY;
  for(double v:x)if(!std::isfinite(v))return INFINITY;
  const CPPUNIV_EXTENDED_REAL n=x.size(),k=kmax;
  CPPUNIV_EXTENDED_REAL upper=INFINITY;
  if(!std::isnan(sigma)) {
    // f <= lambda and lambda=k/sigma <= kmax/sigma.
    upper=n*(std::log(k)-std::log(sigma));
  } else {
    const double xmin=*std::min_element(x.begin(),x.end());
    if(!std::isnan(mu)&&mu<=xmin)return held_location_small_shape_upper(x,mu,kmax,lambda);
    if(!std::isnan(mu)) {
      // Retain the leftmost Phi factor: f(xmin) <= lambda*Phi(-d/sigma)
      // <= lambda/2*exp(-d^2*lambda^2/(2*k^2)). Optimize lambda if free.
      const CPPUNIV_EXTENDED_REAL d=static_cast<CPPUNIV_EXTENDED_REAL>(mu)-xmin;
      if(std::isnan(lambda))upper=n*(std::log(k)+std::log(n)/2-std::log(d)-CPPUNIV_EXTENDED_LITERAL(.5,.5L))-std::log(CPPUNIV_EXTENDED_LITERAL(2.0,2.0L));
      else {const CPPUNIV_EXTENDED_REAL z=d*lambda/k;upper=n*std::log(lambda)-std::log(CPPUNIV_EXTENDED_LITERAL(2.0,2.0L))-z*z/2;}
    } else if(!std::isnan(lambda)) {
      CPPUNIV_EXTENDED_REAL gap=0;for(double v:x)gap+=static_cast<CPPUNIV_EXTENDED_REAL>(v)-xmin;
      upper=n*std::log(lambda)-lambda*gap+std::max(CPPUNIV_EXTENDED_LITERAL(0.0,0.0L),n*(n-1)*k*k/2-std::log(CPPUNIV_EXTENDED_LITERAL(2.0,2.0L)));
    } else return small_shape_upper(x,kmax);
  }
  const double value=static_cast<double>(upper+256*std::numeric_limits<double>::epsilon()*(n+std::abs(upper)));
  return std::isfinite(value)?value:INFINITY;
}
}
}

namespace cppunivariate {
namespace emg_detail {
// The fixed-(u,b) shape second derivative is >= -n/lo^2. Adding that
// quadratic makes every objective convex; taking a supremum preserves
// convexity. Endpoint profile upper bounds therefore give a quadratic chord
// bound, stronger than the one-sided density-ratio bound on short intervals.
// Applies with any holds on mu/sigma, while physical lambda is free.
inline double shape_chord_upper(double left,double right,std::size_t n,double lo,double hi) {
  if(!(lo>0&&hi>=lo&&std::isfinite(hi))||!std::isfinite(left)||!std::isfinite(right))return INFINITY;
  const CPPUNIV_EXTENDED_REAL relative=(static_cast<CPPUNIV_EXTENDED_REAL>(hi)-lo)/lo;
  const CPPUNIV_EXTENDED_REAL extra=static_cast<CPPUNIV_EXTENDED_REAL>(n)*relative*relative/8;
  const CPPUNIV_EXTENDED_REAL top=std::max(left,right);
  const double value=static_cast<double>(top+extra+256*std::numeric_limits<double>::epsilon()*(n+std::abs(top)+extra));
  return std::isfinite(value)?value:INFINITY;
}
// For held lambda use a=lambda*mu as the nuisance coordinate, independent of
// k. Bound its optimizing range uniformly on [lo,hi], then bound the negative
// shape curvature in log Phi(lambda*(x-mu)/k-k). Endpoint profiles retain the
// physical rate. The resulting chord can be loose on wide intervals.
inline double fixed_rate_shape_chord_upper(double left,double right,span x,double lo,double hi,double lambda,double mu=NAN) {
  if(x.empty()||!(lo>0&&hi>=lo&&std::isfinite(hi)&&lambda>0&&std::isfinite(lambda))||
      !std::isfinite(left)||!std::isfinite(right)||(!std::isnan(mu)&&!std::isfinite(mu)))return INFINITY;
  for(double v:x)if(!std::isfinite(v))return INFINITY;
  const CPPUNIV_EXTENDED_REAL a=lo,b=hi,n=x.size();
  CPPUNIV_EXTENDED_REAL zmax=0;
  if(std::isnan(mu)) {
    const double xmin=*std::min_element(x.begin(),x.end()),xmax=*std::max_element(x.begin(),x.end());
    const CPPUNIV_EXTENDED_REAL t=std::sqrt(std::max(CPPUNIV_EXTENDED_LITERAL(0.0,0.0L),2*std::log(2/a)));
    zmax=lambda*(static_cast<CPPUNIV_EXTENDED_REAL>(xmax)-xmin)+(a>=1?CPPUNIV_EXTENDED_LITERAL(2.0,2.0L):b*(b+t));
  } else for(double v:x)zmax=std::max(zmax,std::abs(static_cast<CPPUNIV_EXTENDED_REAL>(lambda)*(static_cast<CPPUNIV_EXTENDED_REAL>(v)-mu)));
  const CPPUNIV_EXTENDED_REAL rmax=b+zmax/a+1;
  const CPPUNIV_EXTENDED_REAL extra_slope=zmax/(a*a);
  // L_kk = n - sum(w*(1+d/k^2)^2) + sum(2*r*d/k^3).
  // Retain the positive n term rather than discarding its cancellation.
  const CPPUNIV_EXTENDED_REAL curvature=n*(2*extra_slope+extra_slope*extra_slope+2*rmax*zmax/(a*a*a));
  const CPPUNIV_EXTENDED_REAL width=b-a,extra=curvature*width*width/8,top=std::max(left,right);
  const double value=static_cast<double>(top+extra+256*std::numeric_limits<double>::epsilon()*(n+std::abs(top)+extra));
  return std::isfinite(value)?value:INFINITY;
}
// With physical rate held, tau=1/k^2 gives
// f(x)=lambda*sqrt(tau/(2*pi))*int_0^inf exp(-y-tau*(d-y)^2/2)dy,
// d=lambda*(x-mu). The log integral is convex in tau (its second
// derivative is a variance). Supremizing over the same location domain
// preserves convexity of P(tau)-n*log(tau)/2. Thus endpoint profile upper
// bounds plus n/2 times the maximum log-versus-chord gap bound the segment.
// Unlike the k-curvature bound this needs no residual/location envelope.
inline double fixed_rate_precision_chord_upper(double left,double right,std::size_t n,double lo,double hi) {
  if(!(lo>0&&hi>=lo&&std::isfinite(hi))||!std::isfinite(left)||!std::isfinite(right))return INFINITY;
  const CPPUNIV_EXTENDED_REAL t=2*std::log1p((static_cast<CPPUNIV_EXTENDED_REAL>(hi)-lo)/lo);
  if(!std::isfinite(t))return INFINITY;
  CPPUNIV_EXTENDED_REAL gap=0;
  if(t<=CPPUNIV_EXTENDED_LITERAL(.01,.01L)) {
    // log''(tau) >= -1/A^2 on [A,B]; avoid cancellation as B/A -> 1.
    const CPPUNIV_EXTENDED_REAL width=std::expm1(t);
    gap=width*width/8;
  } else {
    const CPPUNIV_EXTENDED_REAL e=std::exp(-t),den=-std::expm1(-t);
    // The maximum occurs at tau/A=(exp(t)-1)/t. This form avoids exp(t).
    gap=t+std::log(den)-std::log(t)-1+t*e/den;
  }
  if(!std::isfinite(gap))return INFINITY;
  const CPPUNIV_EXTENDED_REAL top=std::max(left,right),extra=static_cast<CPPUNIV_EXTENDED_REAL>(n)*std::max(CPPUNIV_EXTENDED_LITERAL(0.0,0.0L),gap)/2;
  const double value=static_cast<double>(top+extra+256*std::numeric_limits<double>::epsilon()*(n+std::abs(top)+extra));
  return std::isfinite(value)?value:INFINITY;
}
}
}
