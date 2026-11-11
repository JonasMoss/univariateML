// Prentice generalized gamma, in log-location, log-scale and signed shape.
#pragma once

#include "../../special/special.hpp"
#include <algorithm>
#include <cmath>
#include <limits>

namespace cppunivariate {
namespace distributions {
namespace gengamma_detail {
constexpr double half_log_two_pi = 0.91893853320467274178;

inline bool valid(double mu, double sigma, double shape) {
  return std::isfinite(mu) && std::isfinite(sigma) && sigma > 0 && std::isfinite(shape);
}
// (exp(z)-1-z)/z^2, including its limit.
inline double expm1_remainder_ratio(double z) {
  if (std::abs(z) >= 0.1) return ((std::expm1(z)-z)/z)/z;
  double term=0.5, sum=term;
  for (int k=3;k<=24;++k) { term*=z/k; sum+=term; }
  return sum;
}
inline double exprel(double z) {
  return z == 0 ? 1 : std::expm1(z)/z;
}
inline double penalty(double w,double shape) {
  return (w*expm1_remainder_ratio(shape*w))*w;
}
inline double remainder(double shape) {
  const double q=std::abs(shape), s=q*q;
  if (q <= 0.25)
    return s*(1.0/12-s*s/360+s*s*s*s/1260-s*s*s*s*s*s/1680);
  const double a=(1/q)/q;
  if (a==0) return std::log(q)-half_log_two_pi;
  return std::lgamma(a)-(a-0.5)*std::log(a)+a-half_log_two_pi;
}
struct TailIntegrand {
  double w, shape, rate, direction, slope, ew;
  double delta(double y) const {
    const double d=direction*y/rate, z=shape*d;
    return direction*y*(slope/rate)+ew*(d*expm1_remainder_ratio(z))*d;
  }
  double value(double y) const { return std::exp(-delta(y)); }
  double gradient(double y) const {
    const double d=direction*y/rate;
    return direction*(slope+ew*d*exprel(shape*d))/rate;
  }
  double curvature(double y) const {
    return (std::exp(shape*(w+direction*y/rate))/rate)/rate;
  }
};
inline double integrate(const TailIntegrand &f,double a,double b,double fa,double fm,double fb,
                        double tol,bool &ok) {
  if (!ok) return 0;
  const double width=b-a;
  const double m1=std::max(std::abs(f.gradient(a)),std::abs(f.gradient(b)));
  const double m2=std::max(f.curvature(a),f.curvature(b));
  const double c=std::abs(f.shape)/f.rate, m12=m1*m1;
  const double bound=fa*(m12*m12+6*m12*m2+3*m2*m2+4*m1*m2*c+m2*c*c)*
      width*width*width*width*width/2880;
  if (std::isfinite(bound) && bound<=tol) return width*(fa+4*fm+fb)/6;
  const double mid=(a+b)/2;
  if (mid==a || mid==b || tol==0) { ok=false; return 0; }
  const double left=integrate(f,a,mid,fa,f.value((a+mid)/2),fm,tol/2,ok);
  const double right=integrate(f,mid,b,fm,f.value((mid+b)/2),fb,tol/2,ok);
  return left+right;
}
inline double log_tail(double w,double shape) {
  if (std::isinf(w)) return special::kNegInf;
  const double z=shape*w, slope=w*exprel(z), ew=std::exp(z);
  const double rate=std::max(1.0,std::abs(slope));
  if (!std::isfinite(rate) || !std::isfinite(ew)) return special::kNegInf;
  const TailIntegrand f{w,shape,rate,w<0 ? -1.0 : 1.0,slope,ew};
  double end=1;
  for (;;) {
    const double g=f.gradient(end), v=f.value(end);
    if (g>0 && v/g < 2.5e-14) break;
    end*=2;
    if (!std::isfinite(end)) return special::kNaN;
  }
  bool ok=true;
  const double integral=integrate(f,0,end,1,f.value(end/2),f.value(end),5e-14,ok)/rate;
  if (!ok || !(integral>0)) return special::kNaN;
  return -half_log_two_pi-remainder(shape)-penalty(w,shape)+std::log(integral);
}
// E1(exp(lx)), on the log scale. The alternating series and continued
// fraction are used only in the limiting small-gamma-shape branch.
inline double log_e1(double lx) {
  if (lx<=0) {
    if (lx < -36) return std::log(-lx-0.57721566490153286061);
    const double x=std::exp(lx);
    double term=-x, sum=term;
    for (int k=2;;++k) {
      term*=-x/k;
      const double add=term/k;
      sum+=add;
      if (add==0 || std::abs(add)<std::abs(sum)*1e-16) break;
    }
    return std::log(-0.57721566490153286061-lx-sum);
  }
  const double x=std::exp(lx);
  if (!std::isfinite(x)) return special::kNegInf;
  double b=x+1, c=1e300, d=1/b, h=d;
  double previous=special::kNaN;
  for (int k=1;;++k) {
    const double an=-static_cast<double>(k)*k;
    b+=2;
    d=an*d+b; if (std::abs(d)<1e-300) d=1e-300;
    c=b+an/c; if (std::abs(c)<1e-300) c=1e-300;
    d=1/d;
    const double change=d*c;
    const double old=h;
    h*=change;
    if (!(h>0 && std::isfinite(h))) return special::kNaN;
    if (std::abs(change-1)<4e-16 || h==old) break;
    if (h==previous || k==std::numeric_limits<int>::max()) return special::kNaN;
    previous=old;
  }
  return -x+std::log(h);
}
inline special::LogTails very_large_tails(double w,double shape,double ratio) {
  const double logq=std::log(std::abs(shape)), loga=-2*logq;
  const double lx=std::fma(shape,w,loga);
  special::LogTails t;
  if (lx < -36) {
    const double lh=std::isinf(lx) ? (std::isinf(w) ? std::log(std::abs(ratio)) : std::log(std::abs(w))-logq) :
        loga+std::log(-lx-0.57721566490153286061);
    t=special::tails_from_log_hazard(lh,true);
  } else {
    t=special::tails_from_log(loga+log_e1(lx),false);
  }
  return shape>0 ? t : special::LogTails{t.upper,t.lower,t.valid};
}
// Preserve log Gamma(1+a) when a is too small for 1+a to retain it.
inline special::LogTails gamma_tails(double lx,double a) {
  if (lx < -36) {
    const double lg=a<=1.0/32 ? special::lgamma1p(a) : std::lgamma(a+1);
    return special::tails_from_log(a*lx-lg,true);
  }
  return special::gamma_log_tails(lx,a);
}
inline double gamma_quantile(double p,double a,bool lower,bool log_p) {
  const auto t=special::quantile_log_tails(p,lower,log_p);
  if (!t.valid) return special::kNaN;
  if (t.lower==special::kNegInf) return special::kNegInf;
  if (t.upper==special::kNegInf) return special::kPosInf;
  const double lg=a<=1.0/32 ? special::lgamma1p(a) : std::lgamma(a+1);
  const double leading=(t.lower+lg)/a;
  if (leading < -36) return leading;
  return special::gamma_log_quantile(p,a,lower,log_p);
}
inline special::LogTails tails(double w,double shape) {
  if (std::abs(shape)>1e10) return very_large_tails(w,shape,w/shape);
  if (shape==0) return special::tails_from_log(special::normal_log_upper(std::abs(w)),w<=0);
  if (std::abs(shape)<=0.25) return special::tails_from_log(log_tail(w,shape),w<0);
  const double a=(1/std::abs(shape))/std::abs(shape);
  return shape>0 ? gamma_tails(std::log(a)+shape*w,a) :
      [] (special::LogTails v) { return special::LogTails{v.upper,v.lower,v.valid}; }
      (gamma_tails(std::log(a)+shape*w,a));
}
inline double large_ratio(double lx,double mu,double sigma,double shape) {
  const double product=sigma*std::abs(shape);
  const double ratio=std::isfinite(product) ? special::scaled_difference(lx,mu,product) :
      special::scaled_difference(lx,mu,std::abs(shape))/sigma;
  return std::copysign(1.0,shape)*ratio;
}
} // namespace gengamma_detail

inline double dgengamma(double x,double mu,double sigma,double Q,bool log) {
  if (!gengamma_detail::valid(mu,sigma,Q) || std::isnan(x)) return special::kNaN;
  if (x<0 || x==special::kPosInf) return log ? special::kNegInf : 0;
  if (x==0) {
    if (Q<=0) return log ? special::kNegInf : 0;
    const double product=std::fma(Q,sigma,-1.0);
    if (product<0) return log ? special::kNegInf : 0;
    if (product>0) return special::kPosInf;
    const double a=(1/Q)/Q;
    const double ld=!std::isfinite(a) ? special::kPosInf : a==0 ? -mu : a>=10 ?
        (a-0.5*std::log(a)-gengamma_detail::half_log_two_pi-gengamma_detail::remainder(Q))-mu :
        a*std::log(a)-(a<=1.0/32 ? special::lgamma1p(a) : std::lgamma(1+a))-mu;
    return log ? ld : std::exp(ld);
  }
  const double lx=std::log(x), w=special::scaled_difference(lx,mu,sigma);
  if ((std::isinf(w) || std::isinf(Q*w)) && std::abs(Q)<=1e10) {
    if (std::abs(Q)<=1 || std::signbit(w)==std::signbit(Q)) return log ? special::kNegInf : 0;
    const double a=(1/std::abs(Q))/std::abs(Q);
    const double ratio=gengamma_detail::large_ratio(lx,mu,sigma,Q);
    const double ld=-std::log(sigma)-lx-gengamma_detail::half_log_two_pi-
        gengamma_detail::remainder(Q)+a+ratio;
    return log ? ld : std::exp(ld);
  }
  const double ratio=std::abs(Q)>1e10 ? gengamma_detail::large_ratio(lx,mu,sigma,Q) : 0;
  const double ld=std::abs(Q)>1e10 ?
      -std::log(std::abs(Q))-std::log(sigma)-lx+ratio-
        std::exp(std::fma(Q,w,-2*std::log(std::abs(Q)))) :
      -std::log(sigma)-lx-gengamma_detail::half_log_two_pi-
        gengamma_detail::remainder(Q)-gengamma_detail::penalty(w,Q);
  return log ? ld : std::exp(ld);
}
inline double pgengamma(double q,double mu,double sigma,double Q,bool lower_tail,bool log_p) {
  if (!gengamma_detail::valid(mu,sigma,Q) || std::isnan(q)) return special::kNaN;
  if (q<=0) return special::tail_probability(special::tails_from_log(special::kNegInf,true),lower_tail,log_p);
  if (q==special::kPosInf) return special::tail_probability(special::tails_from_log(special::kNegInf,false),lower_tail,log_p);
  const double lx=std::log(q), w=special::scaled_difference(lx,mu,sigma);
  if ((std::isinf(w) || std::isinf(Q*w)) && std::abs(Q)>1 && std::abs(Q)<=1e10 &&
      std::signbit(w)!=std::signbit(Q)) {
    const double a=(1/std::abs(Q))/std::abs(Q);
    const double lg=a<=1.0/32 ? special::lgamma1p(a) : std::lgamma(a+1);
    const double ratio=gengamma_detail::large_ratio(lx,mu,sigma,Q);
    const auto g=special::tails_from_log(a*std::log(a)-lg+ratio,true);
    const auto t=Q>0 ? g : special::LogTails{g.upper,g.lower,g.valid};
    return special::tail_probability(t,lower_tail,log_p);
  }
  const auto t=std::abs(Q)>1e10 ? gengamma_detail::very_large_tails(
      w,Q,gengamma_detail::large_ratio(lx,mu,sigma,Q)) : gengamma_detail::tails(w,Q);
  return special::tail_probability(t,lower_tail,log_p);
}
inline double qgengamma(double p,double mu,double sigma,double Q,bool lower_tail,bool log_p) {
  if (!gengamma_detail::valid(mu,sigma,Q)) return special::kNaN;
  const auto target=special::quantile_log_tails(p,lower_tail,log_p);
  if (!target.valid) return special::kNaN;
  if (target.lower==special::kNegInf) return 0;
  if (target.upper==special::kNegInf) return special::kPosInf;
  if (Q==0) return std::exp(std::fma(sigma,special::normal_quantile(p,lower_tail,log_p),mu));
  if (std::abs(Q)>1e10) {
    const double logq=std::log(std::abs(Q)), loga=-2*logq;
    const double upper=Q>0 ? target.upper : target.lower;
    const double lower_gamma=Q>0 ? target.lower : target.upper;
    const double logh=lower_gamma < -0.6931471805599453 ? std::log(-lower_gamma) :
        special::log_neg_log1mexp(upper);
    if (logh-loga>std::log(36.0-0.57721566490153286061)) {
      const double product=sigma*Q;
      const double h=lower_gamma < -0.6931471805599453 ? -lower_gamma : std::exp(logh);
      const double correction=(sigma/Q)*(loga+0.57721566490153286061);
      if (std::isfinite(product) && h>0 && std::isfinite(h)) {
        const double error=std::fma(sigma,Q,-product);
        return std::exp((std::fma(-product,h,mu)-error*h)-correction);
      }
      const double magnitude=std::exp(std::log(sigma)+logq+logh);
      return std::exp((mu-std::copysign(magnitude,Q))-correction);
    } else {
      const double wanted=upper-loga;
      double lo=-36, hi=710;
      for (;;) {
        const double mid=lo+(hi-lo)/2;
        if (mid==lo || mid==hi) break;
        const double e=gengamma_detail::log_e1(mid);
        if (std::isnan(e)) return special::kNaN;
        if (e>wanted) lo=mid; else hi=mid;
      }
      return std::exp(std::fma(sigma/Q,lo+(hi-lo)/2-loga,mu));
    }
  }
  if (std::abs(Q)>0.25) {
    const double a=(1/std::abs(Q))/std::abs(Q);
    const double gl=Q>0 ? target.lower : target.upper;
    const double norm=a<=1.0/32 ? special::lgamma1p(a) : std::lgamma(a+1);
    const double leading=(gl+norm)/a;
    if (std::isfinite(leading) && leading<=-36) {
      // The first correction to log G is exp(leading)/(a+1).  Keep it
      // separate: rounding it away before applying sigma can change even a
      // representable quantile into the wrong endpoint at very large scale.
      const double base=std::fma(sigma,(leading-std::log(a))/Q,mu);
      const double log_correction=std::log(sigma)-std::log(std::abs(Q))+
          leading-std::log1p(a);
      const double correction=std::copysign(std::exp(log_correction),Q);
      const double logx=base+correction;
      // For a in (0,16), the omitted log-gamma-quantile terms are bounded
      // by 2*exp(2*leading).  Check their effect after the scale transform.
      const double log_error=std::log(sigma)-std::log(std::abs(Q))+
          2*leading+std::log(2.0);
      if (log_error>std::log(1e-12)) {
        const double error=std::exp(log_error);
        const double lo=std::exp(logx-error), hi=std::exp(logx+error);
        if (lo!=hi && !(std::isfinite(lo) && std::isfinite(hi) &&
                        hi-lo<=1e-10*std::max(lo,hi))) return special::kNaN;
      }
      return std::exp(logx);
    }
    const double lg=gengamma_detail::gamma_quantile(p,a,Q>0 ? lower_tail : !lower_tail,log_p);
    if (lg==special::kNegInf) {
      const double product=sigma*Q;
      if (std::isfinite(product)) {
        const double error=std::fma(sigma,Q,-product);
        const double correction=(sigma/Q)*(norm/a-std::log(a));
        return std::exp((std::fma(product,gl,mu)+error*gl)+correction);
      }
      const double term=std::exp(std::log(sigma)+std::log(std::abs(Q))+std::log(-gl));
      return std::exp(mu-std::copysign(term,Q)+(sigma/Q)*(norm/a-std::log(a)));
    }
    return std::exp(std::fma(sigma,(lg-std::log(a))/Q,mu));
  }
  const bool left=target.lower<target.upper;
  const double wanted=left ? target.lower : target.upper;
  const double initial=special::normal_quantile(p,lower_tail,log_p);
  double step=std::max(1.0,std::abs(initial)*0.25);
  double lo=initial-step, hi=initial+step;
  const auto before=[&](double w) {
    const auto t=gengamma_detail::tails(w,Q);
    return left ? t.lower-wanted : wanted-t.upper;
  };
  double flo=before(lo), fhi=before(hi);
  while (flo>=0) {
    step*=2; lo=initial-step;
    if (!std::isfinite(lo)) return special::kNaN;
    flo=before(lo);
  }
  step=std::max(1.0,std::abs(initial)*0.25);
  while (fhi<0) {
    step*=2; hi=initial+step;
    if (!std::isfinite(hi)) return special::kNaN;
    fhi=before(hi);
  }
  if (!(flo<0 && fhi>=0)) return special::kNaN;
  for (;;) {
    const double mid=lo<0 && hi>0 ? lo/2+hi/2 : lo+(hi-lo)/2;
    if (mid==lo || mid==hi) break;
    const double fm=before(mid);
    if (std::isnan(fm)) return special::kNaN;
    if (fm<0) lo=mid; else hi=mid;
  }
  const double xl=std::exp(std::fma(sigma,lo,mu));
  const double xh=std::exp(std::fma(sigma,hi,mu));
  if (xl!=xh && !(std::isfinite(xl) && std::isfinite(xh) &&
                   xh-xl<=1e-10*std::max(xl,xh))) return special::kNaN;
  return std::exp(std::fma(sigma,lo+(hi-lo)/2,mu));
}

template <typename Rng>
inline double rgengamma(Rng &rng,double mu,double sigma,double Q) {
  if (!gengamma_detail::valid(mu,sigma,Q)) return special::kNaN;
  if (Q==0) return std::exp(std::fma(sigma,rng.rnorm(),mu));
  const double q=std::abs(Q), a=(1/q)/q;
  if (q>0.25) {
    if (a<1.0) return qgengamma(rng.runif(0,1),mu,sigma,Q,true,false);
    return std::exp(std::fma(sigma,(std::log(rng.rgamma(a,1))-std::log(a))/Q,mu));
  }
  const double v=1-q*q/3, root=std::sqrt(v);
  for (;;) {
    const double z=rng.rnorm(), t=(q/3/root)*z;
    if (t<=-1) continue;
    const double u=rng.runif(0,1), lv=3*std::log1p(t);
    const double ratio=t==0 ? 1 : std::log1p(t)/t;
    const double normalized=(Q>0 ? z : -z)*ratio/root;
    const double loss=v*(normalized*normalized)*gengamma_detail::expm1_remainder_ratio(lv);
    if (u<1-0.0331*z*z*z*z || std::log(u)<0.5*z*z-loss) {
      const double center=-Q/3 * (q*q/3==0 ? 1 : std::log1p(-q*q/3)/(-q*q/3));
      return std::exp(std::fma(sigma,center+normalized,mu));
    }
  }
}

} // namespace distributions
} // namespace cppunivariate
