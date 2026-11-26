#pragma once
#include "accuracy.hpp"
#include "../dnorm/mle.hpp"

namespace cppunivariate {
namespace dgamma_detail {

// The score tails and both curvature tests are derived in
// dev/research/evaluations/2026-11-25-dgamma2-math-report.md.
struct LogRateBracket {
  double lo, hi;
};

// Practical arithmetic pads, scaled by the terms in the comparisons.
inline constexpr double curvature_pad = 128 * std::numeric_limits<double>::epsilon();

inline double curvature_deviance(double x) {
  return static_cast<double>(-accuracy_detail::log1pmx(
    accuracy_detail::Jet(x)).v);
}

inline double curvature_upper(const dnorm_detail::Cells& p) {
  const double b = p.x.front()+1, h = p.x.back();
  const double wl = p.w.front()/p.n, wh = p.w.back()/p.n;
  const double gap = h-b;
  const double left = gap*(wh/(wl+wh)), right = gap*(wl/(wl+wh));
  const double m = b+left;
  // log1p's argument can round to -1 for very disparate cells.
  const double dl = left < m/2 ? curvature_deviance(-left/m) :
    b/m-1+std::log(m/b);
  const double d = wl*dl+wh*curvature_deviance(right/m);
  return 1/(d*(1-curvature_pad));
}

inline double curvature_mean(const dnorm_detail::Cells& p) {
  double total=0;
  for (std::size_t i=0; i<p.x.size(); ++i) total+=p.w[i]*p.x[i];
  return total/p.n;
}

inline bool trace_curvature(const dnorm_detail::Cells& p, double high, double mean) {
  const double zero = p.x.front()==0 ? p.w.front()/p.n : 0;
  double spread = 0;
  for (std::size_t i=0; i<p.x.size(); ++i) {
    const double k = p.x[i];
    if (k==0) continue;
    const double b = std::max(std::abs(1/k-1/(mean+1)),
                              std::abs(1/(k+1)-1/mean));
    const double range = high*b;
    spread += (p.w[i]/p.n)*range*range;
  }
  const double inverse = 1/(1+high);
  const double trace = (zero+spread/4)/(.5*(1+inverse*inverse))+
    (high/mean)/mean/4;
  return trace < 1-curvature_pad*(1+p.x.size());
}

inline accuracy_detail::Jet curvature_cell(double a, double u, double k, int maxit) {
  return cell_logprob(accuracy_detail::variable(a,0,true),
    accuracy_detail::exp(accuracy_detail::variable(u,1,true)),k,maxit);
}

// Log-rate brackets are maintained using strict score comparisons. A
// Newton step proposes points only. Near the root, try opposite probes
// so a score within its arithmetic pad does not move a bracket endpoint.
// Share the certificate budget across rate and shape score evaluations
// and the pending intervals, while retaining each solver's iteration cap.
struct CurvatureWork {
  int maxit;
  std::int64_t remaining;
  bool take() {
    if (remaining<=0) return false;
    --remaining;
    return true;
  }
};

inline bool curvature_rate(const dnorm_detail::Cells& p, double a, double width, double mean,
                           CurvatureWork& work, LogRateBracket& box) {
  const double loga=std::log(a);
  const double lo=loga-std::log(mean+1), hi=loga-std::log(mean);
  const double pad=curvature_pad*(1+p.x.size()+std::abs(loga)+std::abs(lo)+std::abs(hi));
  box={lo-pad,hi+pad};
  double u=(lo+hi)/2;
  for (int i=0; i<work.maxit; ++i) {
    if (box.hi-box.lo<=width) return true;
    if (!work.take()) return false;
    double g=0, derivative=0, error=0;
    const double r=std::exp(u);
    for (std::size_t j=0; j<p.x.size(); ++j) {
      const auto f=curvature_cell(a,u,p.x[j],work.maxit);
      if (!std::isfinite(f.g[1])) return false;
      const double w=p.w[j]/p.n, x=(p.x[j]+1)*r;
      g+=w*f.g[1];
      derivative+=w*f.h[1][1];
      error+=w*(1+std::abs(f.g[1])+std::abs(f.v)+a+x);
    }
    error*=curvature_pad*(1+p.x.size());
    if (!std::isfinite(g) || !std::isfinite(error)) return false;
    if (g>error) box.lo=u;
    else if (g< -error) box.hi=u;
    else {
      const double left=u-width/3, right=u+width/3;
      if (left>box.lo && right<box.hi) {
        // A near-zero evaluation gives no sign. Probe towards
        // the more distant endpoint.
        u=(box.hi-u > u-box.lo) ? right : left;
        continue;
      }
      u=(box.hi-u > u-box.lo) ? (u+box.hi)/2 : (box.lo+u)/2;
      continue;
    }
    const double next=u-g/derivative;
    const double inset=width/4;
    if (derivative<0 && box.hi-box.lo>2*inset && next>box.lo && next<box.hi)
      u=std::max(box.lo+inset,std::min(box.hi-inset,next));
    else u=(box.lo+box.hi)/2;
  }
  return false;
}

inline double curvature_score_bound(const dnorm_detail::Cells& p, double a, double u,
                                    double other_a, double other_u, bool upper,
                                    CurvatureWork& work) {
  if (!work.take()) return NAN;
  const double positive=p.x.front()==0 ? (p.n-p.w.front())/p.n : 1;
  const double psi=special::digamma(1+other_a), anchor_psi=special::digamma(1+a);
  const double r=std::exp(u);
  double s=other_u-psi+positive/other_a;
  double size=1+std::abs(other_u)+std::abs(psi)+1/other_a;
  for (std::size_t j=0; j<p.x.size(); ++j) {
    const auto f=curvature_cell(a,u,p.x[j],work.maxit);
    if (!std::isfinite(f.g[0])) return NAN;
    // For the zero cell this is E[log Y]+1/a. Combine the
    // singular terms in the profile score before comparing signs.
    const double m=f.g[0]+anchor_psi-u-(p.x[j]>0 ? 1/a : 0);
    const double w=p.w[j]/p.n;
    s+=w*m;
    size+=w*(1+std::abs(f.g[0])+std::abs(f.v)+std::abs(u)+1/a+
             a+(p.x[j]+1)*r);
  }
  const double error=curvature_pad*(1+p.x.size())*size;
  return std::isfinite(s) && std::isfinite(error) ? s+(upper ? error : -error) : NAN;
}

inline double curvature_score(const dnorm_detail::Cells& p, double a,
                              LogRateBracket rate, bool upper, CurvatureWork& work) {
  return curvature_score_bound(p,a,upper ? rate.lo : rate.hi,
                               a,upper ? rate.hi : rate.lo,upper,work);
}

inline bool box_curvature(const dnorm_detail::Cells& p, double a, double b,
                          LogRateBracket rate, CurvatureWork& work) {
  if (!work.take()) return false;
  const double r=std::exp(rate.lo), s=std::exp(rate.hi), ratio=std::exp(rate.lo-rate.hi);
  // Use (log Y, r*Y) to keep the scale of the second statistic
  // independent of the units of the counts.
  double xx=0, yy=0, xy=0, error=0;
  const double psi=special::trigamma(a), lga=std::lgamma(a), lgb=std::lgamma(b);
  for (std::size_t i=0; i<p.x.size(); ++i) {
    const double k=p.x[i];
    const auto f=curvature_cell(a,rate.lo,k,work.maxit);
    const auto end=curvature_cell(b,rate.hi,k,work.maxit);
    const double log_k=std::log(k+1);
    const double jlo=f.v+lga-a*(rate.lo+log_k)+r*k;
    const double jhi=end.v+lgb-b*(rate.hi+log_k)+s*k;
    const double magnitude=1+std::abs(f.v)+std::abs(end.v)+std::abs(lga)+std::abs(lgb)+
      a*(std::abs(rate.lo)+log_k)+b*(std::abs(rate.hi)+log_k)+s*(k+1);
    const double weight=p.w[i]/p.n*
      std::exp(std::max(0.0,jlo-jhi)+curvature_pad*magnitude);
    // Remove the gamma normalizer derivatives to recover conditional
    // moments. The log-rate Hessian also includes the chain rule term.
    const double vx=f.h[0][0]+psi, vy=f.h[1][1]-f.g[1]+a, cov=1-f.h[0][1];
    if (!std::isfinite(weight) || !std::isfinite(vx) || !std::isfinite(vy) ||
        !std::isfinite(cov) || !std::isfinite(jlo-jhi)) return false;
    xx+=weight*std::max(0.0,vx);
    yy+=weight*std::max(0.0,vy);
    xy+=weight*cov;
    error+=weight*magnitude*(1+psi+std::abs(f.h[1][1])+std::abs(f.g[1])+std::abs(cov));
  }
  error*=curvature_pad*(1+p.x.size());
  // An uncertain domination factor affects the signed covariance as
  // well as the variances. Use the same pad on all three entries.
  const double t=special::trigamma(b)-xx-error;
  const double y=a*ratio*ratio-yy-error;
  const double c=std::max(std::abs(1-xy),std::abs(ratio-xy))+error;
  return t>0 && y>0 && std::isfinite(t*y) && t*y>c*c*(1+curvature_pad);
}

struct ProfileEndpoint {
  double log_shape;
  LogRateBracket rate;
};

inline bool curvature_cover(const dnorm_detail::Cells& p, ProfileEndpoint low,
                            ProfileEndpoint high, double mean, CurvatureWork& work) {
  struct Segment { ProfileEndpoint low, high; };
  std::vector<Segment> pending{{low,high}};
  while (!pending.empty() && work.take()) {
    const auto segment=pending.back();
    pending.pop_back();
    const double a=std::exp(segment.low.log_shape), b=std::exp(segment.high.log_shape);
    const LogRateBracket rate{segment.low.rate.lo,segment.high.rate.hi};
    if (curvature_score_bound(p,a,rate.hi,b,rate.lo,false,work)>0 ||
        curvature_score_bound(p,b,rate.lo,a,rate.hi,true,work)<0 ||
        box_curvature(p,a,b,rate,work)) continue;
    const double middle=(segment.low.log_shape+segment.high.log_shape)/2;
    ProfileEndpoint mid{middle,{}};
    if (!(middle>segment.low.log_shape && middle<segment.high.log_shape) ||
        !curvature_rate(p,std::exp(middle),1e-8,mean,work,mid.rate)) return false;
    pending.push_back({mid,segment.high});
    pending.push_back({segment.low,mid});
  }
  return pending.empty();
}

// The same local likelihood is used for refinement on both fit routes.
inline accuracy_detail::Jet fit_likelihood(const dnorm_detail::Cells& p,
    const std::array<accuracy_detail::Jet,3>& z, int maxit) {
  const auto shape=accuracy_detail::exp(z[0]), rate=accuracy_detail::exp(z[1]);
  accuracy_detail::JetSum sum;
  for (std::size_t i=0; i<p.x.size(); ++i) {
    sum.add(accuracy_detail::Jet(p.w[i])*cell_logprob(shape,rate,p.x[i],maxit));
  }
  return sum.value;
}

inline bool solve_curvature(const dnorm_detail::Cells& p, double tol, int maxit,
                            double& shape, double& rate, int& iterations) {
  const double high=curvature_upper(p);
  if (!(high>0) || !std::isfinite(high)) return false;
  const double mean=curvature_mean(p), zeros=p.x.front()==0 ? p.w.front() : 0;
  if (!(mean>0)) return false;
  double low=std::min(high/2,std::exp(-1.0));
  int i=0;
  for (; i<maxit; ++i, low/=2) {
    const double negative=low*(-std::log(low)+std::log1p(mean)+
      std::max(0.0,special::digamma(1+low)))+(zeros/p.n)*std::expm1(low/mean);
    if ((p.n-zeros)/p.n>negative+curvature_pad) break;
  }
  if (i==maxit || !(low>0)) return false;
  CurvatureWork work{maxit,certificate_budget(maxit)};
  ProfileEndpoint lo{std::log(low),{}}, hi{std::log(high),{}};
  const bool trace=trace_curvature(p,high,mean);
  if (trace) {
    // The trace proof needs only the analytic brackets at the cutoffs.
    const double l=std::log1p(mean), h=std::log(mean);
    const double pad=curvature_pad*(1+p.x.size()+std::abs(lo.log_shape)+std::abs(hi.log_shape)+
                                    std::abs(l)+std::abs(h));
    lo.rate={lo.log_shape-l-pad,lo.log_shape-h+pad};
    hi.rate={hi.log_shape-l-pad,hi.log_shape-h+pad};
  } else if (!curvature_rate(p,low,1e-8,mean,work,lo.rate) ||
             !curvature_rate(p,high,1e-8,mean,work,hi.rate) ||
             !curvature_cover(p,lo,hi,mean,work)) return false;

  // A loose coefficient tolerance should still give an accurate
  // maximized likelihood for the regular interior fit.
  const double target=std::log1p(std::min(tol,1e-10));
  for (int it=0; it<maxit; ++it) {
    const double a=(lo.log_shape+hi.log_shape)/2;
    const double u=(lo.rate.lo+hi.rate.hi)/2;
    const double pad=8*std::numeric_limits<double>::epsilon()*(1+std::abs(a)+std::abs(u));
    if ((hi.log_shape-lo.log_shape)/2+pad < target &&
        (hi.rate.hi-lo.rate.lo)/2+pad < target) {
      std::array<double,3> refined{{a,u,0}};
      const auto objective=[&](const std::array<accuracy_detail::Jet,3>& z) {
        return fit_likelihood(p,z,maxit);
      };
      const std::array<bool,3> free{{true,true,false}};
      const bool polished=accuracy_detail::polish(refined,free,tol,maxit,objective);
      // At this point the bracket midpoint meets the coefficient
      // tolerance. The optional refinement has its own accuracy check
      // and stays in the neighborhood of the unique certified root.
      const bool nearby=polished && std::abs(refined[0]-a)<target &&
        std::abs(refined[1]-u)<target;
      shape=std::exp(nearby ? refined[0] : a);
      rate=std::exp(nearby ? refined[1] : u);
      iterations=it;
      return shape>0 && rate>0 && std::isfinite(shape) && std::isfinite(rate);
    }
    // Off-centre probes allow contraction when a midpoint score
    // lies within its numerical uncertainty band.
    bool advanced=false;
    for (double fraction : {.5,.25,.75}) {
      const double t=lo.log_shape+fraction*(hi.log_shape-lo.log_shape);
      if (!(t>lo.log_shape && t<hi.log_shape)) return false;
      ProfileEndpoint mid{t,{}};
      // A rate bracket uses part of the final rate allowance. The
      // steep one-pass cases can use more of it to resolve a rate sign.
      const double width=std::max(trace ? target/4 : target/8,
                                  std::min(1e-8,(hi.log_shape-lo.log_shape)/32));
      const double shape_at_mid=std::exp(t);
      // A valid enclosure survives an inconclusive tightening. The
      // outer sign bounds and final widths decide whether it suffices.
      curvature_rate(p,shape_at_mid,width,mean,work,mid.rate);
      if (curvature_score(p,shape_at_mid,mid.rate,false,work)>0) {
        lo=mid; advanced=true; break;
      }
      if (curvature_score(p,shape_at_mid,mid.rate,true,work)<0) {
        hi=mid; advanced=true; break;
      }
    }
    if (!advanced) return false;
  }
  return false;
}
}
}
