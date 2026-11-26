#pragma once
#include "../../core/outcome.hpp"
#include "../../core/work_budget.hpp"
#include "../../core/discrete_stop.hpp"
#include "../../core/span.hpp"
#include "dpqr.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>
namespace cppunivariate {
enum class DnormStatus { Ok, InvalidData, InvalidControl, PointMass, TwoPointLimit, ScaleLimit, IterationFailure };
struct DnormResult { double mean, sd, logLik; int iterations; DnormStatus status;   StopDetail stop_detail{};
  OutcomeReason failure_reason=OutcomeReason::NoConvergence;
  bool certified=true;
};
namespace dnorm_detail {
inline constexpr double score_margin = 32 * std::numeric_limits<double>::epsilon();
struct Cells {
  std::vector<double> x, w;
  double n=0, centre=0, variance=0;
  bool init(span data) {
    if (data.size()==0) return false;
    std::vector<double> v(data.begin(),data.end());
    for (double z:v) if (!std::isfinite(z) || z!=std::floor(z) || std::abs(z)>4503599627370495.0) return false;
    std::sort(v.begin(),v.end()); n=static_cast<double>(v.size());
    for (std::size_t i=0;i<v.size();) {
      std::size_t j=i+1; while (j<v.size() && v[j]==v[i]) ++j;
      x.push_back(v[i]); w.push_back(static_cast<double>(j-i)); i=j;
    }
    // Centring avoids summation overflow and loss of location precision.
    const double offset=x.front();
    for (std::size_t j=0;j<x.size();++j) centre+=w[j]*(x[j]-offset)/n;
    centre+=offset+0.5;
    for (std::size_t j=0;j<x.size();++j) variance+=w[j]*(x[j]+0.5-centre)*(x[j]+0.5-centre)/n;
    return true;
  }
};
// Negative log likelihood and analytic score/information in A=(mu-c)/sd,
// B=1/sd. With mean held A=(mean-c)*B; with sd held B is constant.
struct EvaluationError {
  std::vector<double> g, H;
};

inline double evaluate(const Cells &p,double A,double B,bool hm,bool hs,double held,
                       std::vector<double> &g,std::vector<double> &H,
                       EvaluationError& error, bool bound=false, double mean=0) {
  const int d=hm || hs ? 1 : 2;
  g.assign(d,0); H.assign(d*d,0); double f=0;
  if (bound) { error.g.assign(d,0); error.H.assign(d*d,0); }
  for (std::size_t j=0;j<p.x.size();++j) {
    const double l=p.x[j]-p.centre, r=l+1;
    const double left=hm && bound ? p.x[j]-mean : l-held;
    const double right=hm && bound ? p.x[j]+1-mean : r-held;
    const double a=hm && bound ? left*B : l*B-A;
    const double b=hm && bound ? right*B : r*B-A;
    const double m=l+0.5,z=hm && bound ? (p.x[j]+0.5-mean)*B : m*B-A;
    if (B*(1+std::abs(z))<1e-5) {
      // Known width B avoids cancellation in b-a. Differentiate the
      // narrow-interval expansion directly instead of subtracting huge
      // endpoint density/probability ratios.
      const double c=((z*B)*(z*B)-B*B)/24,den=1+c;
      const double ca=-z*B*B/12,cb=(z*m*B*B+(z*z-1)*B)/12;
      const double ga=z+ca/den,gb=1/B-z*m+cb/den;
      const double haa=-1+B*B/(12*den)-ca*ca/(den*den);
      const double hab=m-(m*B*B+2*z*B)/(12*den)-ca*cb/(den*den);
      const double hbb=-1/(B*B)-m*m+(m*m*B*B+4*z*m*B+z*z-1)/(12*den)-cb*cb/(den*den);
      f-=p.w[j]*(std::log(B)-0.5*z*z-0.91893853320467274178+std::log1p(c));
      if (hm && bound) {
        const double mid=p.x[j]+0.5-mean;
        const double correction=(z*mid*B*B+(z*z-1)*B)/(12*den);
        g[0]-=p.w[j]*(1/B-z*mid+correction);
        error.g[0]+=score_margin*p.w[j]*(1/B+std::abs(z*mid)+std::abs(correction));
        continue;
      }
      if (bound) {
        error.g[0]+=score_margin*p.w[j]*(1+std::abs(z)+std::abs(A)+std::abs(m*B));
        if (!hs) {
          error.g[1]+=score_margin*p.w[j]*(1/B+std::abs(z*m)+std::abs(m)*(std::abs(A)+std::abs(m*B)));
          error.H[0]+=score_margin*p.w[j];
          error.H[1]+=score_margin*p.w[j]*(1+std::abs(m));
          error.H[2]+=score_margin*p.w[j]*(1+std::abs(m));
          error.H[3]+=score_margin*p.w[j]*(1/(B*B)+m*m);
        }
      }
      if (hm) {
        g[0]-=p.w[j]*(held*ga+gb);
        H[0]-=p.w[j]*(held*held*haa+2*held*hab+hbb);
      } else if (hs) { g[0]-=p.w[j]*ga; H[0]-=p.w[j]*haa; }
      else {
        g[0]-=p.w[j]*ga; g[1]-=p.w[j]*gb;
        H[0]-=p.w[j]*haa; H[1]-=p.w[j]*hab;
        H[2]-=p.w[j]*hab; H[3]-=p.w[j]*hbb;
      }
      continue;
    }
    const double lp=distributions::dnorm_interval(a,b);
    if (!std::isfinite(lp)) return std::numeric_limits<double>::infinity();
    f-=p.w[j]*lp;
    const double fa=std::exp(-0.5*a*a-0.91893853320467274178-lp);
    const double fb=std::exp(-0.5*b*b-0.91893853320467274178-lp);
    if (hm && bound) {
      // The endpoint ratio involves the known width B. Factor out the
      // nearer endpoint before subtraction, with right-left exactly one.
      const double exponent=B*(a/2+b/2);
      const double near=exponent>=0 ? fa : fb;
      const double distance=exponent>=0 ? left : -right;
      const double t=-std::abs(exponent);
      const double first=std::exp(t), second=distance*std::expm1(t);
      const double derivative=first+second;
      g[0]-=p.w[j]*near*derivative;
      error.g[0]+=p.w[j]*near*score_margin*
          (first+std::abs(second)+std::abs(t)*first*(1+std::abs(distance))+
           std::abs(derivative)*(1+std::abs(lp)+std::min(a*a,b*b)+1/B));
      continue;
    }
    const std::vector<double> va=hm ? std::vector<double>{left} : hs ? std::vector<double>{-1} : std::vector<double>{-1,l};
    const std::vector<double> vb=hm ? std::vector<double>{right} : hs ? std::vector<double>{-1} : std::vector<double>{-1,r};
    std::vector<double> s(d);
    const double padding = bound ? score_margin *
        (1 + std::abs(lp) + a*a + b*b + 1/B +
         (hm ? 0 : (std::abs(A)+std::abs(l*B)+std::abs(r*B))/B)) : 0;
    for (int k=0;k<d;++k) {
      s[k]=fb*vb[k]-fa*va[k];
      g[k]-=p.w[j]*s[k];
      if (bound) error.g[k]+=p.w[j]*padding*(std::abs(fb*vb[k])+std::abs(fa*va[k]));
    }
    for (int k=0;k<d;++k) for (int m=0;m<d;++m) {
      const double lo=a*fa*va[k]*va[m], hi=b*fb*vb[k]*vb[m];
      H[k*d+m]+=p.w[j]*(s[k]*s[m]+hi-lo);
      if (bound) {
        const double vk=std::abs(fa*va[k])+std::abs(fb*vb[k]);
        const double vm=std::abs(fa*va[m])+std::abs(fb*vb[m]);
        error.H[k*d+m]+=p.w[j]*padding*(2*vk*vm+std::abs(lo)+std::abs(hi));
      }
    }
  }
  return f;
}
}
}
#include "certificate.hpp"
#include "held.hpp"
namespace cppunivariate {
inline DnormResult dnorm_mle(span x,double tol=1e-10,int maxit=200,
    double mean=std::numeric_limits<double>::quiet_NaN(),double sd=std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  auto stopped = [&]() -> DnormResult {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  DnormResult out{nan,nan,nan,0,DnormStatus::InvalidData};
  const bool hm=!std::isnan(mean), hs=!std::isnan(sd);
  if (!(tol>0) || !std::isfinite(tol) || maxit<1 || (hm && !std::isfinite(mean)) || (hs && (!(sd>0) || !std::isfinite(sd)))) { out.status=DnormStatus::InvalidControl; return out; }
  dnorm_detail::Cells p; if (!p.init(x)) return out;
  if (!hm && !hs && p.x.back()-p.x.front()<2) {
    out.status=p.x.size()==1 ? DnormStatus::PointMass : DnormStatus::TwoPointLimit; return out;
  }
  if (hm && !hs && p.x.back()<=mean && p.x.front()+1>=mean) { out.status=DnormStatus::ScaleLimit; return out; }
  if (hm && hs) return dnorm_detail::at_pair(p,mean,sd,0);
  if (p.x.size()==1) {
    if (hs) return dnorm_detail::at_pair(p,p.x.front()+0.5,sd,0);
    const double delta=std::max(p.x.front()-mean,mean-(p.x.front()+1));
    const double scale=dnorm_detail::constant_scale(delta);
    if (tol>=256*std::numeric_limits<double>::epsilon())
      return dnorm_detail::at_pair(p,mean,scale,0);
  }
  if (hm) {
    const double span=p.x.back()-p.x.front();
    const double delta=std::abs(p.x.back()-mean);
    if (span==1 && delta<=1e-5)
      return dnorm_detail::adjacent_scale(p,mean,delta,tol,maxit);
    // With all endpoints on one side, each cell score changes sign
    // between its two endpoint distances from the mean.
    const double near=std::max(p.x.front()-mean,mean-(p.x.back()+1));
    if (near>0 && (span+1)/near+dnorm_detail::score_margin<=tol)
      return dnorm_detail::at_pair(p,mean,near,0);
  }
  if (hs && sd<=0.025) {
    if (dnorm_detail::symmetric(p))
      return dnorm_detail::at_pair(p,(p.x.front()+p.x.back()+1)/2,sd,0);
    if (p.x.back()-p.x.front()==1) return dnorm_detail::adjacent_location(p,sd,tol,maxit);
    if (sd<1e-150) return dnorm_detail::projected_location(p,sd,tol,maxit);
  }
  if (hs && (p.x.back()-p.x.front())/sd+dnorm_detail::score_margin<=tol)
    return dnorm_detail::at_pair(p,p.centre,sd,0);
  if (!hm) mean=p.centre;
  if (!hs) sd=std::hypot(std::sqrt(std::max(0.1,p.variance)),hm ? mean-p.centre:0.0);
  double B=1/sd,A=(mean-p.centre)*B;
  const double held=mean-p.centre;
  std::vector<double> g,H;
  dnorm_detail::EvaluationError error;
  double f=dnorm_detail::evaluate(p,A,B,hm,hs,held,g,H,error);
  DnormResult provisional{nan,nan,nan,0,DnormStatus::IterationFailure};
  for (int it=0;it<maxit;++it) {
    out.iterations=it+1;
    if (!std::isfinite(f)) break;
    const int d=static_cast<int>(g.size()); std::vector<double> step(d);
    if (d==1) step[0]=-g[0]/H[0];
    else {
      const double det=H[0]*H[3]-H[1]*H[2];
      if (!(det>0)) break;
      step[0]=(-H[3]*g[0]+H[1]*g[1])/det;
      step[1]=(H[2]*g[0]-H[0]*g[1])/det;
    }
    double slope=0; for (int k=0;k<d;++k) slope+=g[k]*step[k];
    if (!std::isfinite(slope) || slope>0) break;
    // Keep the last full Newton step on the ordinary path. Its decrement
    // schedules the accuracy check, which uses bounds in coefficient units.
    const bool check = -slope <= tol*tol*p.n;
    bool accepted=false;
    double t=1;
    for (std::int64_t backtrack=0;backtrack<solver_budget(maxit) && t>1e-12;++backtrack,t*=0.5) {
      double nA=A,nB=B;
      if (hm) { nB+=t*step[0]; nA=held*nB; }
      else { nA+=t*step[0]; if (!hs) nB+=t*step[1]; }
      if (!(nB>0) || !std::isfinite(nB) || !std::isfinite(nA)) continue;
      std::vector<double> ng,nH;
      double nf=dnorm_detail::evaluate(p,nA,nB,hm,hs,held,ng,nH,error);
      if (std::isfinite(nf) && nf<=f+1e-4*t*slope+1e-14*p.n) {
        A=nA; B=nB; f=nf; g=ng; H=nH; accepted=true; break;
      }
      // A rounded objective may reject the last full step even when
      // the current candidate already has an accurate score enclosure.
      if (check && dnorm_detail::score_enclosure(p,A,B,hm,hs,mean,sd,tol)) break;
    }
    if (check) {
      if (dnorm_detail::score_enclosure(p,A,B,hm,hs,mean,sd,tol)) {
        out.status=DnormStatus::Ok;
        break;
      }
      // Retain the search's candidate if the coefficient enclosure is
      // numerically supported but the full floating-point margin fails.
      if (provisional.status!=DnormStatus::Ok && tol>=16*dnorm_detail::score_margin &&
          dnorm_detail::score_enclosure(p,A,B,hm,hs,mean,sd,tol,0.125)) {
        provisional=dnorm_detail::at_pair(p,hm ? mean : p.centre+A/B,hs ? sd : 1/B,it+1);
        provisional.certified=false;
      }
    }
    if (!accepted) break;
  }
  if (out.status!=DnormStatus::Ok) {
    out.status = dnorm_detail::score_enclosure(p,A,B,hm,hs,mean,sd,tol)
        ? DnormStatus::Ok : DnormStatus::IterationFailure;
  }
  out.mean=hm ? mean : p.centre+A/B; out.sd=hs ? sd : 1/B;
  if (out.status==DnormStatus::Ok)
    return dnorm_detail::at_pair(p,out.mean,out.sd,out.iterations);
  if (provisional.status==DnormStatus::Ok) return provisional;
  return out;

  }();
  if(stopped.status==DnormStatus::PointMass) stopped.stop_detail=discrete_point(x[0]);
  if(stopped.status==DnormStatus::TwoPointLimit) {
    const double lo=*std::min_element(x.begin(),x.end());
    stopped.stop_detail=discrete_two_point(x,lo,lo+1);
  }
  if(stopped.status==DnormStatus::ScaleLimit) {
    const double lo=std::floor(mean);
    stopped.stop_detail=mean==lo?discrete_two_point(x,lo-1,lo,0.5):discrete_point(lo);
  }
  return stopped;
}
}

namespace cppunivariate {
inline FitOutcome outcome(const DnormResult& r) {
  switch (r.status) {
    case DnormStatus::Ok: {
      auto fit=FitOutcome::singleton(true);
      fit.certified=r.certified;
      return fit;
    }
    case DnormStatus::InvalidData: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case DnormStatus::InvalidControl: return FitOutcome::failure(OutcomeReason::InvalidArgument);
    case DnormStatus::PointMass: return FitOutcome::limit();
    case DnormStatus::TwoPointLimit: return FitOutcome::limit();
    case DnormStatus::ScaleLimit: return FitOutcome::limit();
    case DnormStatus::IterationFailure: return FitOutcome::failure(r.failure_reason);
  }
  return {};
}
} // namespace cppunivariate
